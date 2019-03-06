package com.scalaz.config

import com.scalaz.config.syntax.EitherSyntax
import scalaz.std.either._
import scalaz.{ Kleisli, NonEmptyList }

trait Config[F[_], A] {
  def apply(implicit F: ConfigSyntax[F]): F[A]
}

object Config extends EitherSyntax {
  type Env          = Map[String, String]
  type MapReader[A] = Kleisli[Either[NonEmptyList[ConfigError], ?], Env, A]

  object MapReader {
    implicit def configSyntaxReader: ConfigSyntax[Config.MapReader] =
      new ConfigSyntax[Config.MapReader] {
        override def read[A](key: String)(implicit P: Property[A]): MapReader[A] =
          Kleisli[Either[NonEmptyList[ConfigError], ?], Env, A](
            _.get(key).fold(NonEmptyList(ConfigError(key, ConfigError.MissingValue)).asLeft[A])(
              P.read(_).leftMap(t => NonEmptyList(ConfigError(key, t)))
            )
          )

        override def product[A, B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)] =
          l.flatMap(ll => r.map(rr => (ll, rr)))

        override def coproduct[A, B](
          l: => MapReader[A],
          r: => MapReader[B]
        ): MapReader[Either[A, B]] =
          Kleisli[Either[NonEmptyList[ConfigError], ?], Env, Either[A, B]](
            env =>
              l.run(env) match {
                case Left(e)  => r.mapK(_.bimap(e.append, _.asRight[A])).run(env)
                case Right(a) => a.asLeft[B].asRight[NonEmptyList[ConfigError]]
              }
          )

        override def point[A](a: => A): MapReader[A] =
          Kleisli[Either[NonEmptyList[ConfigError], ?], Env, A](
            _ => a.asRight[NonEmptyList[ConfigError]]
          )

        override def ap[A, B](fa: => MapReader[A])(f: => MapReader[A => B]): MapReader[B] =
          Kleisli[Either[NonEmptyList[ConfigError], ?], Env, B] { env =>
            fa.mapK[Either[NonEmptyList[ConfigError], ?], B](
                v1 => {
                  f.mapK[Either[NonEmptyList[ConfigError], ?], B](
                      v2 =>
                        (v1, v2) match {
                          case (Right(a), Right(x)) => Right(x(a))
                          case (Right(_), Left(x))  => Left(x)
                          case (Left(x), Right(_))  => Left(x)
                          case (Left(x1), Left(x2)) => Left(x2.append(x1))
                        }
                    )
                    .run(env)
                }
              )
              .run(env)
          }

        override def bind[A, B](fa: MapReader[A])(f: A => MapReader[B]): MapReader[B] =
          fa.flatMap(f)
      }
  }

  import MapReader._

  def reader[A](config: Config[MapReader, A]): MapReader[A] =
    config.apply
}
