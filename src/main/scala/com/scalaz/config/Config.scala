package com.scalaz.config

import com.scalaz.config.syntax.EitherSyntax
import scalaz.zio.{ IO, ZIO }

trait Config[F[_], A] {
  def apply(implicit F: ConfigSyntax[F]): F[A]
}

object Config extends EitherSyntax {
  type Env          = Map[String, String]
  type MapReader[A] = ZIO[Env, List[ConfigError], A]

  object MapReader {
    implicit def configSyntaxReader: ConfigSyntax[Config.MapReader] =
      new ConfigSyntax[Config.MapReader] {
        override def read[A](key: String)(implicit P: Property[A]): MapReader[A] =
          ZIO.accessM(
            r =>
              ZIO.fromEither(
                r.get(key)
                  .fold(List(ConfigError(key, ConfigError.MissingValue)).asLeft[A])(
                    P.read(_).leftMap(t => List(ConfigError(key, t)))
                  )
              )
          )

        override def product[A, B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)] =
          l.either
            .flatMap(
              res1 =>
                r.either.map(
                  res2 =>
                    (res1, res2) match {
                      case (Right(a), Right(b))     => Right((a, b))
                      case (Left(a), Right(_))      => Left(a)
                      case (Right(_), Left(error))  => Left(error)
                      case (Left(err1), Left(err2)) => Left(err1 ++ err2)
                    }
                )
            )
            .absolve

        override def coproduct[A, B](
          l: => MapReader[A],
          r: => MapReader[B]
        ): MapReader[Either[A, B]] =
          l.either.flatMap({
            case Right(a) => ZIO.access(_ => a.asLeft[B])
            case Left(lerr) =>
              r.either.flatMap({
                case Right(b)   => ZIO.access(_ => b.asRight[A])
                case Left(rerr) => ZIO.fail(lerr ++ rerr)
              })
          })

        override def point[A](a: => A): MapReader[A] =
          ZIO.access(_ => a)

        override def bind[A, B](fa: MapReader[A])(f: A => MapReader[B]): MapReader[B] =
          fa.flatMap(f)

        override def map[A, B](l: => MapReader[A])(f: A => B): MapReader[B] =
          l.map(f)
      }
  }

  import MapReader._

  def fromEnv[A](f: => Config.Env, config: Config[MapReader, A]): IO[List[ConfigError], A] =
    ZIO.apply(f).orDie.flatMap(e => config.apply.provide(e))
}
