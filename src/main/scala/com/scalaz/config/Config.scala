package com.scalaz.config

import scalaz.syntax.either._
import scalaz.syntax.validation._
import scalaz.{ Failure, Kleisli, NonEmptyList, Success, ValidationNel, \/ }

trait Config[F[_], A] {
  def apply(implicit F: ConfigSyntax[F]): F[A]
}

object Config {
  type Env          = Map[String, String]
  type MapReader[A] = Kleisli[ValidationNel[ConfigError, ?], Env, A]

  object MapReader {
    implicit def configSyntaxReader: ConfigSyntax[Config.MapReader] =
      new ConfigSyntax[Config.MapReader] {
        override def read[A](key: String)(implicit P: Property[A]): MapReader[A] =
          Kleisli[ValidationNel[ConfigError, ?], Env, A](
            _.get(key)
              .fold(ConfigError(key, ConfigError.MissingValue).failureNel[A])(
                P.read(_).leftMap(t => ConfigError(key, t)).validationNel
              )
          )

        override def product[A, B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)] =
          l.mapK[NonEmptyList[ConfigError] \/ ?, A](_.disjunction)
            .flatMap(
              ll => r.mapK[NonEmptyList[ConfigError] \/ ?, B](_.disjunction).map(rr => (ll, rr))
            )
            .mapK[ValidationNel[ConfigError, ?], (A, B)](_.validation)

        override def coproduct[A, B](l: => MapReader[A], r: => MapReader[B]): MapReader[A \/ B] =
          Kleisli[ValidationNel[ConfigError, ?], Env, A \/ B](
            env =>
              l.run(env) match {
                case Failure(e) => r.mapK(_.bimap(e.append, _.right[A])).run(env)
                case Success(a) => a.left[B].successNel[ConfigError]
              }
          )

        override def point[A](a: => A): MapReader[A] =
          Kleisli[ValidationNel[ConfigError, ?], Env, A](_ => a.successNel)

        override def ap[A, B](fa: => MapReader[A])(f: => MapReader[A => B]): MapReader[B] =
          Kleisli[ValidationNel[ConfigError, ?], Env, B] { env =>
            fa.mapK[ValidationNel[ConfigError, ?], B](
                v1 => {
                  f.mapK[ValidationNel[ConfigError, ?], B](v2 => v1.ap(v2)).run(env)
                }
              )
              .run(env)
          }

        override def bind[A, B](fa: MapReader[A])(f: A => MapReader[B]): MapReader[B] =
          fa.mapK[NonEmptyList[ConfigError] \/ ?, A](_.disjunction)
            .flatMap(a => f(a).mapK[NonEmptyList[ConfigError] \/ ?, B](_.disjunction))
            .mapK[ValidationNel[ConfigError, ?], B](_.validation)
      }
  }

  import MapReader._

  def reader[A](config: Config[MapReader, A]): MapReader[A] =
    config.apply
}
