package com.scalaz.config

import com.scalaz.config.Config.{ MapReader }
import scalaz.syntax.either._
import scalaz.syntax.nel._
import scalaz.{ -\/, Kleisli, NonEmptyList, \/, \/- }

trait Syntax[F[_]] {
  def point[A](a: => A): F[A]
  def map[A, B](f: F[A])(fa: Equiv[A, B]): F[B]
  def product[A, B](l: => F[A], r: => F[B]): F[(A, B)]
  def coproduct[A, B](l: => F[A], r: => F[B]): F[A \/ B]
}

trait ConfigSyntax[F[_]] extends Syntax[F] {
  def read[A](key: PropertyKey)(implicit P: Property[A]): F[A]
  def document[A](fa: F[A], docs: String): F[A] = ???
}

object ConfigSyntax {
  def apply[F[_]](implicit F: ConfigSyntax[F]): ConfigSyntax[F] = F

  implicit def configSyntaxReader: ConfigSyntax[Config.MapReader] =
    new ConfigSyntax[Config.MapReader] {
      override def read[A](key: PropertyKey)(implicit P: Property[A]): MapReader[A] =
        Kleisli(
          _.get(key)
            .fold(ConfigError(key, ConfigError.MissingValue).wrapNel.left[A])(
              P.read(_).leftMap(t => ConfigError(key, t).wrapNel)
            )
        )

      override def point[A](a: => A): MapReader[A] =
        Kleisli(_ => a.right)

      override def map[A, B](f: MapReader[A])(fa: Equiv[A, B]): MapReader[B] =
        f.map(fa.to)

      override def product[A, B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)] =
        l.flatMap(ll => r.map(rr => (ll, rr)))

      override def coproduct[A, B](l: => MapReader[A], r: => MapReader[B]): MapReader[A \/ B] =
        Kleisli(
          env =>
            l.run(env) match {
              case -\/(e) => r.mapK(_.bimap(e.append, _.right[A])).run(env)
              case \/-(a) => a.left[B].right[NonEmptyList[ConfigError]]
            }
        )
    }

  implicit def configSyntaxWriter[A]: ConfigSyntax[Config.MapWriter] = ???
}
