package com.scalaz.config

import com.scalaz.config.Config.{ Env, MapReader }
import scalaz.syntax.either._
import scalaz.syntax.nel._
import scalaz.{ -\/, Kleisli, NonEmptyList, \/, \/- }

trait Syntax[F[_], A] {
  def point(a: => A): F[A]
  def map[B](f: F[A])(fa: A => B): F[B]
  def product[B](l: => F[A], r: => F[B]): F[(A, B)]
  def coproduct[B](l: => F[A], r: => F[B]): F[A \/ B]
}

trait ConfigSyntax[F[_], A] extends Syntax[F, A] {
  def read(key: PropertyKey)(implicit P: Property[A]): F[A]
  def document(fa: F[A], docs: String): F[A] = ???
}

object ConfigSyntax {
  def apply[F[_], A](implicit F: ConfigSyntax[F, A]): ConfigSyntax[F, A] = F

  implicit def configSyntax[A]: ConfigSyntax[Config.MapReader, A] =
    new ConfigSyntax[Config.MapReader, A] {
      override def read(key: PropertyKey)(implicit P: Property[A]): MapReader[A] =
        Kleisli[NonEmptyList[ConfigError] \/ ?, Env, A](
          env =>
            env
              .get(key)
              .fold(ConfigError(key, ConfigError.MissingValue).wrapNel.left[A])(
                P.read(_).leftMap(t => ConfigError(key, t).wrapNel)
              )
        )

      override def point(a: => A): MapReader[A] =
        Kleisli[NonEmptyList[ConfigError] \/ ?, Env, A](_ => a.right)

      override def map[B](f: MapReader[A])(fa: A => B): MapReader[B] =
        f.map(fa)

      override def product[B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)] =
        l.flatMap(ll => r.map(rr => (ll, rr)))

      // read a value of type A and if that fails, read a value of type B. Hence the out can be A or B
      override def coproduct[B](l: => MapReader[A], r: => MapReader[B]): MapReader[A \/ B] =
        Kleisli[NonEmptyList[ConfigError] \/ ?, Env, A \/ B](
          env =>
            l.run(env) match {
              case -\/(e) => r.mapK(_.bimap(e.append, _.right[A])).run(env)
              case \/-(a) => a.left[B].right[NonEmptyList[ConfigError]]
            }
        )
    }
}
