package com.scalaz.config

import com.scalaz.config.Config.MapReader
import scalaz.std.function._
import scalaz.syntax.arrow._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.nel._
import scalaz.{ -\/, \/, \/- }

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
        _.get(key)
          .fold(
            ConfigError(key, ConfigError.MissingValue).wrapNel.left[A]
          )(P.read(_).leftMap(t => ConfigError(key, t).wrapNel))

      override def point(a: => A): MapReader[A] =
        _ => a.right

      override def map[B](f: MapReader[A])(fa: A => B): MapReader[B] =
        f.>>>(_.map(fa))

      override def product[B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)] =
        l.flatMap(a => r.>>>(b => a.flatMap(aa => b.map(bb => (aa, bb)))))

      // read a value of type A and if that fails, read a value of type B. Hence the out can be A or B
      override def coproduct[B](l: => MapReader[A], r: => MapReader[B]): MapReader[A \/ B] =
        l.flatMap({
          case -\/(_) => r.>>>(_.map(_.right[A]))
          case \/-(_) => l.>>>(_.map(_.left[B]))
        })
    }
}
