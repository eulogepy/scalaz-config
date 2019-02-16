package com.scalaz.config

import scalaz.{Monad, \/}

trait Syntax[F[_]] {
  def product[A, B](l: => F[A], r: => F[B]): F[(A, B)]
  def coproduct[A, B](l: => F[A], r: => F[B]): F[A \/ B]
}

trait ConfigSyntax[F[_]] extends Syntax[F] with Monad[F] {
  def read[A](key: String)(implicit P: Property[A]): F[A]
}

object ConfigSyntax {
  def apply[F[_]](implicit F: ConfigSyntax[F]): ConfigSyntax[F] = F
}
