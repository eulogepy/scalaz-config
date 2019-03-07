package com.scalaz.config

trait Syntax[F[_]] {
  def product[A, B](l: => F[A], r: => F[B]): F[(A, B)]
  def coproduct[A, B](l: => F[A], r: => F[B]): F[Either[A, B]]
  def point[A](a: => A): F[A]
  def map[A, B](l: => F[A])(f: A => B): F[B]
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait ConfigSyntax[F[_]] extends Syntax[F] {
  def read[A](key: String)(implicit P: Property[A]): F[A]
}

object ConfigSyntax {
  def apply[F[_]](implicit F: ConfigSyntax[F]): ConfigSyntax[F] = F
}
