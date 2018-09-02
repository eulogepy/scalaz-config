package com.scalaz.config

import scalaz.\/

trait Syntax[F[_], A] {
  def point(a: => A): F[A]

  def map[B](f: F[A])(fa: A => B): F[B]

  def product[B](l: => F[A], r: => F[B]): F[(A, B)]

  def coproduct[B](l: => F[A], r: => F[B]): F[A \/ B]
}

trait ConfigSyntax[F[_], A] extends Syntax[F, A] {
  def read(key: PropertyKey)(implicit P: Property[A]): F[A]

  def document(fa: F[A], docs: String): F[A] = {
    println(docs)
    fa
  }
}

object ConfigSyntax {
  def apply[F[_], A](implicit F: ConfigSyntax[F, A]): ConfigSyntax[F, A] = F
}