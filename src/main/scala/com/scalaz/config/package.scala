package com.scalaz

package object config {
  type PropertyKey = String
  type PropertyValue = String

  implicit class ConfigSyntaxSyntax[F[_], A](self: F[A]) {
    final def map[B](f: A => B)(implicit F: Syntax[F, A]): F[B] = F.map(self)(f)

    final def ~ [B] (that: F[B])(implicit F: Syntax[F, A]): F[(A, B)] =
      F.product(self, that)

    final def `<?>` (docs: String)(implicit F: ConfigSyntax[F, A]): F[A] =
      F.document(self, docs)
  }

  type ~ [A, B] = (A, B)

  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = Some(t)
  }

  final def read[F[_], A: Property](key: PropertyKey)(implicit F: ConfigSyntax[F, A]): F[A] =
    ConfigSyntax[F, A].read(key)
}