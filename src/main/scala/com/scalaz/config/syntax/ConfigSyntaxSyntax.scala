package com.scalaz.config

package syntax

trait ConfigSyntaxSyntax {
  implicit class ConfigSyntaxSyntaxOps[F[_], A](self: F[A]) {
    final def ~ [B](f: F[B])(implicit F: ConfigSyntax[F]): ProductBuilder[F, A, B] =
      new ProductBuilder[F, A, B] {
        override val a: F[A]            = self
        override val b: F[B]            = f
        implicit val C: ConfigSyntax[F] = F
      }

    final def >> [B](f: A => F[B])(implicit F: ConfigSyntax[F]): F[(A, B)] =
      F.bind(self)(a => F.product(self, f(a)))

    final def | [B](that: F[B])(implicit F: ConfigSyntax[F]): F[Either[A, B]] =
      F.coproduct(self, that)
  }
}
