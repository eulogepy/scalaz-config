package com.scalaz.config

package syntax

import scalaz.\/

trait ConfigSyntaxSyntax {
  implicit class ConfigSyntaxSyntaxOps[F[_], A](self: F[A]) {
    final def map[B](f: Equiv[A, B])(implicit F: ConfigSyntax[F]): F[B] =
      F.map(self)(f)

    final def ~ [B](that: F[B])(implicit F: ConfigSyntax[F]): F[(A, B)] =
      F.product(self, that)

    final def or[B](that: F[B])(implicit F: ConfigSyntax[F]): F[A \/ B] =
      F.coproduct(self, that)

    final def `<?>` (docs: String)(implicit F: ConfigSyntax[F]): F[A] =
      F.document(self, docs)
  }
}
