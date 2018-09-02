package com.scalaz.config.syntax

import com.scalaz.config.{ ConfigSyntax, Syntax }

trait ConfigSyntaxSyntax {
  implicit class ConfigSyntaxSyntaxOps[F[_], A](self: F[A]) {
    final def map[B](f: A => B)(implicit F: Syntax[F, A]): F[B] = F.map(self)(f)

    final def ~ [B](that: F[B])(implicit F: Syntax[F, A]): F[(A, B)] =
      F.product(self, that)

    final def `<?>` (docs: String)(implicit F: ConfigSyntax[F, A]): F[A] =
      F.document(self, docs)
  }
}
