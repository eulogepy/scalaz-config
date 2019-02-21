package com.scalaz.config

package syntax

import scalaz.\/
import scalaz.syntax.monad._

trait ConfigSyntaxSyntax {
  implicit class ConfigSyntaxSyntaxOps[F[_], A](self: F[A]) {
    final def ~ [B](f: A => F[B])(implicit F: ConfigSyntax[F]): F[(A, B)] =
      self.flatMap(a => F.product(self, f(a)))

    final def or[B](that: F[B])(implicit F: ConfigSyntax[F]): F[A \/ B] =
      F.coproduct(self, that)
  }
}
