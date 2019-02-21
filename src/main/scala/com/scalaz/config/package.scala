package com.scalaz

import com.scalaz.config.syntax.ConfigSyntaxSyntax

package object config extends ConfigSyntaxSyntax {
  type ~[A, B] = (A, B)

  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = Some(t)
  }

  final def read[F[_], A: Property](key: String)(implicit F: ConfigSyntax[F]): F[A] =
    ConfigSyntax[F].read(key)
}
