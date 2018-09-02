package com.scalaz

import com.scalaz.config.syntax.ConfigSyntaxSyntax

package object config extends ConfigSyntaxSyntax {
  type PropertyKey   = String
  type PropertyValue = String

  type ~[A, B] = (A, B)

  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = Some(t)
  }

  final def read[F[_], A: Property](key: PropertyKey)(implicit F: ConfigSyntax[F, A]): F[A] =
    ConfigSyntax[F, A].read(key)
}
