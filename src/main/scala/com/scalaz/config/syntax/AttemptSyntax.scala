package com.scalaz.config

package syntax

import scala.util.Try

trait AttemptSyntax extends EitherSyntax {
  implicit class ConfigSyntaxSyntaxOps[A](self: => A) {

    def attempt[E](f: Throwable => E): Either[E, A] =
      Try { self }.toEither.leftMap(f)
  }
}
