package com.scalaz.config.syntax

trait EitherSyntax {
  implicit class EitherSyntaxOps[A, B](self: Either[A, B]) {

    def leftMap[C](f: A => C): Either[C, B] = self match {
      case Left(a)  => Left(f(a))
      case Right(a) => Right(a)
    }

    def bimap[C, D](f: A => C, g: B => D): Either[C, D] = self match {
      case Right(b) => Right(g(b))
      case Left(a)  => Left(f(a))
    }
  }

  implicit class ToEitherOps[A](self: => A) {
    def asRight[E]: Either[E, A] = Right(self)
    def asLeft[B]: Either[A, B]  = Left(self)
  }
}
