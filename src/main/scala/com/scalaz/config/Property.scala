package com.scalaz.config

import com.scalaz.config.ConfigError.{ ErrorType, ParseError }
import com.scalaz.config.syntax.AttemptSyntax

trait Property[A] {
  def show(a: A): String
  def read(p: String): Either[ErrorType, A]
}

object Property extends AttemptSyntax {
  def apply[A](implicit ev: Property[A]): Property[A] = ev

  def instance[A](f: A => String, g: String => Either[ErrorType, A]): Property[A] =
    new Property[A] {
      override def show(a: A): String                    = f(a)
      override def read(p: String): Either[ErrorType, A] = g(p)
    }

  implicit val intProperty: Property[Int] =
    instance(_.toString, a => a.toInt.attempt(_ => ParseError(a, "int")))

  implicit val longProperty: Property[Long] =
    instance(_.toString, a => a.toLong.attempt(_ => ParseError(a, "long")))

  implicit val doubleProperty: Property[Double] =
    instance(
      _.toString,
      a => a.toDouble.attempt(_ => ParseError(a, "double"))
    )

  implicit val stringProperty: Property[String] =
    instance(_.toString, Right(_))
}
