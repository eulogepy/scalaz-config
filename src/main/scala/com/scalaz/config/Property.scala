package com.scalaz.config

import com.scalaz.config.ConfigError.{ErrorType, ParseError}
import scalaz.\/
import scalaz.syntax.either._

trait Property[A] {
  def show(a: A): PropertyValue
  def read(p: PropertyValue): ErrorType \/ A
}

object Property {
  def apply[A](implicit ev: Property[A]): Property[A] = ev

  def instance[A](
    f: A => PropertyValue,
    g: PropertyValue => ErrorType \/ A,
  ): Property[A] =
    new Property[A] {
      override def show(a: A): PropertyValue              = f(a)
      override def read(p: PropertyValue): ErrorType \/ A = g(p)
    }

  implicit val intProperty: Property[Int] =
    instance(_.toString, a => \/.fromTryCatchNonFatal(a.toInt).leftMap(t => ParseError(a, "int")))

  implicit val longProperty: Property[Long] =
    instance(_.toString, a => \/.fromTryCatchNonFatal(a.toLong).leftMap(t => ParseError(a, "long")))

  implicit val doubleProperty: Property[Double] =
    instance(_.toString, a => \/.fromTryCatchNonFatal(a.toDouble).leftMap(t => ParseError(a, "double")))

  implicit val stringProperty: Property[String] =
    instance(_.toString, _.right)
}
