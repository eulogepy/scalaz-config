package com.scalaz.config

import com.scalaz.config.ConfigError.{ErrorType, ParseError}
import scalaz.{@@, InvariantFunctor, Tag, \/}
import scalaz.syntax.either._
import scalaz.syntax.invariantFunctor._

trait Property[A] {
  def show(a: A): String
  def read(p: String): ErrorType \/ A
}

object Property extends  PropertyInstances {
  def apply[A](implicit ev: Property[A]): Property[A] = ev

  def instance[A](
    f: A => String,
    g: String => ErrorType \/ A,
  ): Property[A] =
    new Property[A] {
      override def show(a: A): String              = f(a)
      override def read(p: String): ErrorType \/ A = g(p)
    }

  implicit val intProperty: Property[Int] =
    instance(_.toString, a => \/.fromTryCatchNonFatal(a.toInt).leftMap(_ => ParseError(a, "int")))

  implicit val longProperty: Property[Long] =
    instance(_.toString, a => \/.fromTryCatchNonFatal(a.toLong).leftMap(_ => ParseError(a, "long")))

  implicit val doubleProperty: Property[Double] =
    instance(_.toString, a => \/.fromTryCatchNonFatal(a.toDouble).leftMap(_ => ParseError(a, "double")))

  implicit val stringProperty: Property[String] =
    instance(_.toString, _.right)

  implicit def taggedProperty[A, B](implicit ev: Property[A]): Property[A @@ B] =
    ev.xmap[A @@ B](Tag[A, B], b => Tag.unwrap(b))
}

trait PropertyInstances {
  implicit val invariantProperty: InvariantFunctor[Property] =
    new InvariantFunctor[Property] {
      override def xmap[A, B](ma: Property[A], f: A => B, g: B => A): Property[B] = new Property[B] {
        override def show(a: B): String = ma.show(g(a))
        override def read(p: String): ErrorType \/ B = ma.read(p).map(f)
      }
    }
}
