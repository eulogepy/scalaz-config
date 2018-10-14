package com.scalaz.config

import com.scalaz.config.ConfigError.ErrorType
import scalaz.{ \/ }

trait Property[A] {
  def show(a: A): PropertyValue
  def read(p: PropertyValue): ErrorType \/ A
  def document: String
}

object Property {
  def apply[A](implicit ev: Property[A]): Property[A] = ev

  def instance[A](
    f: A => PropertyValue,
    g: PropertyValue => ErrorType \/ A,
    doc: String
  ): Property[A] =
    new Property[A] {
      override def show(a: A): PropertyValue              = f(a)
      override def read(p: PropertyValue): ErrorType \/ A = g(p)
      override def document: String                       = doc
    }
}
