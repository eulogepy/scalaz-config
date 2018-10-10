package com.scalaz.config

import com.scalaz.config.ConfigError.ErrorType
import scalaz.{\/}

trait Property[A] {
  def show(a: A): PropertyValue
  def read(p: PropertyValue): ErrorType \/ A
  def document: String
}

object Property {
  def apply[A](implicit ev: Property[A]): Property[A] = ev
}
