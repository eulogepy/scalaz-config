package com.scalaz.config

import scalaz.Maybe

trait Property[A] {
  def show(a: A): PropertyValue
  def read(p: PropertyValue): Maybe[A]

  def document: String
}
