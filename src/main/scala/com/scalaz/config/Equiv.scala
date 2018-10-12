package com.scalaz.config

case class Equiv[A, B](to: A => B, from: B => A)
