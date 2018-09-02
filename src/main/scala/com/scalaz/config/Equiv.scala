package com.scalaz.config

import scalaz.\/

case class Equiv[A, B](to: A => ConfigError \/ B, from: B => A)