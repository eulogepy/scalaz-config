package com.scalaz.config

import com.scalaz.config.ConfigError.ErrorType

final case class ConfigError(key: PropertyKey, value: ErrorType)

object ConfigError {
  sealed trait ErrorType

  case object MissingValue extends ErrorType
  final case class InvalidValue(provided: String, expected: String) extends ErrorType
}