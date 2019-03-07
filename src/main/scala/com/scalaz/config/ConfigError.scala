package com.scalaz.config

import com.scalaz.config.ConfigError.ErrorType

final case class ConfigError(key: String, error: ErrorType)

object ConfigError {
  sealed trait ErrorType

  case object MissingValue                                    extends ErrorType
  case class ParseError(provided: String, `type`: String)     extends ErrorType
  case class InvalidValue(provided: String, expected: String) extends ErrorType
  case class Unknown(underlying: Throwable)                   extends ErrorType
}
