package com.scalaz.config

import com.scalaz.config.ConfigError.ErrorType
import scalaz.Show
import scalaz.syntax.show._

final case class ConfigError(key: String, value: ErrorType)

object ConfigError {
  sealed trait ErrorType

  case object MissingValue                                    extends ErrorType
  case class ParseError(provided: String, `type`: String) extends ErrorType
  case class InvalidValue(provided: String, expected: String) extends ErrorType

  implicit val showErrorType: Show[ErrorType] = Show.shows[ErrorType]({
    case MissingValue => "The property is missing from the environment."
    case ParseError(provided, t) => s"The provided value $provided cannot be converted to the type $t."
    case InvalidValue(provided, expected) => s"Invalid value. Provided: $provided, Expected: $expected"
  })

  implicit val showConfigError: Show[ConfigError] = Show.shows[ConfigError](a =>
    s"Error reading configuration. ${a.key}. Cause: ${a.value.shows}"
  )
}
