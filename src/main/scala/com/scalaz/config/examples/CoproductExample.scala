package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import scalaz.{ -\/, Failure, NonEmptyList, Success, \/, \/- }
import scalaz.syntax.monad._
import MapReader._

object CoproductExample extends App {

  case class SampleConfig(v1: String, v2: String)
  case class AnotherConfig(v1: String, v2: Int, v3: Double)

  def firstConfig[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, String]("x1") |@|
        read[F, String]("x2")) {
        SampleConfig
      }
  }

  def secondConfig[F[_]]: Config[F, AnotherConfig] = new Config[F, AnotherConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[AnotherConfig] =
      (read[F, String]("x3") |@| read[F, Int]("x4") |@| read[F, Double]("x5")) {
        AnotherConfig
      }
  }

  def finalReader[F[_]: ConfigSyntax]: F[SampleConfig \/ AnotherConfig] =
    firstConfig[F].apply.or(secondConfig[F].apply)

  // Only variables for left config exists in Env (Ex: use connector1)
  val validConfigForSampleConfig =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3"
    )

  assert(
    // A right of coproduct or a left of nonemptylist of errors.
    finalReader[MapReader].apply(validConfigForSampleConfig) == Success(
      -\/(SampleConfig("v1", "v2"))
    )
  )

  // Only variables for left config exists in Env (Ex: use connector2 in the absence of connector1)
  val validConfigForAnotherConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  assert(
    finalReader[MapReader].apply(validConfigForAnotherConfig) == Success(
      \/-(AnotherConfig("v3", 1, 2.0))
    )
  )

  val invalidConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "notadouble"
    )

  assert(
    finalReader[MapReader].apply(invalidConfig) == Failure(
      NonEmptyList(
        ConfigError("x1", ConfigError.MissingValue),
        ConfigError(
          "x5",
          ConfigError.ParseError("notadouble", "double")
        )
      )
    )
  )

  // Pick the left config (priority)
  val allConfigsExist =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  assert(
    finalReader[MapReader].apply(allConfigsExist) ==
      Success(-\/(SampleConfig("v1", "v2")))
  )
}
