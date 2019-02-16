package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import scalaz.{ -\/, Failure, NonEmptyList, Success, \/, \/- }
import scalaz.syntax.monad._
import MapReader._

object CoproductExample extends App {

  case class SampleConfig(v1: String, v2: String)
  case class AnotherConfig(v1: String, v2: Int, v3: Double)

  def sampleConfig[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, String]("envvar1") |@|
        read[F, String]("envvar2")) {
        SampleConfig
      }
  }

  def anotherConfig[F[_]]: Config[F, AnotherConfig] = new Config[F, AnotherConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[AnotherConfig] =
      (read[F, String]("envvar3") |@| read[F, Int]("envvar4") |@| read[F, Double]("envvar5")) {
        AnotherConfig
      }
  }

  val mapReader: MapReader[SampleConfig \/ AnotherConfig] =
    sampleConfig[MapReader].apply.or(anotherConfig[MapReader].apply)

  // Only variables for left config exists in Env (Ex: use connector1)
  val validConfigForSampleConfig =
    Map(
      "envvar1" -> "v1",
      "envvar2" -> "v2",
      "envvar3" -> "v3"
    )

  assert(
    // A right of coproduct or a left of nonemptylist of errors.
    mapReader(validConfigForSampleConfig) == Success(-\/(SampleConfig("v1", "v2")))
  )

  // Only variables for left config exists in Env (Ex: use connector2 in the absence of connector1)
  val validConfigForAnotherConfig =
    Map(
      "envvar2" -> "v2",
      "envvar3" -> "v3",
      "envvar4" -> "1",
      "envvar5" -> "2.0"
    )

  assert(
    mapReader(validConfigForAnotherConfig) == Success(\/-(AnotherConfig("v3", 1, 2.0)))
  )

  val invalidConfig =
    Map(
      "envvar2" -> "v2",
      "envvar3" -> "v3",
      "envvar4" -> "1",
      "envvar5" -> "notadouble"
    )

  assert(
    mapReader(invalidConfig) == Failure(
      NonEmptyList(
        ConfigError("envvar1", ConfigError.MissingValue),
        ConfigError(
          "envvar5",
          ConfigError.ParseError("notadouble", "double")
        )
      )
    )
  )

  // Pick the left config (priority)
  val allConfigsExist =
    Map(
      "envvar1" -> "v1",
      "envvar2" -> "v2",
      "envvar3" -> "v3",
      "envvar4" -> "1",
      "envvar5" -> "2.0"
    )

  assert(
    mapReader(allConfigsExist) ==
      Success(-\/(SampleConfig("v1", "v2")))
  )
}
