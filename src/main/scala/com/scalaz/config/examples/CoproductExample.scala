package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import com.scalaz.config.Config.MapReader._
import scalaz.zio.DefaultRuntime

object CoproductExample extends App {
  case class SampleConfig(v1: String, v2: String)
  case class AnotherConfig(v1: String, v2: Int, v3: Double)

  def firstConfig[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, String]("x1") ~
        read[F, String]("x2")).apply {
        case (a, b) =>
          SampleConfig(a, b)
      }
  }

  def secondConfig[F[_]]: Config[F, AnotherConfig] = new Config[F, AnotherConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[AnotherConfig] =
      (read[F, String]("x3") ~ read[F, Int]("x4") ~ read[F, Double]("x5")) {
        AnotherConfig
      }
  }

  def finalReader[F[_]: ConfigSyntax]: F[Either[SampleConfig, AnotherConfig]] =
    firstConfig[F].apply | secondConfig[F].apply

  val runtime = new DefaultRuntime {}

  val validConfigForSampleConfig =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3"
    )

  assert(
    runtime.unsafeRun(finalReader.provide(validConfigForSampleConfig).either) == Right(
      Left(SampleConfig("v1", "v2"))
    )
  )

  val validConfigForAnotherConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  assert(
    runtime.unsafeRun(finalReader[MapReader].provide(validConfigForAnotherConfig).either) == Right(
      Right(AnotherConfig("v3", 1, 2.0))
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
    runtime.unsafeRun(finalReader[MapReader].provide(invalidConfig).either) == Left(
      List(
        ConfigError("x1", ConfigError.MissingValue),
        ConfigError(
          "x5",
          ConfigError.ParseError("notadouble", "double")
        )
      )
    )
  )

  val allConfigsExist =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  assert(
    runtime.unsafeRun(finalReader[MapReader].provide(allConfigsExist).either) ==
      Right(Left(SampleConfig("v1", "v2")))
  )
}
