package com.scalaz.config

package examples

import scalaz.zio.DefaultRuntime

object DependentConfigsExample extends App {
  case class Inner(s3: String, s4: String)
  case class SampleConfig(s1: Int, s2: (String, String))

  def config[F[_]] = new Config[F, SampleConfig] {

    def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, Int]("x1") ~
        (read[F, String]("x2") >> (value => read[F, String](s"${value}_key")))).apply {
        SampleConfig
      }
  }

  val result =
    Config.fromEnv(sys.env, config)

  val runtime = new DefaultRuntime {}

  val parsed = runtime.unsafeRun(result.either)

  assert(
    parsed == Left(
      List(
        ConfigError("x1", ConfigError.MissingValue),
        ConfigError("x2", ConfigError.MissingValue)
      )
    )
  )

  val validConfig = Map("x1" -> "1", "x2" -> "value", "value_key" -> "value key")

  assert(
    runtime.unsafeRun(Config.fromEnv(validConfig, config).either) == Right(
      SampleConfig(1, ("value", "value key"))
    )
  )

  val invalidConfig = Map("x1" -> "1", "x2" -> "value")

  assert(
    runtime.unsafeRun(Config.fromEnv(invalidConfig, config).either) == Left(
      List(ConfigError("value_key", ConfigError.MissingValue))
    )
  )
}
