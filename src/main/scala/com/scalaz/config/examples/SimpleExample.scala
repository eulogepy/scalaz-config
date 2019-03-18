package com.scalaz.config

package examples

import scalaz.zio.DefaultRuntime

object SimpleExample extends App {
  case class SampleConfig(s1: Int, s2: String)

  def config[F[_]] = new Config[F, SampleConfig] {

    def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, Int]("envvar") ~
        read[F, String]("envvar2")).apply {
        SampleConfig
      }
  }

  val runtime = new DefaultRuntime {}
  val parsed  = runtime.unsafeRun(Config.fromEnv(sys.env, config).either)

  assert(
    parsed ==
      Left(
        List(
          ConfigError("envvar", ConfigError.MissingValue),
          ConfigError("envvar2", ConfigError.MissingValue)
        )
      )
  )

  val validConfig = Map("envvar" -> "1", "envvar2" -> "value")

  val validRes = runtime.unsafeRun(Config.fromEnv(validConfig, config).either)

  assert(validRes == Right(SampleConfig(1, "value")))

  val invalidConfig = Map("envvar" -> "wrong")

  val invalidRes = runtime.unsafeRun(Config.fromEnv(invalidConfig, config).either)

  assert(
    runtime.unsafeRun(Config.fromEnv(invalidConfig, config).either) == Left(
      List(
        ConfigError("envvar", ConfigError.ParseError("wrong", "int")),
        ConfigError("envvar2", ConfigError.MissingValue)
      )
    )
  )
}
