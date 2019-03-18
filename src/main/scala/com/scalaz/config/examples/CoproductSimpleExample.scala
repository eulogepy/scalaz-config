package com.scalaz.config

package examples

import com.scalaz.config.ConfigError.MissingValue
import scalaz.zio.DefaultRuntime

object CoproductSimpleExample extends App {
  case class SampleConfig(s1: Either[String, Int], s2: String)

  def config[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      ((read[F, String]("x1") | read[F, Int]("x2")) ~
        read[F, String]("x3")).apply { SampleConfig }
  }

  val runtime = new DefaultRuntime {}
  val result  = runtime.unsafeRun(Config.fromEnv(sys.env, config).either)

  assert(
    result == Left(
      List(
        ConfigError("x1", ConfigError.MissingValue),
        ConfigError("x2", MissingValue),
        ConfigError("x3", MissingValue)
      )
    )
  )

  val validEnv = Map("x1" -> "right", "x2" -> "right2", "x3" -> "right3")
  val validRes = runtime.unsafeRun(Config.fromEnv(validEnv, config).either)

  assert(
    validRes == Right(SampleConfig(Left("right"), "right3"))
  )

  val anotherConfig = Map("x2" -> "2", "x3" -> "right3")
  val anotherRes    = runtime.unsafeRun(Config.fromEnv(anotherConfig, config).either)

  assert(
    anotherRes ==
      Right(SampleConfig(Right(2), "right3"))
  )
}
