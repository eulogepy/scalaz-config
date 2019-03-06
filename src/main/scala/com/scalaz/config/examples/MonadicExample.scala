package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import scalaz.NonEmptyList
import scalaz.effect.IO
import scalaz.syntax.monad._

object MonadicExample extends App {
  case class Inner(s3: String, s4: String)
  case class SampleConfig(s1: Int, s2: (String, String))

  def config[F[_]] = new Config[F, SampleConfig] {

    def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, Int]("x1") |@|
        read[F, String]("x2") ~ (value => read[F, String](s"${value}_key"))) { SampleConfig }
  }

  val mapReader: MapReader[SampleConfig] = Config.reader(config)

  //  User will be already be in the context of IO (ZIO potentially)
  val configParsing = IO.apply(sys.env).map(mapReader(_))

  // If config doesn't exist in env
  // Immediate issue is max error accumulation didn't take place
  val parsed = configParsing.unsafePerformIO()

  assert(
    parsed == Left(
      NonEmptyList(
        ConfigError("x1", ConfigError.MissingValue),
        ConfigError("x2", ConfigError.MissingValue)
      )
    )
  )

  // If config exists in the env, and they are valid
  val validConfig = Map("x1" -> "1", "x2" -> "value", "value_key" -> "value key")
  assert(mapReader(validConfig) == Right(SampleConfig(1, ("value", "value key"))))

  val invalidConfig = Map("x1" -> "1", "x2" -> "value")

  assert(
    mapReader(invalidConfig) == Left(
      NonEmptyList(ConfigError("value_key", ConfigError.MissingValue))
    )
  )
}
