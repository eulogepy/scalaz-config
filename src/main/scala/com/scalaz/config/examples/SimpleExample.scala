package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import scalaz.effect.IO
import scalaz.syntax.monad._
import scalaz.{ Failure, NonEmptyList, Success }

object SimpleExample extends App {
  case class SampleConfig(s1: Int, s2: String)

  def config[F[_]] = new Config[F, SampleConfig] {

    def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, Int]("envvar") |@|
        read[F, String]("envvar2")) { SampleConfig }
  }

  val mapReader: MapReader[SampleConfig] = Config.reader(config)

  //  User will be already be in the context of IO (ZIO potentially)
  val configParsing = IO.apply(sys.env).map(mapReader(_))

  // If config doesn't exist in env
  // Immediate issue is max error accumulation didn't take place
  val parsed = configParsing.unsafePerformIO()
  assert(parsed == Success(NonEmptyList(ConfigError("envvar", ConfigError.MissingValue))))

  // If config exists in the env, and they are valid
  val validConfig = Map("envvar" -> "1", "envvar2" -> "value")
  assert(mapReader(validConfig) == Failure(SampleConfig(1, "value")))

  val invalidConfig = Map("envvar" -> "wrong")
  assert(
    mapReader(invalidConfig) == Failure(
      NonEmptyList(ConfigError("envvar", ConfigError.ParseError("wrong", "int")))
    )
  )
}
