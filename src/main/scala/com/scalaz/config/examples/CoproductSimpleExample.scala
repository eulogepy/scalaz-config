package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import com.scalaz.config.ConfigError.MissingValue
import scalaz.effect.IO
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.{ Failure, NonEmptyList, Success, \/ }

object CoproductSimpleExample extends App {
  case class SampleConfig(s1: String \/ Int, s2: String)

  def config[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {
    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, String]("x1").or(read[F, Int]("x2")) |@|
        read[F, String]("x3")) { SampleConfig }
  }

  val mapReader: MapReader[SampleConfig] = Config.reader(config)

  //  User will be already be in the context of IO (ZIO potentially)
  val configParsing = IO.apply(sys.env).map(mapReader(_))

  // If config doesn't exist in env
  // Immediate issue is max error accumulation didn't take place.
  val parsed = configParsing.unsafePerformIO()
  assert(
    parsed == Failure(
      NonEmptyList(
        ConfigError("x1", ConfigError.MissingValue),
        ConfigError("x2", MissingValue)
      )
    )
  )

  // If config exists in the env, and they are valid
  val validConfig = Map("x1" -> "right", "x2" -> "right2", "x3" -> "right3")
  assert(
    mapReader(validConfig) == Success(SampleConfig("right".left[Int], "right3"))
  )

  val anotherConfig = Map("x2" -> "2", "x3" -> "right3")
  assert(
    mapReader(anotherConfig) ==
      Success(SampleConfig(2.right[String], "right3"))
  )
}
