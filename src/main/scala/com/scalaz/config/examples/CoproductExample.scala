package com.scalaz.config

package examples

import com.scalaz.config.Config.MapReader
import com.scalaz.config.ConfigError.MissingValue
import scalaz.effect.IO
import scalaz.syntax.either._
import scalaz.{-\/, NonEmptyList, \/, \/-}

object CoproductExample extends App {


  final case class EnvVar1(s: String) extends AnyVal
  final case class EnvVar2(s: String) extends AnyVal
  final case class EnvVar3(s: String) extends AnyVal

  // EnvVar1 \/ EnvVar2 could be a simple ADT.
  // In that case, explicit instance for EnvVar1 and EnvVar2 is wrong.
  case class SampleConfig(s1: String \/ Int, s2: String)

  def config[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {

    val equiv = Equiv[String \/ Int ~ String, SampleConfig](
      a => SampleConfig(a._1, a._2),
      s => s.s1 -> s.s2
    )

    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, String]("envvar").or(read[F, Int]("envvar2")) ~
        read[F, String]("envvar3")).map(equiv)
  }

  val mapReader: MapReader[SampleConfig] = Config.reader(config)

  //  User will be already be in the context of IO (ZIO potentially)
  val configParsing = IO.apply(sys.env).map(mapReader(_))

  // If config doesn't exist in env
  // Immediate issue is max error accumulation didn't take place.
  val parsed = configParsing.unsafePerformIO()
  assert(
    parsed == -\/(
      NonEmptyList(
        ConfigError("envvar", ConfigError.MissingValue),
        ConfigError("envvar2", MissingValue)
      )
    )
  )

  // If config exists in the env, and they are valid
  val validConfig = Map("envvar" -> "right", "envvar2" -> "right2", "envvar3" -> "right3")
  assert(
    mapReader(validConfig) == \/-(SampleConfig("right".left[Int],  "right3"))
  )

  val anotherConfig = Map("envvar2" -> "2", "envvar3" -> "right3")
  assert(
    mapReader(anotherConfig) ==
      \/-(SampleConfig(2.right[String],  "right3"))
  )
}
