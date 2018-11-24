package com.scalaz.config
package examples

import com.scalaz.config.Config.MapReader
import scalaz.{-\/, NonEmptyList, \/, \/-}

object CoproductExample extends App {

  case class SampleConfig(v1: String, v2: String)
  case class AnotherConfig(v1: String, v2: Int, v3: Double)

  def sampleConfig[F[_]]: Config[F, SampleConfig] = new Config[F, SampleConfig] {
    val equiv = Equiv[String ~ String, SampleConfig](
      a => SampleConfig(a._1, a._2),
      s => s.v1 -> s.v2
    )

    override def apply(implicit F: ConfigSyntax[F]): F[SampleConfig] =
      (read[F, String]("envvar1") ~
        read[F, String]("envvar2")).map(equiv)
  }


  def anotherConfig[F[_]]: Config[F, AnotherConfig] = new Config[F, AnotherConfig] {
    val equiv = Equiv[String ~ Int ~ Double, AnotherConfig]({
      case ((a, b), c) => AnotherConfig(a, b, c)
    },
      s => ((s.v1, s.v2), s.v3)
    )

    override def apply(implicit F: ConfigSyntax[F]): F[AnotherConfig] =
      (read[F, String]("envvar3") ~ read[F, Int]("envvar4") ~ read[F, Double]("envvar5")).map(equiv)
  }

  val mapReader: MapReader[SampleConfig \/ AnotherConfig] =
    sampleConfig[MapReader].apply.or(anotherConfig[MapReader].apply)
  
  // If config exists in the env, and they are valid
  val validConfigForSampleConfig = Map("envvar1" -> "v1", "envvar2" -> "v2", "envvar3" -> "v3")

  assert(
    // Either a coproduct of right or a left of nonemptylist of errors.
    mapReader(validConfigForSampleConfig) ==  \/-(-\/(SampleConfig("v1", "v2")))
  )

  val validConfigForAnotherConfig = Map("envvar2" -> "v2", "envvar3" -> "v3", "envvar4" -> "1", "envvar5" -> "2.0")

  assert(
    // Either a coproduct of right or a left of nonemptylist of errors.
    mapReader(validConfigForAnotherConfig) == \/-(\/-(AnotherConfig("v3", 1, 2.0)))
  )

  val invalidConfig = Map("envvar2" -> "v2", "envvar3" -> "v3", "envvar4" -> "1", "envvar5" -> "notadouble")

  println(mapReader(invalidConfig))

  assert(
    mapReader(invalidConfig) == -\/(
      NonEmptyList(
        ConfigError("envvar1", ConfigError.MissingValue),
        ConfigError("envvar5", ConfigError.ParseError("notadouble", "double")
        )
      )
    )
  )
}
