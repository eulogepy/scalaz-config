package com.scalaz.config

package examples
import com.scalaz.config.ConfigError.ErrorType
import scalaz.{-\/, NonEmptyList, \/, \/-}
import scalaz.syntax.equal._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.effect.IO

object Main extends App {
  final case class EnvVar1(s: String) extends AnyVal
  final case class EnvVar2(s: String) extends AnyVal

  case class SampleConfig(s1: EnvVar1, s2: EnvVar2)

  implicit val propertyEnvVar1: Property[EnvVar1] = new Property[EnvVar1] {
    override def show(a: EnvVar1): PropertyValue = a.s

    override def read(p: PropertyValue): ErrorType \/ EnvVar1 =
      if (p === "right") EnvVar1(p).right else ConfigError.InvalidValue(p, "right").left

    override def document: String =
      "This is first property from the environment that can only be right."
  }

  implicit val propertyEnvVar2: Property[EnvVar2] = new Property[EnvVar2] {
    override def show(a: EnvVar2): PropertyValue = a.s

    override def read(p: PropertyValue): ErrorType \/ EnvVar2 =
      if (p === "right2") EnvVar2(p).right else ConfigError.InvalidValue(p, "right2").left

    override def document: String =
      "This is first property from the environment that can only be right."
  }

  import Config._

  val mapReader: MapReader[(EnvVar1, EnvVar2)] =
    read[EnvVar1](key = "envvar") ~
      read[EnvVar2](key = "envvar2")


  //  User will be already be in the context of IO (ZIO potentially)
  val configParsing = IO.apply(sys.env).map(mapReader(_))


  // If config doesn't exist in env
  // Immediate issue is max error accumulation didn't take place
  val parsed = configParsing.unsafePerformIO()
  assert(parsed == -\/(NonEmptyList(ConfigError("envvar", ConfigError.MissingValue))) )

  // If config exists in the env, and they are valid
  val validConfig = Map("envvar" -> "right", "envvar2" -> "right2")
  assert(mapReader(validConfig) == \/-((EnvVar1("right"),EnvVar2("right2"))))

  val invalidConfig = Map("envvar" -> "wrong")
  assert(mapReader(invalidConfig) == -\/(NonEmptyList(ConfigError("envvar", ConfigError.InvalidValue("wrong", "right"))) ))
}
