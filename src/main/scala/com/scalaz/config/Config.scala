package com.scalaz.config

import scalaz.{ NonEmptyList, \/ }

trait Config[A] {
  def apply[F[_]](implicit F: ConfigSyntax[F, A]): F[A]
}

object Config {
  type MapReader[A] = Map[String, String] => NonEmptyList[ConfigError] \/ A
  type MapWriter[A] = A => Map[String, String]

  // ConfigSyntax should be derived if there is a ConfigSyntax available for each individual fields in  A.
  def reader[A](config: Config[A])(implicit F: ConfigSyntax[MapReader, A]): MapReader[A] =
    config.apply

  // ConfigSyntax should be derived if there is a ConfigSyntax available for each individual fields in A for a Mapwriter.
  def writer[A](config: Config[A])(implicit F: ConfigSyntax[MapWriter, A]): MapWriter[A] =
    config.apply

  final def read[A](key: PropertyKey)(implicit C: Property[A]): MapReader[A] =
    ConfigSyntax[MapReader, A].read(key)
}
