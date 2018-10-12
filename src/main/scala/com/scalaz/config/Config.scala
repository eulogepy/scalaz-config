package com.scalaz.config

import scalaz.{ Kleisli, NonEmptyList, \/ }

trait Config[F[_], A] {
  def apply(implicit F: ConfigSyntax[F]): F[A]
}

object Config {
  type Env          = Map[String, String]
  type MapReader[A] = Kleisli[NonEmptyList[ConfigError] \/ ?, Env, A]
  type MapWriter[A] = A => Map[String, String]

  // ConfigSyntax should be derived if there is a ConfigSyntax available for each individual fields in  A.
  def reader[A](config: Config[MapReader, A]): MapReader[A] =
    config.apply

  // ConfigSyntax should be derived if there is a ConfigSyntax available for each individual fields in A for a Mapwriter.
  def writer[A](config: Config[MapWriter, A]): MapWriter[A] =
    config.apply
}
