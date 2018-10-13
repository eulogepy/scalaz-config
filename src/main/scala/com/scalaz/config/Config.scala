package com.scalaz.config

import scalaz.{ Kleisli, NonEmptyList, \/ }

trait Config[F[_], A] {
  def apply(implicit F: ConfigSyntax[F]): F[A]
}

object Config {
  type Env          = Map[String, String]
  type MapReader[A] = Kleisli[NonEmptyList[ConfigError] \/ ?, Env, A]
  type MapWriter[A] = A => Map[String, String]

  def reader[A](config: Config[MapReader, A]): MapReader[A] =
    config.apply

  def writer[A](config: Config[MapWriter, A]): MapWriter[A] =
    config.apply
}
