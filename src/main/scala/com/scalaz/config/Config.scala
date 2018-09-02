package com.scalaz.config

import scalaz.\/

trait Config[A] {
  def apply[F[_]](implicit F: ConfigSyntax[F, A]): F[A]
}

object Config {
  type MapReader[A] = Map[String, String] => Either[ConfigError, A]
  type MapWriter[A] = A => Either[ConfigError, Map[String, String]]

  def reader[A](config: Config[A]): MapReader[A] =
    config.apply(new ConfigSyntax[MapReader, A] {
      override def read(key: PropertyKey)(implicit P: Property[A]): MapReader[A]             = ???
      override def point(a: => A): MapReader[A]                                              = ???
      override def map[B](f: MapReader[A])(fa: A => B): MapReader[B]                         = ???
      override def product[B](l: => MapReader[A], r: => MapReader[B]): MapReader[(A, B)]     = ???
      override def coproduct[B](l: => MapReader[A], r: => MapReader[B]): MapReader[\/[A, B]] = ???
    })

  def writer[A](config: Config[A]): MapWriter[A] =
    config.apply(new ConfigSyntax[MapWriter, A] {
      override def read(key: PropertyKey)(implicit P: Property[A]): MapWriter[A]             = ???
      override def point(a: => A): MapWriter[A]                                              = ???
      override def map[B](f: MapWriter[A])(fa: A => B): MapWriter[B]                         = ???
      override def product[B](l: => MapWriter[A], r: => MapWriter[B]): MapWriter[(A, B)]     = ???
      override def coproduct[B](l: => MapWriter[A], r: => MapWriter[B]): MapWriter[\/[A, B]] = ???
    })
}
