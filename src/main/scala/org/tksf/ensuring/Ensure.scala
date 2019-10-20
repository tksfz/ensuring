package org.tksf.ensuring

import cats.effect.IO

trait Ensure[+T]

case class Already[T](value: T) extends Ensure[T]
case class TBD[T](update: IO[Ensure[T]]) extends Ensure[T]
case class Except(err: String) extends Ensure[Nothing]