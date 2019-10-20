package org.tksf.ensuring

import cats.effect.IO

trait State[+A] {
  def map[B](f: A => B): State[B] = {
    flatMap(a => Already(f(a)))
  }

  def flatMap[B](f: A => State[B]): State[B] = this match {
    case Already(a) => f(a)
    case ex@Except(_) => ex
    case TBD(op) => TBD(op.map(_.flatMap(f)))
  }
}
case class Already[A](value: A) extends State[A]
case class TBD[A](op: IO[State[A]]) extends State[A]
case class Except(err: String) extends State[Nothing]

trait Ensure[A] {
  self =>
  def ensure: IO[State[A]]

  def recoverWithDestroyAndRestart(destroy: IO[Unit]): Ensure[A] = new Ensure[A] {
    def ensure: IO[State[A]] = {
      self.ensure.map {
        case a@Already(_) => a
        case Except(msg) =>
          // destroy then restart
          TBD(destroy.flatMap(_ => self.ensure))
        case a@TBD(ioa) =>
          // chain the destroy and restart? infinitely?
          a
      }
    }
  }

  def map[B](f: A => B): Ensure[B] = {
    flatMap(a => Ensure.already(f(a)))
  }

  def flatMap[B](f: A => Ensure[B]): Ensure[B] = new Ensure[B] {
    def ensure: IO[State[B]] = {
      self.ensure.flatMap {
        case Already(a) => f(a).ensure
        case ex@Except(err) => IO.delay(ex)
        case TBD(ioa) => ioaToIOStateB(ioa, f)
      }
    }

    def ioaToIOStateB(ioa: IO[State[A]], f: A => Ensure[B]): IO[State[B]] = {
      IO.delay(TBD(ioa.flatMap {
        case Already(a) => f(a).ensure
        case ex@Except(_) => IO.delay(ex)
        case TBD(ioa2) => ioaToIOStateB(ioa2, f)
      }))
    }
  }
}

object Ensure {
  def already[T](t: T): Ensure[T] = new Ensure[T] {
    def ensure: IO[State[T]] = IO.delay(Already(t))
  }

  def find[T](io: IO[T]): Ensure[T] = new Ensure[T] {
    def ensure: IO[State[T]] = {
      io.map(Already(_))
    }
  }
}

