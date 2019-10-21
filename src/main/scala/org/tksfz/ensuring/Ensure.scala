package org.tksfz.ensuring

import cats.effect.IO

/**
  * The result of ensuring some condition may have one of three outcomes:
  * - the condition is already applied (Already)
  * - the condition is not applied (Except)
  * - the condition may become applied and re-probed with some operations (TBD)
  */
sealed trait State[+A] {
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

  /**
    * Restartable ensure operation
    */
  def ensure: IO[State[A]]

  def map[B](f: A => B): Ensure[B] = {
    flatMap(a => Ensure.already(f(a)))
  }

  def flatMap[B](f: A => Ensure[B]): Ensure[B] = new Ensure[B] {
    def ensure: IO[State[B]] = {
      deepFlatMap(self.ensure, f)
    }

    private def deepFlatMap(ioa: IO[State[A]], f: A => Ensure[B]): IO[State[B]] = {
      ioa.flatMap {
        case Already(a) => f(a).ensure
        case ex@Except(_) => IO.delay(ex)
        case TBD(ioa2) => deepFlatMap(ioa2, f)
      }
    }
  }

  /**
    * Almost like flatTap, but reruns (restarts) this Ensure, on the premise
    * that the subcondition may affect our output
    */
  def subcondition[B](f: A => Ensure[B]): Ensure[A] = {
    flatMap(a => f(a).flatMap(_ => self))
  }

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

  def log(): Ensure[A] = new Ensure[A] {
    def ensure: IO[State[A]] = {
      self.ensure.map { state =>
        println("logging: " + state)
        state
      }
    }
  }

  def recoverWith(f: Ensure[A]): Ensure[A] = new Ensure[A] {
    def ensure: IO[State[A]] = {
      deepRecover(self.ensure, f.ensure)
    }
  }

  def recoverWith(f: IO[State[A]]): Ensure[A] = new Ensure[A] {
    def ensure: IO[State[A]] = {
      deepRecover(self.ensure, f)
    }
  }

  private def deepRecover(io: IO[State[A]], f: IO[State[A]]): IO[State[A]] = {
    io.flatMap {
      case a@Already(_) => IO.delay(a)
      case Except(err) => f
      case TBD(io2) => deepRecover(io2, f)
    }
  }

  def recover(f: IO[A]): Ensure[A] = new Ensure[A] {
    def ensure: IO[State[A]] = {
      deepRecover(self.ensure, f.map(Already(_)))
    }
  }
}

object Ensure {
  def equal[T](a: => T, b: => T): Ensure[T] = new Ensure[T] {
    def ensure: IO[State[T]] = IO.delay {
      if (a == b) {
        Already(a)
      } else {
        Except("not equal")
      }
    }
  }

  def already[T](t: T): Ensure[T] = new Ensure[T] {
    def ensure: IO[State[T]] = IO.delay(Already(t))
  }

  def lift[T](f: IO[T]): Ensure[T] = new Ensure[T] {
    def ensure: IO[State[T]] = f.map(Already(_))
  }

  def find[T](io: IO[Option[T]]): Ensure[T] = new Ensure[T] {
    def ensure: IO[State[T]] = {
      io.map {
        case Some(t) => Already(t)
        case None => Except("not found")
      }
    }
  }
}

