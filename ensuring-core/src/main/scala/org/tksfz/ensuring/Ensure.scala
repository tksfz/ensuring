package org.tksfz.ensuring

import cats.Monad

/**
  * The result of ensuring some condition may have one of three outcomes:
  * - the condition is already applied (Already)
  * - the condition is not applied (Except)
  * - the condition may be applied and re-probed using an update operation (TBD)
  */
sealed trait State[F[_], +A]
case class Already[F[_], A](value: A) extends State[F, A]
case class TBD[F[_], A](triggeredBy: State[F, _], update: F[State[F, A]]) extends State[F, A]
case class Except[F[_]](err: String) extends State[F, Nothing]

abstract class Ensure[F[_], A](implicit F: Monad[F]) {
  self =>

  import cats.implicits._

  /**
    * Restartable ensure operation
    */
  def ensure: F[State[F, A]]

  def map[B](f: A => B): Ensure[F, B] = {
    flatMap(a => Ensure.already(f(a)))
  }

  def flatMap[B](f: A => Ensure[F, B]): Ensure[F, B] = new Ensure[F, B] {
    def ensure: F[State[F, B]] = {
      deepFlatMap(self.ensure, f)
    }

    private def deepFlatMap(ioa: F[State[F, A]], f: A => Ensure[F, B]): F[State[F, B]] = {
      ioa.flatMap {
        case Already(a) => f(a).ensure
        case ex@Except(_) => F.pure(ex)
        case TBD(t, ioa2) => if (Ensure.planMode) {
          F.pure(TBD(t, deepFlatMap(ioa2, f)))
        } else deepFlatMap(ioa2, f)
      }
    }
  }

  /**
    * Almost like flatTap, but reruns (restarts) this Ensure, on the premise
    * that the subcondition may affect our output
    */
  def subcondition[B](f: A => Ensure[F, B]): Ensure[F, A] = {
    flatMap(a => f(a).flatMap(_ => self))
  }

  def recoverWithDestroyAndRestart(destroy: F[Unit]): Ensure[F, A] = new Ensure[F, A] {
    def ensure: F[State[F, A]] = {
      self.ensure.map {
        case a@Already(_) => a
        case ex@Except(msg) =>
          // destroy then restart
          TBD(ex, destroy.flatMap(_ => self.ensure))
        case a@TBD(_, ioa) =>
          // chain the destroy and restart? infinitely?
          a
      }
    }
  }

  def log(): Ensure[F, A] = new Ensure[F, A] {
    def ensure: F[State[F, A]] = {
      self.ensure.map { state =>
        println("logging: " + state)
        state
      }
    }
  }

  // TODO: introduce E and make f: E => Ensure[F, A]
  def recoverWith(f: Ensure[F, A]): Ensure[F, A] = {
    recoverWithF(f.ensure)
  }

  def recoverWithF(f: F[State[F, A]]): Ensure[F, A] = new Ensure[F, A] {
    def ensure: F[State[F, A]] = {
      deepRecover(self.ensure, f)
    }
  }

  private def deepRecover(io: F[State[F, A]], f: F[State[F, A]]): F[State[F, A]] = {
    io.flatMap {
      case a@Already(_) => F.pure(a)
      case ex@Except(err) => if (Ensure.planMode) F.pure(TBD(ex, f)) else f
      case tbd@TBD(t, io2) => if (Ensure.planMode) F.pure(tbd) else deepRecover(io2, f)
    }
  }

  def recoverWithDefault(f: F[A]): Ensure[F, A] = {
    recoverWithF(f.map[State[F, A]](Already(_)))
  }
}

object Ensure {
  import cats.implicits._

  // Obviously temporary
  val planMode = true

  def equal[F[_], T](a: => T, b: => T)(implicit F: Monad[F]): Ensure[F, T] = new Ensure[F, T] {
    def ensure: F[State[F, T]] = F.pure {
      if (a == b) {
        Already(a)
      } else {
        Except("not equal")
      }
    }
  }

  def already[F[_], T](t: T)(implicit F: Monad[F]): Ensure[F, T] = new Ensure[F, T] {
    def ensure: F[State[F, T]] = F.pure(Already(t))
  }

  def lift[F[_], T](f: F[T])(implicit F: Monad[F]): Ensure[F, T] = new Ensure[F, T] {
    def ensure: F[State[F, T]] = f.map(Already(_))
  }

  def find[F[_], T](io: F[Option[T]])(implicit F: Monad[F]): Ensure[F, T] = new Ensure[F, T] {
    def ensure: F[State[F, T]] = {
      io.map {
        case Some(t) => Already(t)
        case None => Except("not found")
      }
    }
  }
}

