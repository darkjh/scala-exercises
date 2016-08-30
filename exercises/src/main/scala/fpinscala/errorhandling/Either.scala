package fpinscala.errorhandling

import scala.util.{Either => _}

sealed trait Either[+E, +A] {
  // Ex. 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
     case Left(_) => b
     case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}
case class Left[+E](v: E) extends Either[E, Nothing]
case class Right[+A](v: A) extends Either[Nothing, A]

object Either {
  // Ex. 4.7
  // Why this returns the *first* error?
  // Once  `f(h)` give an `Left`, `map2` will always result the `Left` part first
  // b/c it's the first part of the for comprehension
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  // TODO Ex. 4.8
  // `Validation` in scalaz
}

object TestEither extends App {
  val rights = List(Right(1), Right(2))
  println(Either.sequence(rights))

  val lefts = List(Right(1), Left("ohh"), Left("ahh"))
  println(Either.sequence(lefts))
}
