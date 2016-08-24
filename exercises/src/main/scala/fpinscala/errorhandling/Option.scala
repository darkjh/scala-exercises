package fpinscala.errorhandling

import scala.{Option => _}

// Ex. 4.1
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(a => if (f(a)) Some(a) else None)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.sum / xs.length)
    }
  }

  // Ex. 4.2
  // When want to use an Option
  // use it via map if the function returns a non option
  // use it via flatMap if the function returns another option
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // Ex 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  // Ex. 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft[Option[List[A]]](Some(List())){
      (accu, a) =>
        map2(accu, a)((accu, a) => a :: accu)
      }
      .map(_.reverse)
  }

  // Ex. 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  }
}


object OptionMain extends App {
  import Option._
  println(sequence(List(Some(1), Some(2), Some(3))))
  println(sequence(List(Some(1), None, Some(3))))
}