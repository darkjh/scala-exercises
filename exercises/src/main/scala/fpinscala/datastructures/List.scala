package fpinscala.datastructures


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0d, _) => 0d
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }

  // Ex. 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  // Ex. 3.3
  def setHead[A](l: List[A], e: A): List[A] = l match {
    case Cons(x, xs) => Cons(e, xs)
    case Nil => Cons(e, Nil)
  }
}