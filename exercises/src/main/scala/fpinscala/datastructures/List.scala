package fpinscala.datastructures

import scala.annotation.tailrec


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

  // Ex. 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    } else {
      l
    }
  }

  // Ex. 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) =>
        dropWhile(xs, f)
      case _ => l
    }

  // By using 2 param list, scala can actually infer the types
  // TODO why is this?
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) =>
        dropWhile(xs, f)
      case _ => l
    }

  // Ex. 3.6
  // Not efficient impl
  // Should use internal mutable data structure
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Ex. 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, accu) => accu + 1)
  }

  // Ex. 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Ex. 3.11
  def length1[A](as: List[A]): Int = {
    foldLeft(as, 0)((accu, _) => accu + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((accu, x) => Cons(x, accu))
  }

  // Ex. 3.13
  def foldRight1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    ???
  }

  // Ex. 3.14
  def append[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_, _))
  }

  // Ex. 3.15
  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  // Ex. 3.16
  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  // Ex. 3.17
  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  // Ex. 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Ex. 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  // Ex. 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  // Ex. 3.21
  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  // Ex. 3.22
  def addListPairWise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addListPairWise(xs, ys))
    case _ => throw new Exception("Unmatched lists")
  }

  // Ex. 3.23
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => throw new Exception("Unmatched lists")
  }

  // Ex. 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    ???
  }
}


object ListMain extends App {
  import List._

  println(drop(List(1, 2, 3, 4), 2))
  println(drop(List(1, 2, 3, 4), 8))
  println(drop(List(1, 2, 3, 4), 4))

  println(dropWhile(List(1, 2, 3, 4), (a: Int) => a <= 2))
  println(dropWhile2(List(1, 2, 3, 4))(_ <= 2))

  println(length(List(1, 2, 3, 4)))
  println(length1(List(1, 2, 3, 4)))

  println(append(List(1, 2, 3), List(4, 5, 6)))

  println(add1(List(1, 2, 3)))

  println(addListPairWise(List(1, 2, 3), List(4, 5, 6)))
}