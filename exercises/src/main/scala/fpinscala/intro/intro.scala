package fpinscala.intro

import scala.annotation.tailrec

object Intro extends App {
  // Ex. 2.1
  def fib(n: Int): Int = {
    if (n <= 1) {
      n
    } else {
      fib(n - 2) + fib(n - 1)
    }
  }

  // Ex. 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def recurse(n: Int): Boolean = {
      if (n >= as.length - 1) {
        true
      } else {
        if (ordered(as(n), as(n + 1))) {
          recurse(n + 1)
        } else {
          false
        }
      }
    }

    recurse(0)
  }

  // Ex. 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // Ex. 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Ex. 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}