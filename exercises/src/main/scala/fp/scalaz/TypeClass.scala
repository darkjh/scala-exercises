package fp.scalaz


object TypeClassExample extends App {
  // 1. a simple sum function that we wants to generalize
  // A simple sum function which sums a a list of integers
  def sum1(xs: List[Int]): Int =
    xs.foldLeft(0) {_ + _}

  assert(sum1(List(1, 2, 3, 4)) == 10)

  // 2. Monoid
  // Defines the trait Monoid
  trait Monoid[A] {
    def append(a1: A, a2: A): A
    def zero: A
  }

  // Abstract out the Monoid for integers
  object IntMonoid extends Monoid[Int] {
    def append(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  // The sum function now is independent from the concrete type
  // It takes a list and a monoid of that type
  def sum2[A](xs: List[A], m: Monoid[A]): A =
    xs.foldLeft(m.zero)(m.append)

  // Works with integers
  assert(sum2(List(1, 2, 3, 4), IntMonoid) == 10)

  // 3. Monoid with implicits
  // Monoid can be made as an implicit parameter
  def sum3[A](xs: List[A])(implicit m: Monoid[A]): A =
    xs.foldLeft(m.zero)(m.append)

  // For this to work, there must be an implicit int monoid def.
  // in the scope
  implicit val intMonoid = IntMonoid

  // `sum3` is exatly the same as `sum1` in usage
  assert(sum3(List(1, 2, 3, 4)) == 10)

  // 4. Context bound short hand for implicit parameters
  // implicit parameter can also be put as an context bound
  def sum4[A: Monoid](xs: List[A]): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.zero)(m.append)
  }

  assert(sum4(List(1, 2, 3, 4)) == 10)

  // 5. Generalize that to `String`
  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def append(a: String, b: String): String = a + b
    def zero: String = ""
  }

  // Works for strings
  assert(sum4(List("a", "b", "c")) == "abc")

  // So far, we've generalized the `sum` function over the parameter type.
  // It can be ints, strings or whatevet that that defines an implicit
  // `monoid` operation.

  // 6. A type class for FoldLeft
  // The `Monoid` trait above is a type class, we can also generalize
  // foldLeft operation in its type class
  // This is useful in a case where you have some type that you want to
  // mixin some functionality but don't have the access to de code. This
  // is also the main difference between type classes and interfaces.
  // Type parameter `F[_]` here means it takes a container type
  trait FoldLeft[F[_]] {
    def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
  }

  // The companion object is a perfect place for implicit definitions
  object FoldLeft {
    // Actual definition of foldLeft for List type
    implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
      def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B =
        xs.foldLeft(b)(f)
    }
  }

  // Now `sum5` function has nothing to do with specific type or
  // specific foldLeft operation
  def sum5[A: Monoid, M[_]: FoldLeft](xs: M[A]): A = {
    val m = implicitly[Monoid[A]]
    val fl = implicitly[FoldLeft[M]]
    fl.foldLeft(xs, m.zero, m.append)
  }

  // It works like a charm
  assert(sum5(List(1, 2, 3, 4)) == 10)
}