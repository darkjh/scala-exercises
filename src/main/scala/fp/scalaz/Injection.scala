package fp.scalaz

object Injection extends App {
  // Monoid definition as usually
  trait Monoid[A] {
    def append(a1: A, a2: A): A
    def zero: A
  }

  // with implicit definition for integers and strings
  // in the companion object
  object Monoid {
    implicit val stringMonoid = new Monoid[String] {
      def append(a: String, b: String): String = a + b
      def zero: String = ""
    }
    implicit val intMonoid = new Monoid[Int] {
      def append(a: Int, b: Int): Int = a + b
      def zero: Int = 0
    }
  }

  // 1. We'd like to add an operator for all types that are
  // inherited from Monoid (method injection) in the scalaz 7
  // way (scalaz uses this method to enrich types in scala's
  // standard library)
  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A
    def |+|(a2: A): A = F.append(value, a2)
  }

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }

  assert((1 |+| 2) == 3)
  assert(("a" |+| "c") == "ac")
}
