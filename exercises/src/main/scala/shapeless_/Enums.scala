package shapeless_

import shapeless._

// An ADT+shapeless as a drop-in replacement for a standard Scala Enumeration.
//
// First the unsafe standard Scala Enumeration ...
//
object ScalaEnumDemo extends App {
  // Example from scala.Enumeration scaladoc. Terse ...
  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  import WeekDay._

  def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)

  assert((WeekDay.values filter isWorkingDay) == Set(Mon, Tue, Wed, Thu, Fri))

  // However ...

  def isWeekend(d: WeekDay) = d match {
    case Sat | Sun => true
    // Oops! Missing case ... still compiles
  }

  assert(!isWeekend(Mon)) // MatchError at run time
}

// A safer ADT+shapeless alternative ...
//
object ShapelessEnumDemo extends App {
  // ADT as an enumeration. Barely any more boilerplate ...
  sealed trait WeekDay
  object WeekDay {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = new WeekDay {}
    val values: Set[WeekDay] = Values
  }

  import WeekDay._

  def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)

  assert((WeekDay.values filter isWorkingDay) == Set(Mon, Tue, Wed, Thu, Fri))

  // ... the payoff ...

  def isWeekend(d: WeekDay) = d match {
    case Sat | Sun => true
    case _ => false // compile time non-exhaustive match warning/error without this case
  }

  assert(!isWeekend(Mon)) //
}

// Infrastructure for the above. Original version due to Travis Brown,
//
//   http://stackoverflow.com/questions/25838411
//
object Values {
  implicit def conv[T](self: this.type)(implicit v: MkValues[T]): Set[T] = Values[T]

  def apply[T](implicit v: MkValues[T]): Set[T] = v.values.toSet

  trait MkValues[T] {
    def values: List[T]
  }

  object MkValues {
    implicit def values[T, Repr <: Coproduct]
      (implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): MkValues[T] =
        new MkValues[T] { def values = v.values }

    trait Aux[T, Repr] {
      def values: List[T]
    }

    object Aux {
      implicit def cnilAux[A]: Aux[A, CNil] =
        new Aux[A, CNil] { def values = Nil }

      implicit def cconsAux[T, L <: T, R <: Coproduct]
        (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
        new Aux[T, L :+: R] { def values = l.value :: r.values }
    }
  }
}


object Test extends App {
  case class Person(name: String, age: Int)

  val gp = Generic[Person]

  val john = Person("John", 40)
  val hl: String :: Int :: HNil = gp.to(john)
  val p: Person = gp.from(hl)

  println(p)
}