package sid

// Type level `If`

sealed trait TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] <: Up
}


class TTrue extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = TrueType
}


class TFalse extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = FalseType
}


object TypeIfTest extends App {
  type X[T <: TBool] = T#If[String, Int, Any]

  // The type evaluates to `String`
  val x: X[TTrue] = "Hi"

  // Error here, the type evaluates to `String` so can't take an int
  // val x: X[TTrue] = 5
}


// HList

sealed trait HList


final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
  def ::[T](v : T) = HCons(v, this)
  override def toString = head + " :: " + tail
}


final class HNil extends HList {
  def ::[T](v : T) = HCons(v, this)
  override def toString = "Nil"
}


object HList {
  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new HNil
}


object HListTest extends App {
  import HList._

  val x = "Hi" :: 5 :: "abc" :: HNil
  println(x)
}