package shapeless_

import shapeless._
import syntax.std.product._
import ops.function._


case class A(a: Int, b: String)


object Basic extends App {
  val hl  = 1 :: "String" :: 2.0 :: HNil

  // from HList to tuple
  val t  = hl.tupled
  println(t)

  // from tuple to HList
  println(t.productElements)

  // from case classes to HList
  println(A(1, "1").productElements)
}


object Generics extends App {
  // convert HLists to Products (case classes and tuples)
  // the type parameter way using Generic
  val gp = Generic[A]
  val a = A(1, "111")
  val ghl: Int :: String :: HNil = gp.to(a)
  val aa: A = gp.from(ghl)

  assert(aa == a)


  val f = (i: Int, s: String) => s"$i $s"
  val ft: ((Int, String)) => String = f.tupled

  // convert a fn's param list to a HList
  // the lifted fn takes a HList as param
  import syntax.std.function._
  val fhl: Int::String::HNil => String = f.toProduct

  // 1. convert a case class to HList
  // 2. lift the fn to accept a HList as param
  // 3. apply the lifted fn to the converted case class
  println(f.toProduct(gp.to(A(123, "hooo"))))


}