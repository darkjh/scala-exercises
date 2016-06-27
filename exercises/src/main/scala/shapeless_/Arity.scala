package shapeless_


import shapeless._
import syntax.std.function._
import ops.function._


object Arity extends App {
  def applyProduct[P <: Product, F, L <: HList, R]
      (p: P)
      (f: F)
      (implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]) =
    f.toProduct(gen.to(p))

  assert(applyProduct(1, 2)((_: Int)+(_: Int)) == 3)
  assert(applyProduct(1, 2, 3)((_: Int)*(_: Int)*(_: Int)) == 6)
}


