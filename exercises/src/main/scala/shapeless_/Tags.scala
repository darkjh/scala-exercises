package shapeless_

import shapeless.tag
import shapeless.tag.@@

trait KiloGram

/**
 * About tags
 *   http://eed3si9n.com/learning-scalaz/Tagged+type.html
 *   http://etorreborre.blogspot.ie/2011/11/practical-uses-for-unboxed-tagged-types.html
 */
object Tags extends App {
  // this func needs a kilogram, which happens to be represented in Doubles
  def func(weight: Double @@ KiloGram): Unit = {
    println(weight.getClass)
  }

  // compile time error (type mismatch)
  // func(0.5d)

  func(tag[KiloGram](0.5d))
  // >>> class java.lang.Double
  // so at runtime the tag is removed
}