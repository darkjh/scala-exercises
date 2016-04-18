package sid


object Variance extends App {
  class Invariance[T]

  def func(x: Invariance[AnyVal]) = null
  func(new Invariance[AnyVal])

  // `Invariance[Int]` is not a subtype of `Invariance[AnyVal]`
  // so this cannot work
//  func(new Invariance[Int])

  class Covariance[+T]
  def func2(x: Covariance[AnyVal]) = null

  // This works because of co-variance
  func2(new Covariance[Int])

  class Contravariance[-T]
  def func3(x: Contravariance[AnyVal]) = null

//  func3(new Contravariance[Int])

  // This works because `Any` is a super type of `AnyVal`
  func3(new Contravariance[Any])

  // One useful application of contravariance: function param
  // Firstly for subtypes, we know that we can assign an instance
  // of the subtype to a variable which is the super type

  // Here we can assign `1` to `Any` because `Int` is a subtype of `Any`
  val a: Any = 1

  // Here, a func `Any => String` is a subtype of `String => String`
  // Notice that `Any` is a supertype of `String` in the param
  val x: Any => String = _ => "hey"
  val y: String => String = x

  // Think like this: when we use `y` as a func, it should accept a `String`
  // So assign `x` to `y` is perfectly okay since a `String` is certaily a
  // `Any`
}