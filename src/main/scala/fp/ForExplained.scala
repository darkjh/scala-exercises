package fp

object ForExplained extends App {
  // 1. Simple for-expr
  // Simple for-expr is constructed with map and flatMap
  val as = List(1, 2, 3)
  val bs = List(1, 2, 3)
  val cs = List(1, 2, 3)

  // scala for-expr
  val expected = for {
    a <- as
    b <- bs
    c <- cs
  } yield a * b * c

  // map/flatMap combination
  val res = as.flatMap {
    a => bs.flatMap {
      b => cs.map {
        c => a * b * c
      }
    }
  }

  assert(expected == res)

  // 2. Side-affect for-expr
  // Use `foreach`
  import collection.mutable
  val xs = mutable.ArrayBuffer[Int]()
  val ys = mutable.ArrayBuffer[Int]()

  for {
    x <- List(1, 2, 3)
    y <- List(1, 2, 3)
  } xs += x + y

  // use `foreach` instead of map/flatMap
  List(1, 2, 3).foreach {
    x => List(1, 2, 3).foreach {
      y => ys += x + y
    }
  }

  assert(xs == ys)

  // 3. Filtering (`if` guarded for-expr)
  val xx = List("Abc", "Bbc", "Cbc", "Add")

  val startsWithA = for {
    x <- xx if x.startsWith("A")
  } yield x

  val res2 = xx.withFilter(x => x.startsWith("A")).
    map {
      x => x
    }

  assert(startsWithA == res2)
}