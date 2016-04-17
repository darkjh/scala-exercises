package sid


// Structual types is a way to simulate duck typing in scala
// Impl using reflection under the hood, so has perf impact
// avoid it in general
object StructuralTypes extends App {
  object Resources {
    type Resource = {def close(): Unit}

    def closeResource(r: Resource): Unit = {
      r.close()
    }
  }

  class A {
    def close(): Unit = {
      println("a closed!!")
    }
  }

  val a = new A()

  Resources.closeResource(a)
  Resources.closeResource(System.in)
}