package sid


// a way to
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


trait Observable {
  import scala.collection.mutable
  // Path dependent type here
  type Handle

  val callbacks = new mutable.HashMap[Handle, this.type => Unit]()

  def observe(callback: this.type => Unit): Handle = {
    val handle = createHandle(callback)
    callbacks += (handle -> callback)
    handle
  }

  def unobserve(handle: Handle) : Unit = {
    callbacks -= handle
  }

  protected def createHandle(callback: this.type => Unit): Handle

  protected def notifyListeners() : Unit =
    for(callback <- callbacks.values) callback(this)
}


trait DefaultHandles extends Observable {
  type Handle = (this.type => Unit)
  protected def createHandle(callback: this.type => Unit): Handle =
    callback
}


class IntStore(private var value: Int)
  extends Observable with DefaultHandles {
  def get : Int = value
  def set(newValue : Int) : Unit = {
    value = newValue
    notifyListeners()
  }
  override def toString : String = "IntStore(" + value + ")"
}


object ObservableTest extends App {
  val callback = println(_: Any)

  val x = new IntStore(4)
  val handlerX = x.observe(callback)

  val y = new IntStore(4)
  val handlerY = y.observe(callback)

  // the handlers created equal
  println(handlerX == handlerY)


// Can't do this even if the runtime object is equal
//  y.unobserve(handlerX)

// Path dependent type restricted us using `x`'s handler to unobserve y
// found   : sid.cp5.ObservableTest.x.Handle
//    (which expands to)  sid.cp5.ObservableTest.x.type => Unit
// required: sid.cp5.ObservableTest.y.Handle
//    (which expands to)  sid.cp5.ObservableTest.y.type => Unit
}