package sid

object PathDependentTypes extends App {
  class Outer {
    trait Inner
    def y = new Inner {}
    def foo(x : this.Inner) = null
    def bar(x : Outer#Inner) = null
  }

  val a = new Outer
  val b = new Outer

  // Perfectly ok
  a.foo(a.y)

  // Problem here
  // need `x.Inner` but given `y.Inner`
//  a.foo(b.y)

  // With type projection all compile
  a.bar(a.y)
  a.bar(b.y)
}


// A more complex example of path dependent type

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
