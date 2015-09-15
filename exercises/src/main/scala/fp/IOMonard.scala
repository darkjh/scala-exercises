package fp


object RTConsole {
  def getString =
    IOAction(Console.readLine())
  def putString(s: String) =
    IOAction(Console.println(s))
}

sealed abstract class IOAction[+A] extends
    Function1[WorldState, (WorldState, A)] {
  def map[B](f: A => B): IOAction[B] =
    flatMap {x => IOAction(f(x))}
  def flatMap[B](f: A => IOAction[B]): IOAction[B] =
    new ChainedAction(this, f)

  private class ChainedAction[+A, B](
      action1: IOAction[B],
      f: B => IOAction[A]) extends IOAction[A] {
    def apply(state1: WorldState) = {
      val (state2, intermediateResult) =
        action1(state1)
      val action2 = f(intermediateResult)
      action2(state2)
    }
  }
}

object IOAction {
  def apply[A](expression: => A): IOAction[A] =
    new SimpleAction(expression)

  private class SimpleAction[+A](expression: => A)
      extends IOAction[A] {
    def apply(state:WorldState) =
      (state.nextState, expression)
  }
}

// the rest remains the same
sealed trait WorldState{
  def nextState: WorldState
}

abstract class IOApplication {
  private class WorldStateImpl(id: BigInt)
      extends WorldState {
    def nextState = new WorldStateImpl(id + 1)
  }
  final def main(args: Array[String]): Unit = {
    val ioAction = iomain(args)
    ioAction(new WorldStateImpl(0))
  }
  def iomain(args:Array[String]): IOAction[_]
}

object HelloWorld extends IOApplication {
  import RTConsole._
  override def iomain(args:Array[String]) = {
    for {
        _ <- putString("This is an example of the IO monad.")
        _ <- putString("What's your name?")
        name <- getString
        _ <- putString("Hello " + name)
    } yield ()
  }
}