package tetrix

import akka.actor.Actor

// messages for ui actor
sealed trait StageMessage
case object MoveLeft extends StageMessage
case object MoveRight extends StageMessage
case object RotateClockWise extends StageMessage
case object Tick extends StageMessage
// case object Drop extends StageMessage
case object View extends StageMessage

class StageActor(s: GameState) extends Actor {
  import Stage._

  // mutation is limited
  private[this] var state = s

  def receive: Actor.Receive = {
    case MoveLeft  => state = moveLeft(state)
    case MoveRight => state = moveRight(state)
    case RotateClockWise  => state = rotateClockWise(state)
    case Tick      => state = tick(state)
    case View      => sender ! state.view
  }
}

class AbstractUI {
  import akka.actor._
  import akka.pattern.ask
  import scala.concurrent.duration._
  import akka.util.Timeout
  import scala.concurrent._
  implicit val timeout = Timeout(1 second)
  import ExecutionContext.Implicits.global


  private[this] val initState = Stage.initState(Nil)
  private[this] val system = ActorSystem("TetrixSystem")
  private[this] val playerActor =
    system.actorOf(Props[StageActor](new StageActor(initState)),
      "playerActor")
  private[this] val timer =
    system.scheduler.schedule(0 millisecond,
      1000 millisecond, playerActor, Tick)

  def left() = playerActor ! MoveLeft
  def right() = playerActor ! MoveRight
  def rotateCW() = playerActor ! RotateClockWise
  def down() = playerActor ! Tick
  def view: GameView =
    Await.result((playerActor ? View).mapTo[GameView], timeout.duration)
}