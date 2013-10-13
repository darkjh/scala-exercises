package tetrix

/**
 */
class AbstractUI {
  import Stage._
  import java.{util => ju}

  private[this] var state = Stage.newState(Nil)

  private[this] val timer = new ju.Timer()
  timer.scheduleAtFixedRate(new ju.TimerTask {
    def run() = {
      state = tick(state)
    }
  }, 0, 1000)

  def left() = state = moveLeft(state)

  def right() = state =  moveRight(state)

  def rotateCW() = state = rotateClockWise(state)

  def down() = state = tick(state)

  def view: GameView = state.view
}
