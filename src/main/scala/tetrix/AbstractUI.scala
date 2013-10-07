package tetrix

/**
 */
class AbstractUI {
  import Stage._

  private[this] var state = Stage.newState(Nil)

  def left() = state = moveLeft(state)

  def right() = state =  moveRight(state)

  def rotateCW() = state = rotateClockWise(state)

  def view: GameView = state.view
}
