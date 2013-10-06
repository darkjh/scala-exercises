package tetrix

/**
 */
class AbstractUI {
  private[this] val stage = new Stage((10, 20))

  def left() = stage.moveLeft()

  def right() = stage.moveRight()

  def rotateCW() = stage.rotateClockWise()

  def up() {}

  def down() {}

  def view: GameView = stage.view
}
