package tetrix

sealed trait PieceKind
case object IKind extends PieceKind
case object JKind extends PieceKind
case object LKind extends PieceKind
case object OKind extends PieceKind
case object SKind extends PieceKind
case object TKind extends PieceKind
case object ZKind extends PieceKind

case class Block(pos: (Int, Int), kind: PieceKind)

case class GameView(blocks: Seq[Block],
                    gridSize: (Int, Int),
                    current: Seq[Block])

case class GameState(blocks: Seq[Block],
                     gridSize: (Int, Int),
                     currentPiece: Piece) {
  def view: GameView = GameView(blocks, gridSize, currentPiece.current)
}

case class Piece(pos: (Double, Double),
                 kind: PieceKind,
                 shape: Seq[(Double, Double)]) {
  def current: Seq[Block] =
    shape map { case (x, y) =>
      Block((math.floor(x + pos._1).toInt,
        math.floor(y + pos._2).toInt), kind)
    }

  def moveBy(delta: (Double, Double)): Piece =
    copy(pos = (pos._1 + delta._1, pos._2 + delta._2))

  def rotateBy(theta: Double) = {
    val c = math.cos(theta)
    val s = math.sin(theta)
    def roundToHalf(v: (Double, Double)): (Double, Double) =
      (math.round(v._1 * 2.0) * 0.5, math.round(v._2 * 2.0) * 0.5)

    // rotation formula
    copy(shape = shape map { case(x, y) =>
      (x * c - y * s, x * s + y * c) } map roundToHalf)
  }
}
case object Piece {
  def apply(pos: (Double, Double), kind: PieceKind): Piece =
    Piece(pos, kind, kind match {
      case TKind => Seq((-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (0.0, 1.0))
      case IKind => Seq((-1.5, 0.0), (-0.5, 0.0), (0.5, 0.0), (1.5, 0.0))
      case JKind => Seq((-1.0, 0.5), (0.0, 0.5), (1.0, 0.5), (1.0, -0.5))
      case LKind => Seq((-1.0, 0.5), (0.0, 0.5), (1.0, 0.5), (-1.0, -0.5))
      case OKind => Seq((-0.5, 0.5), (0.5, 0.5), (-0.5, -0.5), (0.5, -0.5))
      case SKind => Seq((0.0, 0.5), (1.0, 0.5), (-1.0, -0.5), (0.0, -0.5))
      case ZKind => Seq((-1.0, 0.5), (0.0, 0.5), (0.0, -0.5), (1.0, -0.5))
    })
}