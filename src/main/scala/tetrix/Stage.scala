package tetrix

object Stage {
  private[this] val size = (10, 20)
  private[this] def dropOffPos = (size._1 / 2.0, size._2 - 3.0)
//  private[this] var currentPiece = Piece(dropOffPos, TKind)
//  private[this] var blocks = Block((0, 0), TKind) +: currentPiece.current
//  def view: GameView = GameView(blocks, size, currentPiece.current)

  // Initial game state
  def newState(blocks: Seq[Block]): GameState = {
    val size = (10, 20)
    def dropOffPos = (size._1 / 2.0, size._2 - 3.0)
    val p = Piece(dropOffPos, TKind)
    GameState(blocks ++ p.current, size, p)
  }

  def moveLeft = transformPiece(_.moveBy(-1.0, 0.0))
  def moveRight = transformPiece(_.moveBy(1.0, 0.0))
  def rotateClockWise = transformPiece(_.rotateBy(Math.PI / 2.0))

  private[this] def transformPiece(trans: Piece => Piece): GameState => GameState =
    (s: GameState) => {
      val unloaded = unload(s.currentPiece, s.blocks)
      val moved = trans(s.currentPiece)
      validate(moved) match {
        case Some(p) =>
          s.copy(blocks = load(moved, unloaded), currentPiece = moved)
        case None => s // do nothing
      }
  }

  private[this] def validate(p: Piece): Option[Piece] = {
    def inBounds(pos: (Int, Int)): Boolean = {
      pos._1 >= 0 && pos._1 < size._1 &&
        pos._2 >= 0 && pos._2 < size._2
    }

    if (p.current.map(_.pos).forall(inBounds)) Some(p)
    else None
  }

  private[this] def unload(p: Piece, bs: Seq[Block]): Seq[Block] = {
    val currentPoss = p.current map {_.pos}
    bs filterNot { currentPoss contains _.pos  }
  }

  private[this] def load(p: Piece, bs: Seq[Block]): Seq[Block] =
    bs ++ p.current
}