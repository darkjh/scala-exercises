package tetrix

object Stage {
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
      val newState = s.copy(blocks = load(moved, unloaded), currentPiece = moved)
      validate(newState) match {
        case Some(ns) => ns
        case None => s // do nothing
      }
  }

  private[this] def validate(s: GameState): Option[GameState] = {
    val size = s.gridSize
    val currentPos = s.currentPiece.current.map(_.pos)

    def inBounds(pos: (Int, Int)): Boolean = {
      pos._1 >= 0 && pos._1 < size._1 &&
        pos._2 >= 0 && pos._2 < size._2
    }

    if (currentPos.forall(inBounds) &&
      s.blocks.intersect(currentPos).isEmpty) Some(s)
    else None
  }

  private[this] def unload(p: Piece, bs: Seq[Block]): Seq[Block] = {
    val currentPoss = p.current map {_.pos}
    bs filterNot { currentPoss contains _.pos  }
  }

  private[this] def load(p: Piece, bs: Seq[Block]): Seq[Block] =
    bs ++ p.current
}