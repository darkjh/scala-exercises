package tetrix

object Stage {
  // Initial game state
  def newState(blocks: Seq[Block]): GameState = {
    val size = (10, 20)
    def dropOffPos = (size._1 / 2.0, size._2 - 3.0)
    val p = Piece(dropOffPos, TKind)
    GameState(blocks, size, p)
  }

  def moveLeft = transit(_.moveBy(-1.0, 0.0))
  def moveRight = transit(_.moveBy(1.0, 0.0))
  def rotateClockWise = transit(_.rotateBy(Math.PI / 2.0))

  // move down the current piece, if collision, spawn a new piece
  val tick = transit(_.moveBy(0.0, -1.0), spawn)

  private[this] def spawn(s: GameState): GameState = {
    def dropOffPos = (s.gridSize._1 / 2.0, s.gridSize._2 - 3.0)
    val p = Piece(dropOffPos, TKind)
    // also add previous moving piece to the state's blocks
    // the current piece is the newly spawned piece
    s.copy(blocks = s.blocks ++ s.currentPiece.current,
      currentPiece = p)
  }

  private[this] def transit(trans: Piece => Piece,
                            onFail: GameState => GameState = identity
                             ): GameState => GameState =
    (s: GameState) => {
      val moved = trans(s.currentPiece)
      val newState = s.copy(currentPiece = moved)
      validate(newState) match {
        case Some(ns) => ns
        case None => onFail(s)
      }
  }

  private[this] def validate(s: GameState): Option[GameState] = {
    val size = s.gridSize
    val currentPos = s.currentPiece.current.map(_.pos)

    def inBounds(pos: (Int, Int)): Boolean = {
      pos._1 >= 0 && pos._1 < size._1 &&
        pos._2 >= 0 && pos._2 < size._2
    }
    println(s.blocks)

    if (currentPos.forall(inBounds) &&
      s.blocks.map(_.pos).intersect(currentPos).isEmpty) Some(s)
    else None
  }

  private[this] def unload(p: Piece, bs: Seq[Block]): Seq[Block] = {
    val currentPoss = p.current map {_.pos}
    bs filterNot { currentPoss contains _.pos  }
  }

  private[this] def load(p: Piece, bs: Seq[Block]): Seq[Block] =
    bs ++ p.current
}