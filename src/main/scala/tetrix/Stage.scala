package tetrix

import scala.annotation.tailrec

object Stage {
  // Initial game state
  def initState(blocks: Seq[Block]): GameState = {
    val size = (10, 20)
    def dropOffPos = (size._1 / 2.0, size._2 - 3.0)
    val p = Piece(dropOffPos, TKind)
    GameState(blocks, size, p, randomStream(new util.Random(0L)))
  }

  def moveLeft = transit(_.moveBy(-1.0, 0.0))
  def moveRight = transit(_.moveBy(1.0, 0.0))
  def rotateClockWise = transit(_.rotateBy(Math.PI / 2.0))

  // move down the current piece
  // if collision, check rows to clear and then spawn a new piece
  val tick = transit(_.moveBy(0.0, -1.0),
    Function.chain(spawn :: clearFullRow :: Nil))

  private[this] def randomStream(random: util.Random): Stream[PieceKind] =
    PieceKind(random.nextInt % 7) #:: randomStream(random)

  private[this] lazy val spawn: GameState => GameState =
    (s: GameState) => {
      def dropOffPos = (s.gridSize._1 / 2.0, s.gridSize._2 - 3.0)
      val p = Piece(dropOffPos, s.kinds.head)
      // also add previous moving piece to the state's blocks
      // the current piece is the newly spawned piece
      val newState = s.copy(blocks = s.blocks ++ s.currentPiece.current,
        currentPiece = p, kinds = s.kinds.tail)

      validate(newState) match {
        case Some(ss) => ss
        case None => initState(Nil)  // game over, restart
      }
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

    if (currentPos.forall(inBounds) &&
      s.blocks.map(_.pos).intersect(currentPos).isEmpty) Some(s)
    else None
  }

  private[this] lazy val clearFullRow: GameState => GameState =
    (s: GameState) => {
      val highestRow = s.blocks.map(_.pos._2).max

      def isFullRow(row: Int, s: GameState): Boolean = {
        s.blocks.count(_.pos._2 == row) == s.gridSize._1
      }

      @tailrec def clearFullRows(row: Int, s: GameState): GameState =
        if (row > highestRow) s
        else if (!isFullRow(row, s)) clearFullRows(row + 1, s)
        else {
          val upper = s.blocks.filter(_.pos._2 > row)
          val lower = s.blocks.filter(_.pos._2 < row)
          val descended =
            upper.map(b => b.copy(pos = (b.pos._1, b.pos._2 - 1)))
          clearFullRows(row, s.copy(blocks = lower ++ descended))
        }

      clearFullRows(0, s)
    }
}