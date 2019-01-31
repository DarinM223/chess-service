package Board

object BoardChecker {
  def isValidPos(pos: (Int, Int)): Boolean = pos._1 >= 0 && pos._1 < 8 && pos._2 >= 0 && pos._2 < 8

  def checkPos(board: Board, pos: (Int, Int), white: Boolean, checkType: PieceType): Boolean =
    board.cells(pos._1)(pos._2) match {
      case Some(Piece(pieceType, pieceWhite))
        if pieceWhite != white && pieceType == checkType => true
      case _ => false
    }

  def checkPawns(board: Board, pos: (Int, Int), white: Boolean): Boolean = {
    val pawnPositions: Seq[(Int, Int)] = if (white) {
      Seq((pos._1 - 1, pos._2 + 1), (pos._1 - 1, pos._2 - 1))
    } else {
      Seq((pos._1 + 1, pos._2 + 1), (pos._1 + 1, pos._2 - 1))
    }
    pawnPositions.exists(pos => isValidPos(pos) && checkPos(board, pos, white, Pawn))
  }

  def checkDiagonals(board: Board, pos: (Int, Int), white: Boolean): Boolean = {
    val diagonals: Seq[(Int, Int)] = for {
      y <- Seq.range(0, 8)
      x <- Seq.range(0, 8)
      if (y - pos._1).abs == (x - pos._2).abs
    } yield (y, x)

    diagonals.exists { pos =>
      isValidPos(pos) && (checkPos(board, pos, white, Bishop) || checkPos(board, pos, white, Queen))
    }
  }

  def checkRowsCols(board: Board, pos: (Int, Int), white: Boolean): Boolean = {
    val rowsCols: Seq[(Int, Int)] = for {
      y <- Seq.range(0, 8)
      x <- Seq.range(0, 8)
      if y == pos._1 || x == pos._2
    } yield (y, x)

    rowsCols.exists { pos =>
      isValidPos(pos) && (checkPos(board, pos, white, Rook) || checkPos(board, pos, white, Queen))
    }
  }

  def checkKnights(board: Board, pos: (Int, Int), white: Boolean): Boolean = {
    val lPositions: Seq[(Int, Int)] = Seq(
      (pos._1 - 1, pos._2 - 2),
      (pos._1 - 1, pos._2 + 2),
      (pos._1 + 1, pos._2 - 2),
      (pos._1 + 1, pos._2 + 2),
      (pos._1 - 2, pos._2 - 1),
      (pos._1 - 2, pos._2 + 1),
      (pos._1 + 2, pos._2 - 1),
      (pos._1 + 2, pos._2 + 1)
    )
    lPositions.exists(pos => isValidPos(pos) && checkPos(board, pos, white, Knight))
  }

  def inCheck(board: Board, pos: (Int, Int), white: Boolean): Boolean = {
    val check = checkPawns(board, pos, white) ||
      checkKnights(board, pos, white) ||
      checkDiagonals(board, pos, white) ||
      checkRowsCols(board, pos, white)
    isValidPos(pos) && check
  }

  def adjs(pos: (Int, Int)): Seq[(Int, Int)] = Seq(
    (pos._1 + 1, pos._2),
    (pos._1 - 1, pos._2),
    (pos._1, pos._2 + 1),
    (pos._1, pos._2 - 1),
    (pos._1 + 1, pos._2 + 1),
    (pos._1 + 1, pos._2 - 1),
    (pos._1 - 1, pos._2 + 1),
    (pos._1 - 1, pos._2 - 1)
  )
}
