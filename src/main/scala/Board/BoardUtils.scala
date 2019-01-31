package Board

import cats._
import cats.implicits._

object BoardUtils {
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

  def parsePosition(pos: String): Either[BoardError, (PieceType, (Int, Int))] = ???

  def parseMove(move: String, white: Boolean): Either[BoardError, Move] = move match {
    case "O-O"   => Right(Castle(white, true))
    case "O-O-O" => Right(Castle(white, false))
    case _ if move.split(' ').length == 2 =>
      for {
        List((pieceType, pos1), (_, pos2)) <- move.split(' ').toList
          .traverse[Either[BoardError, ?], (PieceType, (Int, Int))](parsePosition)
      } yield NormalMove(Piece(pieceType, white), pos1, pos2)
    case _ => Left(CannotParseMove(move))
  }

  // TODO(DarinM223): check that move is valid
  def applyMove(board: Board, move: Move): Either[BoardError, Board] = move match {
    case NormalMove(piece, start, end) =>
      for {
        foundPiece <- board.cells(start._1)(start._2).toRight(PieceNotFound(piece))
        _ <- if (piece != foundPiece) Left(DifferentPieces(piece, foundPiece)) else Right()
        updatedCells = board.cells
          .updated(start._1, board.cells(start._1).updated(start._2, None))
          .updated(end._1, board.cells(end._1).updated(end._2, Some(piece)))
        updatedBoard = piece match {
          case Piece(King, true) => board.copy(cells = updatedCells, kingWhite = end)
          case Piece(King, false) => board.copy(cells = updatedCells, kingBlack = end)
          case _ => board.copy(cells = updatedCells)
        }
      } yield updatedBoard
    case Castle(white, kingSide) => {
      val castleRow = if (white) 7 else 0
      val kingCol = 4
      val rookCol = if (kingSide) 7 else 0
      for {
        king <- board.cells(castleRow)(kingCol).toRight(PieceNotFound(Piece(King, white)))
        rook <- board.cells(castleRow)(rookCol).toRight(PieceNotFound(Piece(Rook, white)))
        moveKing = if (kingSide) {
          NormalMove(king, (castleRow, kingCol), (castleRow, kingCol + 2))
        } else {
          NormalMove(king, (castleRow, kingCol), (castleRow, kingCol - 2))
        }
        moveRook = if (kingSide) {
          NormalMove(rook, (castleRow, rookCol), (castleRow, rookCol - 2))
        } else {
          NormalMove(rook, (castleRow, rookCol), (castleRow, rookCol + 3))
        }
        board1 <- applyMove(board, moveKing)
        board2 <- applyMove(board1, moveRook)
      } yield board2
    }
  }
}