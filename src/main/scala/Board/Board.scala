package Board

import cats.implicits._

sealed trait PieceType
case object Pawn extends PieceType
case object Knight extends PieceType
case object Bishop extends PieceType
case object Rook extends PieceType
case object Queen extends PieceType
case object King extends PieceType

case class Piece(pieceType: PieceType, white: Boolean)

sealed trait BoardError

case class Board(cells: Vector[Vector[Option[Piece]]],
                 kingWhite: (Int, Int),
                 kingBlack: (Int, Int))

object Board {
  val init: String = "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR"

  val checkTest: String =
    " Q      \n     pk \n  p   p \n pbbN  p\n    n  P\n        \nr     P \n      K "
  val checkmateTest: String =
    " Q      \n     pk \n  p   p \n p  N  p\n b     P\n bn     \n  r   P \n  K     "

  def fromString(s: String): Option[Board] = {
    def toPiece(ch: Char): Option[Piece] = ch match {
      case ' ' => None
      case 'P' => Some(Piece(Pawn, true))
      case 'p' => Some(Piece(Pawn, false))
      case 'N' => Some(Piece(Knight, true))
      case 'n' => Some(Piece(Knight, false))
      case 'B' => Some(Piece(Bishop, true))
      case 'b' => Some(Piece(Bishop, false))
      case 'R' => Some(Piece(Rook, true))
      case 'r' => Some(Piece(Rook, false))
      case 'Q' => Some(Piece(Queen, true))
      case 'q' => Some(Piece(Queen, false))
      case 'K' => Some(Piece(King, true))
      case 'k' => Some(Piece(King, false))
      case _   => None
    }

    val pieceIndexes: Seq[(Int, Int)] = for {
      row <- Seq.range(0, 8)
      col <- Seq.range(0, 8)
    } yield (row, col)

    def findPiece(piece: Char): Option[(Int, Int)] =
      s.filter(_ != '\n')
        .zip(pieceIndexes)
        .find { case (c, _) => c == piece }
        .map(_._2)

    val cells = s.split('\n').map(row => row.map(toPiece).to[Vector]).to[Vector]
    (Some(cells), findPiece('K'), findPiece('k')).mapN(Board(_, _, _))
  }

  def checkEnd(board: Board): Option[Boolean] = {
    import BoardChecker._
    val blackAdjs = adjs(board.kingBlack).filter(isValidPos)
    val whiteAdjs = adjs(board.kingWhite).filter(isValidPos)

    if (inCheck(board, board.kingBlack, false) &&
        blackAdjs.count(inCheck(board, _, false)) == blackAdjs.length) {
      Some(false)
    } else if (inCheck(board, board.kingWhite, true) &&
               whiteAdjs.count(inCheck(board, _, true)) == whiteAdjs.length) {
      Some(true)
    } else {
      None
    }
  }

  def applyMove(board: Board, move: String): Either[BoardError, Board] = ???

  def main(args: Array[String]) = {
    val board = Board.fromString(Board.checkmateTest)
    println(board.flatMap(Board.checkEnd(_)))
  }
}
