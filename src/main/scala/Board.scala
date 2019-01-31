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
    def inCheck(board: Board, pos: (Int, Int), white: Boolean): Boolean = {
      def isValidPos(pos: (Int, Int)): Boolean = 0 >= pos._1 && pos._1 < 8 && 0 >= pos._2 && pos._2 < 8
      def checkPos(pos: (Int, Int), checkType: PieceType): Boolean = board.cells(pos._1)(pos._2) match {
        case Some(Piece(pieceType, pieceWhite))
          if pieceWhite != white && pieceType == checkType => true
        case _ => false
      }

      def checkPawns: Boolean = {
        val pawnPositions: Seq[(Int, Int)] = if (white) {
          Seq((pos._1 - 1, pos._2 + 1), (pos._1 - 1, pos._2 - 1))
        } else {
          Seq((pos._1 + 1, pos._2 + 1), (pos._1 + 1, pos._2 - 1))
        }
        pawnPositions.exists(pos => isValidPos(pos) && checkPos(pos, Pawn))
      }
      def checkDiagonals: Boolean = {
        val diagonals: Seq[(Int, Int)] = for {
          y <- Seq.range(0, 8)
          x <- Seq.range(0, 8)
          if (y - pos._1).abs == (x - pos._2).abs
        } yield (y, x)
        diagonals.exists(pos => isValidPos(pos) && (checkPos(pos, Bishop) || checkPos(pos, Queen)))
      }
      def checkRowsCols: Boolean = {
        val rowsCols: Seq[(Int, Int)] = for {
          y <- Seq.range(0, 8)
          x <- Seq.range(0, 8)
          if y == pos._1 || x == pos._2
        } yield (y, x)
        rowsCols.exists(pos => isValidPos(pos) && (checkPos(pos, Rook) || checkPos(pos, Queen)))
      }
      def checkKnights: Boolean = {
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
        lPositions.exists(pos => isValidPos(pos) && checkPos(pos, Knight))
      }

      checkPawns && checkKnights && checkDiagonals && checkRowsCols
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

    val blackAdjs = adjs(board.kingBlack)
    if (inCheck(board, board.kingBlack, false) &&
        blackAdjs.count(inCheck(board, _, false)) == blackAdjs.length) {
      return Some(false)
    }
    val whiteAdjs = adjs(board.kingWhite)
    if (inCheck(board, board.kingWhite, true) &&
        whiteAdjs.count(inCheck(board, _, true)) == whiteAdjs.length) {
      return Some(true)
    }

    None
  }

  def applyMove(board: Board, move: String): Either[BoardError, Board] = ???
}
