package com.d_m.Models

import cats.Monad
import cats.implicits._
import com.d_m.Board.Board
import doobie.implicits._
import doobie.util.transactor.Transactor

case class Game(id: Long,
                createdID: Long,
                otherID: Long,
                currTurnID: Option[Long],
                board: String)

object Game {
  import io.circe._
  import io.circe.syntax._

  implicit val gameEncoder = new Encoder[Game] {
    override def apply(game: Game): Json = Json.obj(
      ("id", Json.fromLong(game.id)),
      ("createdID", Json.fromLong(game.createdID)),
      ("otherID", Json.fromLong(game.otherID)),
      ("currTurnID", game.currTurnID.asJson),
      ("board", Json.fromString(game.board))
    )
  }
}

trait GameRepository[F[_]] {
  def getGame(id: Long): F[Game]
  def createGame(userID: Long, otherID: Long): F[Long]
  def nextTurn(game: Game, updatedBoard: String): F[Unit]
}

class GameRepositoryImpl[F[_]: Monad](xa: Transactor[F]) extends GameRepository[F] {
  override def getGame(id: Long): F[Game] =
    sql"""
          SELECT id, created_id, other_id, curr_turn_id, board
          FROM games
          WHERE id = $id
      """.query[Game].unique.transact(xa)

  override def createGame(userID: Long, otherID: Long): F[Long] = {
    val board = Board.init
    val currTurn = otherID
    val insertAndGetID = for {
      _ <- sql"""
                INSERT INTO games (created_id, other_id, curr_turn_id, board)
                VALUES ($userID, $otherID, $currTurn, $board)
             """.update.run
      id <- sql"SELECT lastval()".query[Long].unique
    } yield id

    insertAndGetID.transact(xa)
  }

  override def nextTurn(game: Game, updatedBoard: String): F[Unit] = {
    val next = if (game.currTurnID.contains(game.createdID)) game.otherID else game.createdID
    sql"""
          UPDATE games
          SET curr_turn_id = $next, board = $updatedBoard
          WHERE id = ${game.id}
      """.update.run.transact(xa).as(())
  }
}
