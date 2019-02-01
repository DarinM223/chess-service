package com.d_m.Models

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
  def nextTurn(updatedBoard: String): F[Unit]
}

class GameRepositoryImpl[F[_]](xa: Transactor[F]) extends GameRepository[F] {
  override def getGame(id: Long): F[Game] = ???
  override def createGame(userID: Long, otherID: Long): F[Long] = ???
  override def nextTurn(updatedBoard: String): F[Unit] = ???
}
