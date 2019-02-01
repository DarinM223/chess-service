package com.d_m.Models

import cats.Monad
import cats.implicits._
import com.d_m.Auth
import doobie.implicits._
import doobie.util.transactor.Transactor

case class User(id: Int, name: String, hashedPassword: String)

trait UserRepository[F[_]] {
  def getUser(id: Long): F[User]
  def createUser(username: String, password: String): F[Long]
}

class UserRepositoryImpl[F[_]: Monad](xa: Transactor[F], auth: Auth[F]) extends UserRepository[F] {
  override def getUser(id: Long): F[User] =
    sql"SELECT id, name, password FROM users WHERE id = $id".query[User].unique.transact(xa)

  override def createUser(username: String, password: String): F[Long] =
    for {
      hashedPassword <- auth.createPassword(password)
      insertAndGetID =
      for {
        _  <- sql"INSERT INTO users (name, password) VALUES ($username, $hashedPassword)".update.run
        id <- sql"SELECT lastval()".query[Long].unique
      } yield id
      id <- insertAndGetID.transact(xa)
    } yield id
}

