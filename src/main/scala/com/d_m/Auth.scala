package com.d_m

import cats.Functor
import cats.implicits._
import com.d_m.Models.User
import tsec.passwordhashers._
import tsec.passwordhashers.jca._

class Auth[F[_]: Functor](implicit P: PasswordHasher[F, BCrypt]) {
  def createPassword(password: String): F[String] =
    BCrypt.hashpw(password).fmap(hash => hash.toString)
  def checkPassword(user: User, password: String): F[Boolean] = {
    val hash = PasswordHash[BCrypt](user.hashedPassword)
    BCrypt.checkpwBool(password, hash)
  }
}
