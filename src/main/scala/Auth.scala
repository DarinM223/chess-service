import cats.effect._
import tsec.passwordhashers._
import tsec.passwordhashers.jca._

class Auth(implicit val P: PasswordHasher[IO, BCrypt]) {
  def checkPassword(user: User, password: String): IO[Boolean] = {
    val hash = PasswordHash[BCrypt](user.hashedPassword)
    BCrypt.checkpwBool(password, hash)
  }
}
