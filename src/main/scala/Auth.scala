import cats.effect._
import tsec.passwordhashers._
import tsec.passwordhashers.jca._

class Auth(implicit P: PasswordHasher[IO, BCrypt]) {
  def createPassword(password: String): IO[String] = BCrypt.hashpw(password).map(hash => hash.toString)
  def checkPassword(user: User, password: String): IO[Boolean] = {
    val hash = PasswordHash[BCrypt](user.hashedPassword)
    BCrypt.checkpwBool(password, hash)
  }
}
