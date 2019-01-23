import cats.implicits._
import cats.effect._
import doobie.util.transactor.Transactor
import tsec.authentication._
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import tsec.passwordhashers._
import tsec.passwordhashers.jca._

object Auth {
  type AuthService = TSecAuthService[User, AugmentedJWT[HMACSHA256, Int], IO]

  def checkPassword(user: User, password: String): Boolean = ???
}
