import cats.Id
import cats.effect._
import doobie._
import tsec.authentication._
import tsec.common.SecureRandomId
import tsec.mac.jca.{HMACSHA256, MacSigningKey}

import scala.concurrent.duration._

object Main extends IOApp {
  // Change password for postgres with:
  //
  // user@computer $ sudo -i -u postgres
  // postgres@computer $ psql
  // postgres=# ALTER USER postgres PASSWORD 'postgres';
  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:chess_service",
    "postgres",
    "postgres"
  )

  // Postgres commands:
  // \c to connect to database
  // \d table to view schema of table
  // \dt to view tables
  // \i to run sql file

  val auth = new Auth()
  val models = new Models(xa, auth)
  val jwtStore = models.dummyBackingStore[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Int]](s => SecureRandomId.coerce(s.id))
  val userStore = models.doobieBackingStore
  val signingKey: MacSigningKey[HMACSHA256] = HMACSHA256.generateKey[Id]
  val jwtStatefulAuth: JWTAuthenticator[IO, Int, User, HMACSHA256] =
    JWTAuthenticator.backed.inBearerToken(
      expiryDuration = 10.minutes,
      maxIdle = None,
      tokenStore = jwtStore,
      identityStore = userStore,
      signingKey = signingKey
    )

  val userRepo = new models.UserRepositoryImpl()
  val authHandler = SecuredRequestHandler(jwtStatefulAuth)
  val service = new Service(jwtStatefulAuth, authHandler, auth, userRepo)

  def run(args: List[String]): IO[ExitCode] = service.runServer
}

