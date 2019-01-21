import cats.Id
import cats.effect._
import cats.implicits._
import doobie._
import tsec.authentication._
import tsec.common.SecureRandomId
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import scala.concurrent.duration._

// After running server, try `localhost:8080/hello/foo` and
// `localhost:8080/api/hello/foo`
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

  val jwtStore = Models.dummyBackingStore[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Int]](s => SecureRandomId.coerce(s.id))
  val userStore = Models.doobieBackingStore(xa)
  val signingKey: MacSigningKey[HMACSHA256] = HMACSHA256.generateKey[Id]
  val jwtStatefulAuth =
    JWTAuthenticator.backed.inBearerToken(
      expiryDuration = 10.minutes,
      maxIdle = None,
      tokenStore = jwtStore,
      identityStore = userStore,
      signingKey = signingKey
    )
  val Auth = SecuredRequestHandler(jwtStatefulAuth)
  val service = new Service(Auth)

  def run(args: List[String]): IO[ExitCode] = IO { println("Hello world! ")}.as(ExitCode.Success)
}

