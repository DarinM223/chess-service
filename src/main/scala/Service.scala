import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.Router
import tsec.authentication.{AugmentedJWT, JWTAuthenticator, SecuredRequestHandler}
import tsec.mac.jca.HMACSHA256

class Service(jwtAuth: JWTAuthenticator[IO, Int, User, HMACSHA256],
              auth: SecuredRequestHandler[IO, Int, User, AugmentedJWT[HMACSHA256, Int]],
              userRepo: UserRepository[IO])
             (implicit timer: Timer[IO], effect: ConcurrentEffect[IO]) {
  val helloWorldService = HttpRoutes.of[IO] {
    case req @ POST -> Root / "login" / IntVar(id) =>
      req.decode[UrlForm] { form =>
        val password = form.getFirstOrElse("password", "")
        // TODO(DarinM223): retrieve user with id from database, check that the password
        // matches the bcrypt hash, and if true, generate a JWT with jwtAuth.create.
        val retValue: IO[Response[IO]] = for {
          user <- userRepo.getUser(id)
          jwt  <- jwtAuth.create(id)
          resp <- if (Auth.checkPassword(user, password)) {
            Ok("Hello")
          } else {
            Ok("world")
          }
        } yield resp

        retValue
      }
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }

  val services = helloWorldService
  val httpApp = Router("/" -> helloWorldService, "/api" -> services).orNotFound

  def runServer: IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
