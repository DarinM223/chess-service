import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.Router
import tsec.authentication._
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256

class Service(jwtAuth: JWTAuthenticator[IO, Int, User, HMACSHA256],
              authHandler: SecuredRequestHandler[IO, Int, User, AugmentedJWT[HMACSHA256, Int]],
              auth: Auth,
              userRepo: UserRepository[IO])
             (implicit timer: Timer[IO], effect: ConcurrentEffect[IO]) {
  val nonAuthService = HttpRoutes.of[IO] {
    case req @ POST -> Root / "login" / IntVar(id) =>
      req.decode[UrlForm] { form =>
        val password = form.getFirstOrElse("password", "")
        for {
          user  <- userRepo.getUser(id)
          check <- auth.checkPassword(user, password)
          resp  <- if (check) {
            jwtAuth.create(id).flatMap(jwt => Ok(JWTMac.toEncodedString[IO, HMACSHA256](jwt.jwt)))
          } else {
            NotFound()
          }
        } yield resp
      }
  }

  val userService = HttpRoutes.of[IO] {
    case req @ POST -> Root / "users" / "create" =>
      req.decode[UrlForm] { form =>
        (form.getFirst("username"), form.getFirst("password")) match {
          case (Some(username), Some(password)) =>
            userRepo.createUser(username, password).flatMap(id => Ok(id.toString))
          case _ => NotFound()
        }
      }
  }

  val gameAuthService = authHandler.liftService(TSecAuthService {
    case req @ GET -> Root / IntVar(gameId) asAuthed user =>
      // TODO(DarinM223): return game data, and if user is part of game, information about whose turn is it.
      ???
    case req @ POST -> Root / IntVar(gameId) asAuthed user =>
      // TODO(DarinM223): if user is part of game and it is user's turn, then apply user action, if it is valid.
      ???
    case req @ POST -> Root / "create" asAuthed user =>
      val r: SecuredRequest[IO, User, AugmentedJWT[HMACSHA256, Int]] = req
      r.request.decode[UrlForm] { form =>
        (form.getFirst("game-id"), form.getFirst("other-user-id")) match {
          case (Some(gameId), Some(otherUserId)) => ???
          case _ => NotFound()
        }
      }
      ???
  })

  val authService = authHandler.liftService(TSecAuthService {
    case GET -> Root / "hello" asAuthed user => Ok(user.toString)
  })

  val services = nonAuthService <+> userService <+> authService <+> gameAuthService
  val httpApp = Router("/" -> services).orNotFound

  def runServer: IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
