import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.Router
import tsec.authentication._
import tsec.jws.mac.{JWSMacCV, JWTMac}
import tsec.jwt.algorithms.JWTMacAlgo
import tsec.mac.jca.HMACSHA256
import scala.concurrent.duration._

object AugmentedJWTOps {
  implicit class ToResponseCookie[A, I](jwt: AugmentedJWT[A, I]) {
    def toResponseCookie[F[_]]
        (settings: TSecCookieSettings)
        (implicit F: Sync[F], J: JWSMacCV[F, A], algo: JWTMacAlgo[A])
        : ResponseCookie =
      ResponseCookie(
        settings.cookieName,
        JWTMac.toEncodedString[F, A](jwt.jwt),
        expires = Some(HttpDate.unsafeFromInstant(jwt.expiry)),
        None,
        settings.domain,
        settings.path,
        settings.secure,
        settings.httpOnly,
        settings.extension
      )
  }
}

class Service(jwtAuth: JWTAuthenticator[IO, Int, User, HMACSHA256],
              authHandler: SecuredRequestHandler[IO, Int, User, AugmentedJWT[HMACSHA256, Int]],
              auth: Auth,
              userRepo: UserRepository[IO])
             (implicit timer: Timer[IO], effect: ConcurrentEffect[IO]) {
  import AugmentedJWTOps._

  val cookieSettings = TSecCookieSettings(secure = true, expiryDuration = 30.minutes, maxIdle = None)
  val nonAuthService = HttpRoutes.of[IO] {
    case req @ POST -> Root / "login" / IntVar(id) =>
      req.decode[UrlForm] { form =>
        val password = form.getFirstOrElse("password", "")
        for {
          user  <- userRepo.getUser(id)
          check <- auth.checkPassword(user, password)
          resp  <- if (check) {
            jwtAuth.create(id).flatMap(jwt => Ok("").map(_.addCookie(jwt.toResponseCookie(cookieSettings))))
          } else {
            NotFound()
          }
        } yield resp
      }
  }

  val userRoutes = HttpRoutes.of[IO] {
    case req @ POST -> Root / "users" / "create" =>
      req.decode[UrlForm] { form =>
        (form.getFirst("username"), form.getFirst("password")) match {
          case (Some(username), Some(password)) =>
            userRepo.createUser(username, password).flatMap(id => Ok(id.toString))
          case _ => NotFound()
        }
      }
  }

  val authService = authHandler.liftService(TSecAuthService {
    case request @ GET -> Root / "hello" asAuthed user =>
      val r: SecuredRequest[IO, User, AugmentedJWT[HMACSHA256, Int]] = request
      Ok(user.toString)
  })

  val services = nonAuthService <+> userRoutes <+> authService
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
