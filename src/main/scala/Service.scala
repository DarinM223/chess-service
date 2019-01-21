import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.Router
import tsec.authentication.SecuredRequestHandler

class Service[Identity, User, Auth](Auth: SecuredRequestHandler[IO, Identity, User, Auth])
                                   (implicit timer: Timer[IO], effect: ConcurrentEffect[IO]) {
  val helloWorldService = HttpRoutes.of[IO] {
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
