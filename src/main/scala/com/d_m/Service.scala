package com.d_m

import cats.effect._
import cats.implicits._
import com.d_m.Board.Board._
import com.d_m.Board.CannotParseMove
import com.d_m.Models.{Game, GameRepository, User, UserRepository}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import tsec.authentication._
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256

import scala.util.Try

class Service(jwtAuth: JWTAuthenticator[IO, Int, User, HMACSHA256],
              authHandler: SecuredRequestHandler[IO, Int, User, AugmentedJWT[HMACSHA256, Int]],
              auth: Auth[IO],
              userRepo: UserRepository[IO],
              gameRepo: GameRepository[IO])
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
    case req @ POST -> Root / "create" =>
      req.decode[UrlForm] { form =>
        (form.getFirst("username"), form.getFirst("password")) match {
          case (Some(username), Some(password)) =>
            userRepo.createUser(username, password).flatMap(id => Ok(id.toString))
          case _ => NotFound()
        }
      }
  }

  val gameAuthService = authHandler.liftService(TSecAuthService {
    case req @ GET -> Root / IntVar(gameId) asAuthed _ =>
      import io.circe.syntax._
      import org.http4s.circe._
      import Game._
      gameRepo.getGame(gameId).flatMap(game => Ok(game.asJson))
    case req @ POST -> Root / IntVar(gameId) asAuthed user =>
      val r: SecuredRequest[IO, User, AugmentedJWT[HMACSHA256, Int]] = req
      gameRepo.getGame(gameId).flatMap { game =>
        if (!game.currTurnID.contains(user.id)) {
          NotFound()
        } else {
          r.request.decode[UrlForm] { form =>
            val white = ???
            val board = for {
              moveStr <- form.getFirst("move").toRight(CannotParseMove(""))
              board   <- fromString(game.board)
              updated <- applyMove(board, moveStr, white)
            } yield updated

            board match {
              case Left(_)      => NotFound()
              case Right(board) => gameRepo.nextTurn(board.pretty).flatMap(_ => Ok())
            }
          }
        }
      }
    case req @ POST -> Root / "create" asAuthed user =>
      val r: SecuredRequest[IO, User, AugmentedJWT[HMACSHA256, Int]] = req
      r.request.decode[UrlForm] { form =>
        form.getFirst("other-user-id").flatMap(s => Try(s.toLong).toOption) match {
          case Some(otherUserId) =>
            gameRepo.createGame(user.id, otherUserId).flatMap(id => Ok(id.toString))
          case _ => NotFound()
        }
      }
  })

  val authService = authHandler.liftService(TSecAuthService {
    case GET -> Root / "hello" asAuthed user => Ok(user.toString)
  })

  val services = nonAuthService <+> authService
  val httpApp = Router(
    "/"      -> services,
    "/games" -> gameAuthService,
    "/users" -> userService
  ).orNotFound

  def runServer: IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
