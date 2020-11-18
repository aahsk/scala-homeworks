package com.aahsk.homeworks.http

import cats.{Applicative, Defer, Monad}
import org.http4s.{HttpRoutes, QueryParamDecoder, Request, Response}
import org.http4s.dsl.io._
import org.http4s.server.blaze.BlazeServerBuilder
import cats.data.Kleisli
import com.aahsk.homeworks.effects.SharedState.Cache
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Sync, Timer}
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.

case class Meta(host: String, port: Int)

case class Game(
    min: Int,
    max: Int,
    guessesAllowed: Int,
    answer: Int,
    guessesAttempted: Int = 0,
    finished: Boolean = false,
    won: Boolean = false
)

object Game {
  def of(min: Int, max: Int, guessesAllowed: Int): Game = {
    Game(min, max, guessesAllowed, Random.between(min, max))
  }
}

case class Lobby[F[_]: Monad](cache: Cache[F, String, Option[Game]]) {
  def keys: F[List[String]]              = cache.keys
  def get(name: String): F[Option[Game]] = cache.get(name).map(_.flatten)
  def modify[Z](name: String, modifier: Option[Game] => (Option[Game], Z)): F[Z] =
    cache.modify(name, maybeGame => modifier(maybeGame.flatten))
  def put: (String, Option[Game]) => F[Unit] = cache.put
}

case class State[F[_]](meta: Meta, lobby: Lobby[F]) {}

object State {

  /**
    * Author's note: I would've liked to have the Monad as a context bound not an implicit parameter
    * however then I don't know how to lift State into the functor
    * implicitly[F] is syntactically wrong, but implicitly[F[State[F]] doesn't resolve :/
    */
  def of[F[_]: Clock: Timer: Concurrent](
      meta: Meta
  )(implicit M: Monad[F]): F[State[F]] = {
    for {
      lobbyCache <- Cache.of[F, String, Option[Game]](40.minutes, 1.minute)
      lobby      <- M.pure(Lobby[F](lobbyCache))
      state      <- M.pure(State[F](meta, lobby))
    } yield state
  }
}

/**
  * Author's note: I would've gladly written this also with tagless final (such a weird name)
  * but for some reason Ok seems to forcibly return IO here :/
  */
case class GuessApp(state: State[IO]) {
  private val metaRoutes = {
    HttpRoutes.of[IO] {
      case GET -> Root / "ping" => Ok("pong")
      case GET -> Root / "help" =>
        Ok(
          "To read more about the game, read " +
            "https://evolutiongaming.slack.com/archives/G01AYKVSSS0/p1605209498124200"
        )
      case GET -> Root / "info" =>
        Ok(state.meta.toString)
    }
  }

  object MinMatcher      extends QueryParamDecoderMatcher[Int]("min")
  object MaxMatcher      extends QueryParamDecoderMatcher[Int]("max")
  object AttemptsMatcher extends QueryParamDecoderMatcher[Int]("attempts")
  object GuessMatcher    extends QueryParamDecoderMatcher[Int]("guess")
  private val gameRoutes = {
    HttpRoutes.of[IO] {
      case GET -> Root / "game" / "create" / name
          :? MinMatcher(min)
          +& MaxMatcher(max)
          +& AttemptsMatcher(attempts) =>
        state.lobby
          .modify(
            name,
            {
              case Some(oldGame) =>
                (Some(oldGame), BadRequest("A game by that name already exists!"))
              case None => (Some(Game.of(min, max, attempts)), Ok("Your game is ready!"))
            }
          )
          .flatten

      case GET -> Root / "game" / "play" / name :? GuessMatcher(guess) =>
        state.lobby
          .modify(
            name,
            {
              case None => (None, BadRequest("No such game exists!"))
              case Some(game) if game.finished =>
                (
                  Some(game),
                  BadRequest(
                    s"This game has already been ${if (game.won) { "won" }
                    else { "lost" }}."
                  )
                )
              case Some(game) if guess == game.answer =>
                (
                  Some(
                    game.copy(
                      finished = true,
                      won = true,
                      guessesAttempted = game.guessesAttempted + 1
                    )
                  ),
                  Ok("Correct guess! You've won this game.")
                )
              case Some(game) if guess != game.answer =>
                val newGame = game.copy(
                  finished = (game.guessesAttempted + 1) == game.guessesAllowed,
                  won = false,
                  guessesAttempted = game.guessesAttempted + 1
                )
                val message = if (newGame.finished) {
                  "Incorrect guess! You've lost!"
                } else {
                  "Incorrect guess! Try again!"
                }
                println(game, newGame)
                (
                  Some(newGame),
                  Ok(message)
                )
            }
          )
          .flatten

      case GET -> Root / "game" / "list" =>
        state.lobby.keys.flatMap(keys => Ok(s"Games:\n\n${keys.mkString("\n")}"))
    }
  }

  private[http] val httpApp = {
    metaRoutes <+> gameRoutes
  }.orNotFound

  def self: Kleisli[IO, Request[IO], Response[IO]] = httpApp
}

object GuessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      meta  <- IO.pure(Meta("localhost", 9001))
      state <- State.of(meta)
      app   <- IO.pure(GuessApp(state))
      server <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9001, host = "localhost")
          .withHttpApp(app.self)
          .serve
          .compile
          .drain
          .as(ExitCode.Success)
    } yield server
  }
}

object GuessClient {
  // ...
}
