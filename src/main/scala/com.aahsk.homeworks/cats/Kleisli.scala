package com.aahsk.homeworks.cats

import java.net.URL
import cats.implicits._
import cats.data.{Kleisli, Reader}

/**
  * Redis database for caching solved poker boards
  */
case class RedisConfig(url: String, user: String, pass: String)
trait Redis {
  val config: RedisConfig
}
object Redis {
  private def dnsResolveRedisUrl(redisConfig: RedisConfig): Option[URL] = ???
  private def isRedisUp(user: String, pass: String, url: URL): Boolean  = ???

  /**
    * We define this function for smart-constructing a Redis instance
    * And because it's wrapped in a Kleisli - it can be easily compo
    */
  val fromRedisConfig: Kleisli[Option, RedisConfig, Redis] =
    Kleisli((redisConfig: RedisConfig) => {
      // Let's try to resolve the dns address into an IP address using DNS
      // Somewhat nonsensical - no one would manually do this, but for the
      // sake of the exercise, let's just do something here
      val address = dnsResolveRedisUrl(redisConfig)
      if (address.isEmpty) None

      // Check if redis is working by connecting to it once
      val available = isRedisUp(redisConfig.user, redisConfig.pass, address.get)
      if (!available) None

      // If all is good - config validations check out, return configured Redis
      Some(new Redis { val config: RedisConfig = redisConfig })
    })
}

/**
  * Solver for actually solving poker boards
  */
case class SolverConfig(isOmaha: Boolean, playersWithCheatsAllowed: List[String])
trait Solver {
  val config: SolverConfig
}
object Solver {
  val fromSolverConfig: Kleisli[Option, SolverConfig, Solver] =
    Kleisli((solverConfig: SolverConfig) => {
      // I'm ok with cheaters, but there's this really mean guy
      // Joshua, he's banned from my cheater whitelist - he can't be
      // configured as a cheater, because I said so
      if (solverConfig.playersWithCheatsAllowed.contains("Joshua")) None

      // Return configured solver
      Some(new Solver { val config: SolverConfig = solverConfig })
    })
}

/**
  * Parser for unserializing user input into a poker board
  */
// If rankFirst is true then cards are "5s" - five of spades, otherwise "s5" - spades five
case class ParserConfig(trimWhitespace: Boolean, rankFirst: Boolean)
trait Parser {
  val config: ParserConfig
}
object Parser {

  /**
    * This parser constructor is of Reader type, which represents simply a function
    * that reads (and doesn't modify) a parameter
    *
    * No Kleisli of Option here, just passing along the parser
    */
  val fromParserConfig: Reader[ParserConfig, Parser] =
    Reader((parserConfig: ParserConfig) => {
      new Parser { val config: ParserConfig = parserConfig }
    })
}

case class AppConfig(
    redisConfig: RedisConfig,
    solverConfig: SolverConfig,
    parserConfig: ParserConfig
)
class App(redis: Redis, solver: Solver, parser: Parser)
object App {

  /**
    * Super important - notice that this app constructor isn't a function
    * It does _not_ take in an appConfig and pass it into every service that needs it
    * It's three service constructions composed into one
    *
    * - The return type of this is a Kleisli which can be called by providin an AppConfig,
    * - Then it will be fed into redis, which will either fail validation and short-circuit or continue the control flow
    * - The redis kleisli is mapped over using the solver constructor,
    *   this mapping allows re-using that original appConfig and also ensures that errors are short-circuited
    *
    * TL;DR - use of Kleisli allows for easier composition of effectful (option/either/io/future) functions
    * Note: This compiles, but hasn't actually been tested
    */
  def appFromAppConfig: Kleisli[Option, AppConfig, App] =
    for {
      redis  <- Redis.fromRedisConfig.local[AppConfig](_.redisConfig)
      solver <- Solver.fromSolverConfig.local[AppConfig](_.solverConfig)
      parser <- Parser.fromParserConfig.local[AppConfig](_.parserConfig).lift[Option]
    } yield new App(redis, solver, parser)
}
