package com.aahsk.homeworks.async

import java.net.URL
import java.util.concurrent.Executors

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object Async extends App {
  private implicit val ec: ExecutionContext =
    ExecutionContext.Implicits.global
//    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  // put your code there
  // ^ Note: I'm a rebel and I put my code at the bottom, sorry for the inconvenience

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] =
    Future {
      val linkPattern = """href="(http[^"]+)"""".r
      linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
    }

  private def getLinkServers(links: List[String]): Future[List[String]] = {
    links
      .map(fetchServerName)
      .foldLeft(Future.successful(List[String]()))(
        (acc: Future[List[String]], f: Future[Option[String]]) => {
          for {
            names     <- acc
            maybeName <- f
          } yield maybeName match {
            case None       => names
            case Some(name) => names :+ name
          }
        }
      )
      .map(_.toSet.toList)
  }

  type Url             = String
  type Error           = String
  type FutureEither[A] = EitherT[Future, Error, A]
  type ErrorOr[A]      = Either[Error, A]

  def parse(args: Array[String]): Either[Error, Url] =
    args.toList match {
      case url :: Nil => Right(url)
      case _          => Left("Invalid argument count")
    }

  def render(serverNames: List[String]): String =
    ("Server names:" :: serverNames.sorted).mkString("\n")

  val promise: EitherT[Future, Error, String] = for {
    url     <- EitherT.fromEither[Future](parse(args))
    body    <- EitherT.right(fetchPageBody(url))
    links   <- EitherT.right(findLinkUrls(body))
    servers <- EitherT.right(getLinkServers(links))
  } yield render(servers)

  val result = Try(Await.result[Either[Error, String]](promise.value, Duration.Inf))

  result match {
    case Success(Right(result)) => println("Success: " + result)
    case Success(Left(error))   => println("Error: " + error)
    case Failure(error)         => println("Failure: " + error)
  }
}
