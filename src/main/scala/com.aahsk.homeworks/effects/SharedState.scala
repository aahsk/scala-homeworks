package com.aahsk.homeworks.effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 *
 * If we will put a value with the same key then it should renew expiration
 */
object SharedState extends IOApp {

  trait Cache[F[_], K, V] {
    def keys: F[List[K]]

    def get(key: K): F[Option[V]]

    def modify[Z](key: K, modifier: Option[V] => (V, Z)): F[Z]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_]: Clock: Monad, K, V](
      stateref: Ref[F, Map[K, (Long, V)]],
      expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {
    def keys: F[List[K]] = stateref.get.map(_.keySet.toList)

    def get(key: K): F[Option[V]] = stateref.get.map(_.get(key).map { case (_, value) => value })

    def modify[Z](key: K, modifier: Option[V] => (V, Z)): F[Z] =
      Clock[F]
        .realTime(MILLISECONDS)
        .flatMap(now =>
          stateref.modify(cachemap => {
            val recordModification: (V, Z) =
              modifier(cachemap.get(key).map { case (_, value) => value })
            val recordNew      = recordModification._1
            val recordLeftover = recordModification._2

            (cachemap.updated(key, (now + expiresIn.toMillis, recordNew)), recordLeftover)
          })
        )

    def put(key: K, value: V): F[Unit] =
      Clock[F]
        .realTime(MILLISECONDS)
        .flatMap(now =>
          stateref.getAndUpdate(_.updated(key, (now + expiresIn.toMillis, value))).void
        )
  }

  object Cache {
    def of[F[_]: Clock: Monad, K, V](
        expiresIn: FiniteDuration,
        checkExpirationsOnEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      // Update a given state by removing records whose expiration date is in the past
      def updatedWithoutOlds(now: Long, state: Map[K, (Long, V)]): Map[K, (Long, V)] =
        state.mapFilter {
          case (expiration, _) if expiration < now => None
          case record                              => Some(record)
        }

      // Sleep a little then check the given state for stale records and remove them
      def sleepyExpirationCheck(
          stateRef: Ref[F, Map[K, (Long, V)]]
      ): F[Unit] =
        for {
          _   <- T.sleep(checkExpirationsOnEvery)
          now <- Clock[F].realTime(MILLISECONDS)
          _   <- stateRef.getAndUpdate(state => updatedWithoutOlds(now, state))
        } yield ()

      for {
        // Create a state reference
        stateref <- Ref.of(Map[K, (Long, V)]())
        // Monitor expired state records
        _ <- C.start(sleepyExpirationCheck(stateref).foreverM.void)
        // Create a cache reference with the aforementioned state
        cacheref = new RefCache(stateref, expiresIn)
      } yield cacheref
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _     <- cache.put(1, "Hello")
      _     <- cache.put(2, "World")
      _ <-
        cache
          .get(1)
          .flatMap(s =>
            IO {
              println(s"first key $s")
            }
          )
      _ <-
        cache
          .get(2)
          .flatMap(s =>
            IO {
              println(s"second key $s")
            }
          )
      _ <- IO.sleep(12.seconds)
      _ <-
        cache
          .get(1)
          .flatMap(s =>
            IO {
              println(s"first key $s")
            }
          )
      _ <-
        cache
          .get(2)
          .flatMap(s =>
            IO {
              println(s"second key $s")
            }
          )
      _ <- IO.sleep(12.seconds)
      _ <-
        cache
          .get(1)
          .flatMap(s =>
            IO {
              println(s"first key $s")
            }
          )
      _ <-
        cache
          .get(2)
          .flatMap(s =>
            IO {
              println(s"second key $s")
            }
          )
    } yield ExitCode.Success
  }
}
