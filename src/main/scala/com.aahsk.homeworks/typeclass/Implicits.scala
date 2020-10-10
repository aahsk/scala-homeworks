package com.aahsk.homeworks.typeclass

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object Implicits {

  /**
    * Lo and behold! Brand new super-useful collection library for Scala!
    *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
    * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
    * of the data stored.
    *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
    * a thing called size score. Its calculation rules:
    * - size score of a Byte is 1
    * - Int - 4 (as primitive JVM int consists of 4 bytes)
    * - Long - 8
    * - Char - 2 (one UTF-16 symbol is 2 bytes)
    * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
    * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
    * the fields
    * - score for any sequence (Array[T], List[T], Vector[T]) is
    * 12 (our old friend object header) + sum of scores of all elements
    * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
    */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T)(implicit
          getSizeScore: GetSizeScore[T]
      ) {
        def sizeScore: SizeScore = getSizeScore.apply(inner)
      }
    }

    /**
      * Mutable key-value cache which limits the size score of the data scored.
      *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
      * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
      * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
      * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
      *
     * @param maxSizeScore max size score for the stored data
      * @tparam K key type
      * @tparam V value type
      */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._
      import instances._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      /**
        * Checks whether enough space is available for an allocation to happen
        */
      private def allocationAvailable(allocation: SizeScore): Boolean = {
        val elementSizeScore = map.sizeScore - mutable.LinkedHashMap.empty[K, V].sizeScore
        val freeSpace        = maxSizeScore - elementSizeScore
        freeSpace >= allocation
      }

      /**
        * Allocates free space in the LinkedHashMap
        *
       * @param allocation - the amount of SizeScore that needs to be allocated
        * @return false if allocation failed, true if allocation succeeded
        */
      @tailrec
      private def cleanGarbageForAllocation(allocation: SizeScore): Boolean = {
        if (allocationAvailable(allocation)) {
          true
        } else {
          map.headOption match {
            case None => false
            case Some((k, _)) =>
              map.remove(k)
              cleanGarbageForAllocation(allocation)
          }
        }
      }

      /**
        * Inserts a key-value pair in cache
        */
      def put(key: K, value: V): Unit = {
        if (!cleanGarbageForAllocation(key.sizeScore + value.sizeScore)) {
          // As they say in Rust world - lets panic!
          throw new Exception("Panic: MutableBoundedCache ran out of memory");
        }
        map.put(key, value)
      }

      /**
        * Retrieves a key-value pair in cache
        */
      def get(key: K): Option[V] = map.get(key)
    }

    /**
      * Cool custom immutable multi-map collection - does not extend the standard library collection types
      * (yes, this is a feature) - aargh!
      */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] =
        PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
      * Type-class allowing us to iterate over different "collection-like" types with one type arg
      */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
      * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
      */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._
      import cats.Foldable
      import cats.Traverse
      import cats.implicits._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      implicit def mapIterate2: Iterate2[Map] =
        new Iterate2[Map] {
          def iterator1[A, B](f: Map[A, B]): Iterator[A] = f.keysIterator
          def iterator2[A, B](f: Map[A, B]): Iterator[B] = f.valuesIterator
        }

      implicit def packedMultiMapIterate2: Iterate2[PackedMultiMap] =
        new Iterate2[PackedMultiMap] {
          def iterator1[A, B](f: PackedMultiMap[A, B]): Iterator[A] = f.inner.map(_._1).iterator
          def iterator2[A, B](f: PackedMultiMap[A, B]): Iterator[B] = f.inner.map(_._2).iterator
        }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      implicit def byteGetSizeScore: GetSizeScore[Byte] = (_: Byte) => 1
      implicit def charGetSizeScore: GetSizeScore[Char] = (_: Char) => 2
      implicit def intGetSizeScore: GetSizeScore[Int]   = (_: Int) => 4
      implicit def longGetSizeScore: GetSizeScore[Long] = (_: Long) => 8
      implicit def traverseGetSizeScore[A[_], B: GetSizeScore](implicit
          traverse: Traverse[A],
          getSizeScore: GetSizeScore[B]
      ): GetSizeScore[A[B]] =
        (xs: A[B]) => {
          12 + traverse.foldLeft(xs, 0)((acc: SizeScore, x: B) => acc + getSizeScore.apply(x))
        }
      implicit def stringGetSizeScore: GetSizeScore[String] =
        (x: String) => x.toList.sizeScore
      implicit def arrayGetSizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] =
        (xs: Array[T]) => 12 + xs.map(_.sizeScore).sum
      implicit def mapGetSizeScore[A: GetSizeScore, B: GetSizeScore]: GetSizeScore[Map[A, B]] =
        (xs: Map[A, B]) => 12 + xs.map { case (a, b) => a.sizeScore + b.sizeScore }.sum
      implicit def mutableLinkedHashMapSizeScore[A: GetSizeScore, B: GetSizeScore]
          : GetSizeScore[mutable.LinkedHashMap[A, B]] =
        (xs: mutable.LinkedHashMap[A, B]) =>
          12 + xs.map { case (a, b) => a.sizeScore + b.sizeScore }.sum
      implicit def packedMultiMapGetSizeScore[A: GetSizeScore, B: GetSizeScore]
          : GetSizeScore[PackedMultiMap[A, B]] =
        (xs: PackedMultiMap[A, B]) =>
          12 + xs.inner.map { case (a, b) => a.sizeScore + b.sizeScore }.sum
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._

    final case class Twit(
        id: Long,
        userId: Int,
        hashTags: Vector[String],
        attributes: PackedMultiMap[String, String],
        fbiNotes: List[FbiNote]
    )

    final case class FbiNote(
        month: String,
        favouriteChar: Char,
        watchedPewDiePieTimes: Long
    )

    trait TwitCache {
      def put(twit: Twit): Long
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = {
      import syntax._
      import instances._
      implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] = (n: FbiNote) =>
        n.month.sizeScore + n.favouriteChar.sizeScore + n.watchedPewDiePieTimes.sizeScore
      implicit val twitGetSizeScore: GetSizeScore[Twit] = (t: Twit) => {
        12 + t.id.sizeScore + t.userId.sizeScore + t.hashTags.sizeScore + t.attributes.sizeScore + t.fbiNotes.sizeScore
      }
      new TwitCache {
        val cache                = new MutableBoundedCache[Long, Twit](maxSizeScore)
        var lastID: Option[Long] = None;
        def generateID(): Long =
          lastID match {
            case None =>
              lastID = Some(1L)
              lastID.get
            case Some(x) =>
              lastID = Some(x + 1L)
              lastID.get
          }
        override def put(twit: Twit): Long = {
          val id = generateID()
          cache.put(id, twit)
          id
        }
        override def get(id: Long): Option[Twit] = cache.get(id)
      }
    }
  }
}
