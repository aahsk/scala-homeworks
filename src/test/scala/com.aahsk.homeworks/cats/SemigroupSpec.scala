package com.aahsk.homeworks.cats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SemigroupSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "semigroups" should "make sense" in {
    import cats.Semigroup
    import cats.implicits._

    Semigroup[Int].combine(1, 2) should be(3)
    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(
      List(1, 2, 3, 4, 5, 6)
    )
    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(
      Some(3)
    )
    Semigroup[Option[Int]].combine(Option(1), None) should be(
      Some(1)
    )

    Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(67)

    val aMap        = Map("foo" -> Map("bar" -> 5))
    val anotherMap  = Map("foo" -> Map("bar" -> 6))
    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

    combinedMap.get("foo") should be(
      Some(
        Map("bar" -> 11)
      )
    )

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int]   = None

    one |+| two should be(
      Some(3)
    )
    n |+| two should be(
      Some(2)
    )
    n |+| n should be(
      None
    )
    two |+| n should be(
      Some(2)
    )

  }
}
