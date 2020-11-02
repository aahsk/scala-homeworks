package com.aahsk.homeworks.cats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MonoidSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "monoids" should "make sense" in {
    import cats.Monoid
    import cats.implicits._

    Monoid[String].empty should be(
      ""
    )
    Monoid[String].combineAll(List("a", "b", "c")) should be(
      "abc"
    )
    Monoid[String].combineAll(List()) should be(
      ""
    )

    Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(
      Map(
        "a" -> 4,
        "b" -> 2
      )
    )
    Monoid[Map[String, Int]].combineAll(List()) should be(
      Map()
    )

    val l = List(1, 2, 3, 4, 5)
    l.foldMap(identity) should be(
      15
    )
    l.foldMap(i => i.toString) should be(
      "12345"
    )

    l.foldMap(i => (i, i.toString)) should be(
      (15, "12345")
    )
  }
}
