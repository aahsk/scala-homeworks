package com.aahsk.homeworks.basics

import com.aahsk.homeworks.basics.Traits._
import com.aahsk.homeworks.basics.Objects._
import com.aahsk.homeworks.basics.Ops._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ClassesAndTraitsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "minimumBoundingRectangle" should "work on a set of bounded objects" in {
    val b: Bounded = Set[Bounded](
      Rectangle(0, 0, 2, 2),
      Circle(5, 2, 2),
      Point(-2, -2)
    ).minimumBoundingRectangle()
    // Iffy, because comparing doubles may lead to problems in future
    // if there's more computation involved and precision is lost
    (b.minX, b.maxX, b.minY, b.maxY) shouldEqual (-2.0, 7.0, -2.0, 4.0)
  }
}
