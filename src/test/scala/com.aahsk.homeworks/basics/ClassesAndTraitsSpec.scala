package com.aahsk.homeworks.basics

import com.aahsk.homeworks.basics.Traits._
import com.aahsk.homeworks.basics.Objects._
import com.aahsk.homeworks.basics.Manipulations._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ClassesAndTraitsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "sphere area" should "calculate correcly" in {
    val radius = 2
    Sphere(D3(0, 0, 0), radius).area shouldEqual (4 * Math.PI * Math.pow(radius, 2))
  }

  "sphere volume" should "calculate correcly" in {
    val radius = 2
    Sphere(D3(0, 0, 0), radius).volume shouldEqual ((4 / 3) * Math.PI * Math.pow(radius, 3))
  }

  "rectangle bounds" should "calculate correcly" in {
    val rectangle = Rectangle(D2(1, 1), D2(2, 2))
    rectangle.min shouldEqual D2(-1, -1)
    rectangle.max shouldEqual D2(3, 3)
  }

  "circle bounds" should "calculate correcly" in {
    val circle = Circle(D2(5, 2), 2)
    circle.min shouldEqual D2(3, 0)
    circle.max shouldEqual D2(7, 4)
  }

  "point bounds" should "calculate correcly" in {
    val point = Point(D2(-2, -2))
    point.min shouldEqual D2(-2, -2)
    point.max shouldEqual D2(-2, -2)
  }

  "minimumBoundingRectangle" should "work on a set of bounded objects" in {
    val boundedObjects = Set[Bounded[D2]](
      Rectangle(D2(), D2(2, 2)),
      Circle(D2(5, 2), 2),
      Point[D2](D2(-2, -2))
    )
    val b: Bounded[D2] = boundedObjects.minimumBoundingRectangle()
    (b.min.get.x, b.min.get.y, b.max.get.x, b.max.get.y) shouldEqual (-2.0, -2.0, 7.0, 4.0)
  }
}
