package com.aahsk.homeworks.basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BasicsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "gcd" should "be correct for arbitrarily chosen test values" in {
    gcd(0, 0) shouldEqual 0
    gcd(2, 4) shouldEqual 2
    gcd(-2, -4) shouldEqual 2
    gcd(543326, 124166) shouldEqual 14
  }

  "lcm" should "be correct for arbitrarily chosen test values" in {
    lcm(21, 6) shouldEqual 42
    lcm(8, 21) shouldEqual 168
    lcm(124125, 12312) shouldEqual 509409000
  }
}
