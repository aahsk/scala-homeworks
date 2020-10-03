package com.aahsk.homeworks.basics

import com.aahsk.homeworks.basics.ControlStructures.Command._
import com.aahsk.homeworks.basics.ControlStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ControlStructuresSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "parseCommand" should "instantiate commands properly" in {
    parseCommand("divide 4 5") shouldEqual Right(Divide(4, 5))
    parseCommand("sum 5 5 6 8.5") shouldEqual Right(Sum(List(5, 5, 6, 8.5)))
    parseCommand("average 4 3 8.5 4") shouldEqual Right(Average(List(4, 3, 8.5, 4)))
    parseCommand("min 4 -3 -17") shouldEqual Right(Min(List(4, -3, -17)))
    parseCommand("max 4 -3 -17") shouldEqual Right(Max(List(4, -3, -17)))
  }

  "provided test cases" should "calculate correctly (ignoring formatting)" in {
    process("divide 4 5") shouldEqual ("4.0 divided by 5.0 is 0.8")
    process("sum 5 5 6 8.5") shouldEqual ("The sum of 5.0 5.0 6.0 8.5 is 24.5")
    process("average 4 3 8.5 4") shouldEqual ("The average of 4.0 3.0 8.5 4.0 is 4.875")
    process("min 4 -3 -17") shouldEqual ("The minimum of 4.0 -3.0 -17.0 is -17.0")
    process("max 4 -3 -17") shouldEqual ("The maximum of 4.0 -3.0 -17.0 is 4.0")
  }
}
