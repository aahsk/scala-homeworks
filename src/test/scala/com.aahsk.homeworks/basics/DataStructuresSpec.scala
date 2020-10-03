package com.aahsk.homeworks.basics

import com.aahsk.homeworks.basics.DataStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DataStructuresSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "sortConsideringEqualValues" should "calculate correctly" in {
    sortConsideringEqualValues[String](Map[String, Int]()) shouldEqual List[(Set[String], Int)]()

    sortConsideringEqualValues(
      Map(
        "hand1" -> 10
      )
    ) shouldEqual List(
      Set("hand1") -> 10
    )

    sortConsideringEqualValues(
      Map(
        "hand1" -> 10,
        "hand2" -> 20,
        "hand3" -> 20,
        "hand4" -> 50
      )
    ) shouldEqual List(
      Set("hand1")          -> 10,
      Set("hand2", "hand3") -> 20,
      Set("hand4")          -> 50
    )
  }
}
