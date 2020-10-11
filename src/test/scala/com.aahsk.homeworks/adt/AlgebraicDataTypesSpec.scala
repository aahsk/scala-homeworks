package com.aahsk.homeworks.adt

import com.aahsk.homeworks.adt.AlgebraicDataTypes._
import com.aahsk.homeworks.adt.AlgebraicDataTypes.Combination._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AlgebraicDataTypesSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  val d: Suit                  = Suit.Diamond;
  val h: Suit                  = Suit.Heart;
  val c: Suit                  = Suit.Club;
  val s: Suit                  = Suit.Spade;
  def c(r: Int, s: Suit): Card = Card(Rank.create(r).get, s)

  "Poker test case" should "be constructable with the given algebraic data types" in {
    val board = Board.create(Set(c(2, h), c(3, h), c(4, h), c(5, d), c(8, d))).get
    val hand1 = TexasHand.create(Set(c(13, d), c(13, s))).get
    val hand2 = TexasHand.create(Set(c(9, h), c(10, h))).get

    val hand1Combinations: Set[Combination] = Set(Pair(Set(c(13, d), c(13, s))))
    val hand2Combinations: Set[Combination] =
      Set(Flush(Set(c(2, h), c(3, h), c(4, h), c(9, h), c(10, h))))

    val expectedEngineAnswer: Map[CombinationScore, Set[AnalysedHand[TexasHand]]] = Map(
      20 -> Set(AnalysedHand(hand1, hand1Combinations)),
      60 -> Set(AnalysedHand(hand2, hand2Combinations))
    )
//    val calculatedEngineAnswer: Map[CombinationScore, Set[AnalysedHand[TexasHand]]] = ???
//    calculatedEngineAnswer shouldEqual expectedEngineAnswer
  }
}
