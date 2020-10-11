package com.aahsk.homeworks.adt

// Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
// task you completed to join the bootcamp. Use your best judgement about particular data types to include
// in the solution, you can model concepts like:
//
// 1. Suit
// 2. Rank
// 3. Card
// 4. Hand (Texas or Omaha)
// 5. Board
// 6. Poker Combination (High Card, Pair, etc.)
// 7. Test Case (Board & Hands to rank)
// 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
//
// Make sure the defined model protects against invalid data. Use value classes and smart constructors as
// appropriate. Place the solution under `adt` package in your homework repository.
object AlgebraicDataTypes {
  sealed trait Suit
  object Suit {
    final case object Diamond extends Suit
    final case object Spade   extends Suit
    final case object Heart   extends Suit
    final case object Club    extends Suit
  }

  final case class Rank private (value: Int) extends AnyVal
  object Rank {
    def create(value: Int): Option[Rank] =
      if (value >= 1 && value <= 14) {
        Some(Rank(value))
      } else {
        None
      }
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Hand[H <: Hand[H]] {
    val cards: Set[Card]
  }

  final case class TexasHand private (cards: Set[Card]) extends Hand[TexasHand]
  object TexasHand {
    def create(cards: Set[Card]): Option[TexasHand] =
      if (cards.nonEmpty && cards.size <= 2) {
        Some(TexasHand(cards))
      } else {
        None
      }
  }

  final case class OmahaHand private (cards: Set[Card]) extends Hand[OmahaHand]
  object OmahaHand {
    def create(cards: Set[Card]): Option[OmahaHand] =
      if (cards.nonEmpty && cards.size <= 4) {
        Some(OmahaHand(cards))
      } else {
        None
      }
  }

  final case class Board private (cards: Set[Card])
  object Board {
    def create(cards: Set[Card]): Option[Board] =
      if (cards.nonEmpty && cards.size <= 5) {
        Some(Board(cards))
      } else {
        None
      }
  }

  type CombinationScore = Int

  sealed trait Combination {
    val cards: Set[Card]
    val worth: CombinationScore
  }

  object Combination {
    final case class Highcard(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 10
    }
    final case class Pair(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 20
    }
    final case class TwoPairs(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 30
    }
    final case class ThreeOfKind(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 40
    }
    final case class Straight(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 50
    }
    final case class Flush(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 60
    }
    final case class FullHouse(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 70
    }
    final case class FourOfKind(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 80
    }
    final case class StraightFlush(cards: Set[Card]) extends Combination {
      val worth: CombinationScore = 90
    }
  }

  case class AnalysedHand[H <: Hand[H]](hand: Hand[H], combinations: Set[Combination])

  // See test case and test result models in com.aahsk.homeworks/adt/AlgebraicDataTypesSpec.scala
}
