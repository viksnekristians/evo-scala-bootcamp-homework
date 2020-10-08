package adt

object ADTsHomework {

  final case class Suit private (suitValue: Char) extends AnyVal
  object Suit {
    def create(suitValue: Char) : Option[Suit] = {
      suitValue match {
        case 'c' | 'd' | 'h' | 's' => Some(Suit(suitValue))
        case _ => None
      }
    }
  }

  final case class Rank private (rankValue: Int) extends AnyVal // it will be easier later if rank value is passed as an int , not char
  object Rank {
    def create(rankValue: Char) : Option[Rank] = {
      val ranks = List(2,3,4,5,6,7,8,9,10,11,12,13,14)
      if (ranks.contains(rankValue)) Some(Rank(rankValue))
      else None
    }
  }


  final case class Card(suit: Suit, rank: Rank) // by making rank and suit using 'create' we will make sure that the right values are passed here

  final case class Hand private(cards: List[Card])
  object Hand {
    def create(cards: List[Card]): Option[Hand] = {
      cards.length match {
        case 2 => Some(Hand(cards))
        case _ => None
      }
    }
  }

  final case class Board private (cards: List[Card])
  object Board {
    def create(cards: List[Card]): Unit = {
      cards.length match {
        case 5 => Some(Board(cards))
        case _ => None
      }
    }
  }

  final case class GameInstance private (hands: List[Hand] , board: Board)
  object GameInstance {
    def create(hands: List[Hand] , board: Board): Option[GameInstance] = {
      if (hands.nonEmpty && hands.length<10) Some(GameInstance(hands, board)) // max length depends on the table, it is often 9 or 10
      else None
    }
  }


  final case class Combination(hand: Hand, board: Board) {
    //Implement method that gets the best combination for this hand with a specific board and returns a result of a type 'HandResult'
  }



  sealed trait HandResult //need to implement getHandValue method that will return object's value, for example as a List of numbers that can later be compared with other object's
  object HandResult {

    final case class StraightFlush(highest: Rank) extends HandResult

    final case class FourOfAKind(rank: Rank, highestOfOthers: Rank) extends HandResult

    final case class FullHouse(threeOfAKind: ThreeOfAKind, pair: Pair) extends HandResult

    final case class Flush(cards: List[Card]) extends HandResult

    final case class Straight(highest: Rank) extends HandResult

    final case class ThreeOfAKind(rank: Rank, highestOfOthersList: List[Rank]) extends HandResult

    final case class TwoPairs(firstPairRank: Rank, secondPairRank: Rank, highestOfOthers: Rank) extends HandResult

    final case class Pair(pairRank: Rank, highestOfOthersList: List[Rank]) extends HandResult

    final case class HighCard(highestCardRanks: List[Rank]) extends HandResult

  }

  final case class Result(handResults: List[HandResult]) {
    //Implement sorting of hand results
  }

}
