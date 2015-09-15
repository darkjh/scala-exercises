package poker

import scala.util.Random


/**
 * A simple poker program
 * Udacity - Design of computer programs
 * Unit 1
 *
 * Hand representation:
 *   - list of 5 tuples of format (number, suit)
 * Hand ranking (http://en.wikipedia.org/wiki/List_of_poker_hands):
 *   - Straight flush
 *   - Four of a kind
 *   - Full house (3 + 2)
 *   - Flush
 *   - Three of a kind
 *   - Two pair
 *   - One pair
 *   - High card
 * Hand suits:
 *   - Spades, Hearts, Diamonds, Clubs
 */
object Poker extends App {
  val myDeck = (for {
    r <- "23456789TJQKA"
    s <- "SHDC"
  } yield r.toString+s.toString).toList

  lazy val numberLookUp = "__23456789TJQKA"
  /** parse a hand string and return a list of pair (number, suit) */
  def parse(hand: String): List[(Int,Char)] = {
    hand.split(" ").map(
      (str: String) => (numberLookUp.indexOf(str(0)), str(1))
    ).toList
  }

  def group(items: List[Int]): List[(Int,Int)] = {
    (for {
      i: Int <- items.toSet.toList
    } yield (items.count(_ == i), i)).sorted.reverse
  }

  type Rank = (Int, Seq[Int])

  /**
   * Given a hand, return its rank
   * E.g. '7 T 7 9 7' => counts=(3, 1, 1), ranks=(7, 10, 9) => (3, ranks)
   *
   * @param hand hand representation in String
   */
  def rank(hand: String): Rank = {
    val (numbers, suits) = parse(hand).unzip
    var (counts, ranks) = group(numbers).unzip

    if (ranks == List(14,5,4,3,2))
      ranks = List(5,4,3,2,1)

    lazy val straight =
      ranks.max - ranks.min == 4 && ranks.toSet.size == 5
    lazy val flush =
      suits.toSet.size == 1

    (counts match {
      case List(5) => 9
      case _ if straight && flush => 8
      case List(4, 1) => 7
      case List(3, 2) => 6
      case _ if flush => 5
      case _ if straight => 4
      case List(3, 1, 1) => 3
      case List(2, 2, 1) => 2
      case List(2, 1, 1, 1) => 1
      case _ => 0
    }, ranks)
  }

  object IntSeqOrdering extends Ordering[Seq[Int]] {
    def compare(x: Seq[Int], y: Seq[Int]): Int = {
      if (x.isEmpty && y.isEmpty) {
        0
      } else if (x.head == y.head) {
        compare(x.tail, y.tail)
      } else {
        x.head - y.head
      }
    }
  }

  object RankOrdering extends Ordering[Rank] {
    def compare(x: Poker.Rank, y: Poker.Rank): Int = {
      if (x._1 == y._1) {
        IntSeqOrdering.compare(x._2, y._2)
      } else {
        x._1 - y._1
      }
    }
  }

  /**
   * Play poker
   *
   * @param hands hands of players
   * @return the max hand/hands
   */
  def poker(hands: List[String]) = {
    def findAllMax(hands: Seq[String]) = {
      val ranks = hands.map(rank)
      val handsWithRank = hands.zip(ranks)
      val sorted = handsWithRank.sortBy(_._2)(RankOrdering.reverse)
      val maxRank = sorted.head._2
      sorted.takeWhile(_._2 == maxRank).unzip._1
    }

    findAllMax(hands)
  }

  // Main
  val player = 4
  val hands = Random.shuffle(myDeck).take(player*5).grouped(5)
    .map(_.mkString(" ")).toList
  println("Hands: "+hands)
  println("Winner: "+poker(hands))
}

