package poker

import org.scalatest.FunSuite

/**
 * Tests for poker program
 */
class PokerTestSuite extends FunSuite {
  val sf = "6C 7C 8C 9C TC"
  val fk = "9D 9H 9C 9S 7D"
  val fh = "5D 5C 5H 4H 4C"

  test("trivial") {
    assert(2 == 2)
    assert(3 == 3)
  }

  test("parsing") {
    assert(Poker.parse(sf)(0) == (6,'C'))
    assert(Poker.parse(sf)(3) == (9,'C'))
    assert(Poker.parse(sf)(4) == (10,'C'))
  }

  test("group") {
    assert(Poker.group(List(1, 2, 3, 2, 2))
      == List((3,2), (1,3), (1,1)))
  }

  test("rank") {
    assert(Poker.rank(sf)
      == (8, List(10,9,8,7,6)))
    assert(Poker.rank(fk)
      == (7, List(9,7)))
    assert(Poker.rank(fh)
      == (6, List(5,4)))
  }

  test("poker") {
    assert(Poker.poker(List(sf, fk, fh)) == List(sf))
    assert(Poker.poker(List(fk, fh)) == List(fk))
    assert(Poker.poker(List(fh, fh)) == List(fh, fh))
    assert(Poker.poker(List(sf)) == List(sf))
    assert(Poker.poker(List(fk, fh))
      == List.iterate(fk, 100)(_ => fh))
  }
}
