package soc.inventory.resources

import soc.inventory._
import soc.inventory.resources._
import org.scalatest.{FunSpec, Matchers}

class SOCPossibleHandsSpec extends FunSpec with Matchers {

  val possHands = SOCPossibleHands.empty
  val resourceSet = CatanResourceSet[Int](3, 2, 0, 0, 0)

  describe("handsForPlayers") {

    it ("should return empty map if hands are all empty") {
      possHands.handsForPlayers shouldBe empty
    }

    it("should return a nonempty map if a resource set is gained") {
      val gain = possHands.calculateHands(List(Gain(0, resourceSet)))
      gain.handsForPlayers should not be empty
    }

    it("stealing when player contains two types of cards should result in 3 possible hand") {
      val steal = possHands.calculateHands(List(Gain(0, resourceSet), Steal(1, 0, None), Steal(1, 0, None)))
      val expectedStolen = Seq(CatanResourceSet(2, 0, 0, 0, 0), CatanResourceSet(1,1,0,0,0), CatanResourceSet(0,2,0,0,0))
      steal.handsForPlayers(0).map(_._1) should contain only expectedStolen.map(s => resourceSet.subtract(s))
      steal.handsForPlayers(1).map(_._1) should contain only expectedStolen
    }

  }

  describe("toProbableHands") {

    it("should return empty map if hands are all empty") {
      possHands.probableHands shouldBe empty
    }

    it("should return a nonempty map if a resource set is gained") {
      val gain = possHands.calculateHands(List(Gain(0, resourceSet)))
      gain.probableHands should not be empty
    }

    it("should calculate probable hands from stolen hands") {
      val steal = possHands.calculateHands(List(Gain(0, resourceSet), Steal(1, 0, None), Steal(1, 0, None)))
      val probableHands = steal.probableHands

    }
  }

  describe ("playerGainCards") {

    describe("should add player and player's hand") {

      it("when all hands are empty") {
        val handsWithPlayer = possHands.playerGainCards(0, resourceSet)
        handsWithPlayer.hands should have length 1
        handsWithPlayer.hands.head should contain only (0 -> (resourceSet, 1))
      }



    }

  }

  describe("playerLoseCards") {

  }

  describe("stealUnknownCards") {

  }





}
