package soc.soc.inventory.developmentCard

import org.scalatest.{FunSpec, Matchers}
import soc.inventory._
import soc.inventory.developmentCard.DevelopmentCardSpecificationSet
import soc.inventory.developmentCard.DevelopmentCardSet._

class DevelopmentCardSpecificationSetSpec extends FunSpec with Matchers {

  val devCardSet = DevelopmentCardSpecificationSet()

  describe("buyCard") {

    it("should add card as unplayed") {
      val newSet = devCardSet.buyCard(0, Knight)
      newSet.isEmpty shouldBe false
      newSet.length shouldBe 1
      newSet.toList should contain only Knight
      newSet.filterPlayed.toList shouldBe empty
      newSet.filterUnPlayed.toList should contain only Knight
      newSet.getAmount(Knight) shouldBe 1
    }

    it("should add 2 card as unplayed") {
      val newSet = devCardSet.buyCard(0, Knight).buyCard(0, Knight)
      newSet.isEmpty shouldBe false
      newSet.length shouldBe 2
      newSet.toList shouldBe List(Knight, Knight)
      newSet.filterPlayed.toList shouldBe empty
      newSet.filterUnPlayed.toList shouldBe List(Knight, Knight)
      newSet.getAmount(Knight) shouldBe 2
    }

  }

  describe ("playCard") {

    it("before buy card should add card directly as played") {
      val newSet = devCardSet.playCard(0, Knight).playCard(0, Knight)
      newSet.isEmpty shouldBe false
      newSet.length shouldBe 2
      newSet.toList shouldBe List(Knight, Knight)
      newSet.filterPlayed.toList shouldBe List(Knight, Knight)
      newSet.filterUnPlayed.toList shouldBe empty
      newSet.getAmount(Knight) shouldBe 2
    }



    it("buy card then play should add card as played and removed card as unplayed") {
      val newSet = devCardSet.buyCard(0, Knight).playCard(1, Knight)
      newSet.isEmpty shouldBe false
      newSet.length shouldBe 1
      newSet.toList should contain only Knight
      newSet.filterPlayed.toList should contain only Knight
      newSet.filterUnPlayed.toList shouldBe empty
      newSet.getAmount(Knight) shouldBe 1
    }



  }

  describe("playedCardOnTurn") {
      it("should return true if any card was played on specified turn") {
        val newSet = devCardSet.buyCard(0, Knight).playCard(1, Knight)
        newSet.playedCardOnTurn(1) shouldBe true
      }

      it("should return false if no card was played on specified turn") {
        val newSet = devCardSet.buyCard(0, Knight)
        newSet.playedCardOnTurn(1) shouldBe false
      }
  }

  describe("canPlayCardOnTurn") {

   it ("if set is empty should return false") {
     devCardSet.canPlayCardOnTurn(Knight, 1) shouldBe false
   }

    describe("if set is not empty") {
      it("should return false if card was bought on same turn") {
        val newSet = devCardSet.buyCard(0, Knight)
        newSet.canPlayCardOnTurn(Knight, 0) shouldBe false
      }

      it ("should return true if no card was played or bought on specified turn") {
        val newSet = devCardSet.buyCard(0, Knight)
        newSet.canPlayCardOnTurn(Knight, 1) shouldBe true
      }

      it("should return true if card was bought on earlier turn but also on specified turn") {
        val newSet = devCardSet.buyCard(0, Knight).buyCard(1, Knight)
        newSet.canPlayCardOnTurn(Knight, 1) shouldBe true
      }
    }



  }



}
