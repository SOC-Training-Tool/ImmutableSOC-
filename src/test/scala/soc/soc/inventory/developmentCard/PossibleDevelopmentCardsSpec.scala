package soc.soc.inventory.developmentCard

import org.scalatest.{FunSpec, Matchers}
import soc.core.GameRules
import soc.inventory._
import soc.inventory.developmentCard.PossibleDevelopmentCards

class PossibleDevelopmentCardsSpec extends FunSpec with Matchers {

  implicit val gameRules = GameRules.default
  val possibleDevCards = PossibleDevelopmentCards()

  describe("playCard") {

    it("") {
      val newSet = possibleDevCards.playCard(0, 0, Knight)
      newSet.knownCards.getAmount(Knight) shouldBe 1

    }

  }

}
