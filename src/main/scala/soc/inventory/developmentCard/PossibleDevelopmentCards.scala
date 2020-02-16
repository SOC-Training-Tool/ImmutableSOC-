package soc.inventory.developmentCard

import soc.core.GameRules
import soc.inventory.DevelopmentCard
import soc.inventory.developmentCard.DevelopmentCardSet._

case class PossibleDevCardsHands(
  knownDevCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet(),
  numUnknownDevCards: Int = 0,
  unknownDevCards: DevelopmentCardSet[Double] = DevelopmentCardSet.empty[Double])

case class PossibleDevelopmentCards(cards: Map[Int, PossibleDevCardsHands] = Map.empty)(implicit gameRules: GameRules[_]) {

  def apply(player: Int): PossibleDevCardsHands = cards.getOrElse(player, PossibleDevCardsHands())

  lazy val knownCards: DevelopmentCardSet[Int] = cards.toSeq.map { case (_, pdev) => pdev.knownDevCards}.foldLeft(DevelopmentCardSet.empty[Int])(_.add(_))

  lazy val prob: DevelopmentCardSet[Double] = {
    val left = gameRules.initDevCardAmounts.subtract(knownCards)
    DevelopmentCardSet(left.amountMap.map[DevelopmentCard, Double] { case (card, amt) =>
      card -> amt.toDouble / left.getTotal.toDouble
    })
  }

  private lazy val updateUnknownDevCards: PossibleDevelopmentCards = copy(
    cards.map {
      case (p, hand) =>
        val numUnknownCards = hand.numUnknownDevCards
        p -> hand.copy(unknownDevCards = (1 to numUnknownCards).foldLeft(DevelopmentCardSet.empty[Double]) { case (newSet, _) => newSet.add(prob) })
    }
  )

  def buyKnownCard(player: Int, turn: Int, card: DevelopmentCard): PossibleDevelopmentCards = {
    val insert = cards.get(player).fold(copy(cards + (player -> PossibleDevCardsHands())))(_ => this)
    insert.copy(
      cards = insert.cards.map {
        case (`player`, hand) => player -> hand.copy(knownDevCards = hand.knownDevCards.buyCard(turn, card))
        case (p, hand) => p -> hand
      }
    ).updateUnknownDevCards
  }

  def buyUnknownCard(player: Int): PossibleDevelopmentCards = {
    val insert = cards.get(player).fold(copy(cards + (player -> PossibleDevCardsHands())))(_ => this)
    insert.copy(
      cards = insert.cards.map {
        case (`player`, hand) => player -> hand.copy(numUnknownDevCards = hand.numUnknownDevCards + 1)
        case (p, hand) => p -> hand
      }
    ).updateUnknownDevCards
  }

  def playCard(turn: Int, player: Int, card: DevelopmentCard): PossibleDevelopmentCards = {
    val insert = cards.get(player).fold(copy(cards + (player -> PossibleDevCardsHands())))(_ => this)
    insert.copy {
      insert.cards.map {
        case (`player`, hand) =>
          val known = hand.knownDevCards.filterUnPlayed.contains(card)
          val addPlayedCard: PossibleDevCardsHands = hand.copy(knownDevCards = hand.knownDevCards.playCard(turn, card))
          player -> (if (known) addPlayedCard else addPlayedCard.copy(numUnknownDevCards = addPlayedCard.numUnknownDevCards - 1))
        case (p, hand) => p -> hand
      }
    }.updateUnknownDevCards
  }


}

object PossibleDevelopmentCards {
  def empty(implicit gameRules: GameRules[_]) = PossibleDevelopmentCards()
}
