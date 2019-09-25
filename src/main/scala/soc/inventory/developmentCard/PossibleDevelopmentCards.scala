package soc.inventory.developmentCard

import soc.core.GameRules
import soc.inventory.DevelopmentCard
import soc.inventory.developmentCard.DevelopmentCardSet.{PlayedInventory, UnplayedInventory}

case class PossibleDevCardsHands(
  playedDevCards: PlayedInventory = DevelopmentCardSet.empty[Int],
  knownunplayedDevCards: PlayedInventory = DevelopmentCardSet.empty[Int],
  numUnknownDevCards: Int = 0,
  unknownDevCards: UnplayedInventory = DevelopmentCardSet.empty[Double])

case class PossibleDevelopmentCards(cards: Map[Int, PossibleDevCardsHands] = Map.empty)(implicit gameRules: GameRules) {

  def apply(player: Int): PossibleDevCardsHands = cards.getOrElse(player, PossibleDevCardsHands())

  lazy val knownCards: DevelopmentCardSet[Int] = cards.toSeq.map { case (_, pdev) =>
    pdev.playedDevCards.add(pdev.knownunplayedDevCards)
  }.foldLeft(DevelopmentCardSet.empty[Int])(_.add(_))

  lazy val prob: DevelopmentCardSet[Double] = {
    val left = gameRules.initDevCardAmounts.subtract(knownCards)
    DevelopmentCardSet.toInventory(left.amountMap.map { case (card, amt) =>
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

  def buyKnownCard(player: Int, card: DevelopmentCard): PossibleDevelopmentCards =
    cards.get(player).fold(copy(cards + (player -> PossibleDevCardsHands())))(_ => this).copy(
      cards.map {
        case (`player`, hand) => player -> hand.copy(knownunplayedDevCards = hand.knownunplayedDevCards.add(1, card))
        case (p, hand) => p -> hand
      }
    ).updateUnknownDevCards

  def buyUnknownCard(player: Int): PossibleDevelopmentCards =
    cards.get(player).fold(copy(cards + (player -> PossibleDevCardsHands())))(_ => this).copy(
      cards.map {
        case (`player`, hand) => player -> hand.copy(numUnknownDevCards = hand.numUnknownDevCards + 1)
        case (p, hand) => p -> hand
      }
    ).updateUnknownDevCards

  def playCard(player: Int, card: DevelopmentCard): PossibleDevelopmentCards = copy(
    cards.map {
      case (`player`, hand) =>
        val addPlayedCard = hand.copy(playedDevCards = hand.playedDevCards.add(1, card))
        player -> {
          if (addPlayedCard.knownunplayedDevCards.contains(card)) {
            addPlayedCard.copy(knownunplayedDevCards = addPlayedCard.knownunplayedDevCards.subtract(1, card))
          } else addPlayedCard.copy(numUnknownDevCards = addPlayedCard.numUnknownDevCards - 1)
        }
      case (p, hand) => p -> hand
    }
  ).updateUnknownDevCards


}

object PossibleDevelopmentCards {
  def empty(implicit gameRules: GameRules) = PossibleDevelopmentCards()
}
