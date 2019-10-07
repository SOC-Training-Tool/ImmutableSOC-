package soc.inventory

import soc.core.GameRules
import soc.inventory.Inventory._
import soc.inventory.developmentCard._
import soc.inventory.resources.{PossibleHands, ProbableResourceSet, SOCPossibleHands, SOCTransactions}
import soc.state.player.PlayerState

trait InventoryHelper[T <: Inventory[T]] {

  implicit val gameRules: GameRules

  def updateResources(players: Map[Int, PlayerState[T]], transactions: List[SOCTransactions]): (Map[Int, PlayerState[T]], InventoryHelper[T])
  def playDevelopmentCard(players: Map[Int, PlayerState[T]], id: Int, turn: Int, card: DevelopmentCard): (Map[Int, PlayerState[T]], InventoryHelper[T])
  def buyDevelopmentCard(players: Map[Int, PlayerState[T]], id: Int, turn: Int, card: Option[DevelopmentCard]): (Map[Int, PlayerState[T]], InventoryHelper[T])
  def createInventory: T
}

case class PerfectInfoInventoryHelper()(implicit val gameRules: GameRules) extends InventoryHelper[PerfectInfo] {

  override def updateResources(players: Map[Int, PlayerState[PerfectInfo]], transactions: List[SOCTransactions]): (Map[Int, PlayerState[PerfectInfo]], InventoryHelper[PerfectInfo]) = {
    (players.view.mapValues(_.updateResources(transactions)).toMap, this)
  }

  override def playDevelopmentCard(players: Map[Int, PlayerState[PerfectInfo]], id: Int, turn: Int, card: DevelopmentCard): (Map[Int, PlayerState[PerfectInfo]], InventoryHelper[PerfectInfo]) = {
    (players.map {
        case(`id`, ps) => id -> ps.updateDevelopmentCard(turn, PlayDevelopmentCard(id, card))
        case (i, ps) => i -> ps
      }, this)
  }

  override def buyDevelopmentCard(players: Map[Int, PlayerState[PerfectInfo]], id: Int, turn: Int, card: Option[DevelopmentCard]): (Map[Int, PlayerState[PerfectInfo]], InventoryHelper[PerfectInfo]) = {
    (players.map {
      case(`id`, ps) => id -> ps.updateDevelopmentCard(turn, BuyDevelopmentCard(id, card))
      case (i, ps) => i -> ps
    }, this)
  }

  override def createInventory: PerfectInfo = new PerfectInfoInventory()
}

case class ProbableInfoInventoryHelper(
  possibleHands: PossibleHands,
  possibleDevCards: PossibleDevelopmentCards)
  (implicit val gameRules: GameRules) extends InventoryHelper[ProbableInfo] {

  override def updateResources(players: Map[Int, PlayerState[ProbableInfo]], transactions: List[SOCTransactions]): (Map[Int, PlayerState[ProbableInfo]], InventoryHelper[ProbableInfo]) = {
    val newPossibleHands = possibleHands.calculateHands(transactions)
    val update = copy(possibleHands = newPossibleHands)
    (players.map{ case (i, ps) =>
        i -> ps.updateResources(newPossibleHands.probableHands.get(i).getOrElse(ProbableResourceSet.empty))
      }, update)
  }

  override def playDevelopmentCard(players: Map[Int, PlayerState[ProbableInfo]], id: Int, turn: Int, card: DevelopmentCard): (Map[Int, PlayerState[ProbableInfo]], InventoryHelper[ProbableInfo])  = {
    val newPossibleDevCards = possibleDevCards.playCard(turn, id, card)
    updateDevCards(turn, players, newPossibleDevCards)
  }

  override def buyDevelopmentCard(players: Map[Int, PlayerState[ProbableInfo]], id: Int, turn: Int, card: Option[DevelopmentCard]): (Map[Int, PlayerState[ProbableInfo]], InventoryHelper[ProbableInfo])  = card match {
    case Some(dcard) =>
      val newPossibleDevCards = possibleDevCards.buyKnownCard(id, dcard)
      updateDevCards(turn, players, newPossibleDevCards)

    case None =>
      val newPossibleDevCards = possibleDevCards.buyUnknownCard(id)
      updateDevCards(turn, players, newPossibleDevCards)
  }

  private def updateDevCards(turn: Int, players: Map[Int, PlayerState[ProbableInfo]], possibleDevelopmentCards: PossibleDevelopmentCards): (Map[Int, PlayerState[ProbableInfo]], InventoryHelper[ProbableInfo]) = {
    val update = copy(possibleDevCards = possibleDevelopmentCards)
    (players.map{ case (i, ps) =>
        val possDevCards = possibleDevelopmentCards(i)
        i -> ps.updateDevelopmentCard(
          turn,
          (possDevCards.playedDevCards,
          possDevCards.knownunplayedDevCards,
          possDevCards.unknownDevCards)
        )
      }, update)
  }

  override def createInventory: ProbableInfo = new ProbableInfoInventory()
}

case class NoInfoInventoryHelper()(implicit val gameRules: GameRules) extends InventoryHelper[NoInfo] {

  override def updateResources(players: Map[Int, PlayerState[NoInfo]], transactions: List[SOCTransactions]): (Map[Int, PlayerState[NoInfo]], InventoryHelper[NoInfo]) = {
    (players.view.mapValues(_.updateResources(transactions)).toMap, this)
  }


  override def playDevelopmentCard(players: Map[Int, PlayerState[NoInfo]], id: Int, turn: Int, card: DevelopmentCard):(Map[Int, PlayerState[NoInfo]], InventoryHelper[NoInfo]) = {
    (players.map {
      case(`id`, ps) => id -> ps.updateDevelopmentCard(turn, PlayDevelopmentCard(id, card))
      case (i, ps) => i -> ps
    }, this)
  }



  override def buyDevelopmentCard(players: Map[Int, PlayerState[NoInfo]], id: Int, turn: Int, card: Option[DevelopmentCard]): (Map[Int, PlayerState[NoInfo]], InventoryHelper[NoInfo]) = {
    (players.map {
      case (`id`, ps) => id -> ps.updateDevelopmentCard(turn, BuyDevelopmentCard(id, card))
      case (i, ps) => i -> ps
    }, this)
  }


  override def createInventory: NoInfo = new NoInfoInventory
}

trait InventoryHelperFactory[T <: Inventory[T]] {
  def createInventoryHelper(implicit gameRules: GameRules): InventoryHelper[T]
}

object InventoryHelper {

  implicit val perfectInfoInventoryManagerFactory = new InventoryHelperFactory[PerfectInfo] {
    override def createInventoryHelper(implicit gameRules: GameRules): InventoryHelper[PerfectInfo] = PerfectInfoInventoryHelper()
  }

  implicit val probableInfoInventoryManagerFactory = new InventoryHelperFactory[ProbableInfo] {
    override def createInventoryHelper(implicit gameRules: GameRules): InventoryHelper[ProbableInfo] = ProbableInfoInventoryHelper(SOCPossibleHands.empty, PossibleDevelopmentCards.empty)
  }

  implicit val noInfoInventoryManagerFactory = new InventoryHelperFactory[NoInfo] {
    override def createInventoryHelper(implicit gameRules: GameRules): InventoryHelper[NoInfo] = NoInfoInventoryHelper()
  }
}




