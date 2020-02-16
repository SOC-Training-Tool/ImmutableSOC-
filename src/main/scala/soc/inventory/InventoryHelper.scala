package soc.inventory

import soc.core.GameRules
import soc.inventory.Inventory._
import soc.inventory.developmentCard._
import soc.inventory.resources.{PossibleHands, ProbableResourceSet, SOCPossibleHands, SOCTransactions}
import soc.state.player.{PlayerState, PlayerStateHelper}

sealed trait InventoryHelper[T <: Inventory[T]] {

  implicit val gameRules: GameRules[_]

  def updateResources(players: Map[Int, PlayerState[T]], transactions: List[SOCTransactions]): PlayerStateHelper[T]
  def playDevelopmentCard(players: Map[Int, PlayerState[T]], id: Int, turn: Int, card: DevelopmentCard): PlayerStateHelper[T]
  def buyDevelopmentCard(players: Map[Int, PlayerState[T]], id: Int, turn: Int, card: Option[DevelopmentCard]): PlayerStateHelper[T]
  def createInventory: T
}

case class PerfectInfoInventoryHelper()(implicit val gameRules: GameRules[_]) extends InventoryHelper[PerfectInfo] {

  override def updateResources(players: Map[Int, PlayerState[PerfectInfo]], transactions: List[SOCTransactions]): PlayerStateHelper[PerfectInfo] = {
    PlayerStateHelper(players.view.mapValues(_.updateResources(transactions)).toMap, this)
  }

  override def playDevelopmentCard(players: Map[Int, PlayerState[PerfectInfo]], id: Int, turn: Int, card: DevelopmentCard): PlayerStateHelper[PerfectInfo] = {
    PlayerStateHelper(players.map {
        case(`id`, ps) => id -> ps.updateDevelopmentCard(turn, PlayDevelopmentCard(id, card))
        case (i, ps) => i -> ps
      }, this)
  }

  override def buyDevelopmentCard(players: Map[Int, PlayerState[PerfectInfo]], id: Int, turn: Int, card: Option[DevelopmentCard]): PlayerStateHelper[PerfectInfo] = {
    PlayerStateHelper(players.map {
      case(`id`, ps) => id -> ps.updateDevelopmentCard(turn, BuyDevelopmentCard(id, card))
      case (i, ps) => i -> ps
    }, this)
  }

  override def createInventory: PerfectInfo = new PerfectInfoInventory()
}

case class ProbableInfoInventoryHelper(
  possibleHands: PossibleHands,
  possibleDevCards: PossibleDevelopmentCards)
  (implicit val gameRules: GameRules[_]) extends InventoryHelper[ProbableInfo] {

  override def updateResources(players: Map[Int, PlayerState[ProbableInfo]], transactions: List[SOCTransactions]): PlayerStateHelper[ProbableInfo] = {
    val newPossibleHands = possibleHands.calculateHands(transactions)
    val update = copy(possibleHands = newPossibleHands)
    PlayerStateHelper(players.map{ case (i, ps) =>
        i -> ps.updateResources((newPossibleHands.probableHands.get(i).getOrElse(ProbableResourceSet.empty), newPossibleHands.handsForPlayers.get(i).getOrElse(Nil).map(_._1)))
      }, update)
  }

  override def playDevelopmentCard(players: Map[Int, PlayerState[ProbableInfo]], id: Int, turn: Int, card: DevelopmentCard): PlayerStateHelper[ProbableInfo]  = {
    val newPossibleDevCards = possibleDevCards.playCard(turn, id, card)
    updateDevCards(turn, players, newPossibleDevCards)
  }

  override def buyDevelopmentCard(players: Map[Int, PlayerState[ProbableInfo]], id: Int, turn: Int, card: Option[DevelopmentCard]): PlayerStateHelper[ProbableInfo]  = card match {
    case Some(dcard) =>
      val newPossibleDevCards = possibleDevCards.buyKnownCard(id, turn, dcard)
      updateDevCards(turn, players, newPossibleDevCards)

    case None =>
      val newPossibleDevCards = possibleDevCards.buyUnknownCard(id)
      updateDevCards(turn, players, newPossibleDevCards)
  }

  private def updateDevCards(turn: Int, players: Map[Int, PlayerState[ProbableInfo]], possibleDevelopmentCards: PossibleDevelopmentCards): PlayerStateHelper[ProbableInfo] = {
    val update = copy(possibleDevCards = possibleDevelopmentCards)
    PlayerStateHelper(players.map{ case (i, ps) =>
        val possDevCards = possibleDevelopmentCards(i)
        i -> ps.updateDevelopmentCard(
          turn,
          (possDevCards.knownDevCards,
          possDevCards.unknownDevCards)
        )
      }, update)
  }

  override def createInventory: ProbableInfo = new ProbableInfoInventory()
}

case class PublicInfoInventoryHelper()(implicit val gameRules: GameRules[_]) extends InventoryHelper[PublicInfo] {

  override def updateResources(players: Map[Int, PlayerState[PublicInfo]], transactions: List[SOCTransactions]): PlayerStateHelper[PublicInfo] = {
    PlayerStateHelper(players.view.mapValues(_.updateResources(transactions)).toMap, this)
  }

  override def playDevelopmentCard(players: Map[Int, PlayerState[PublicInfo]], id: Int, turn: Int, card: DevelopmentCard): PlayerStateHelper[PublicInfo] = {
    PlayerStateHelper(players.map {
      case(`id`, ps) => id -> ps.updateDevelopmentCard(turn, PlayDevelopmentCard(id, card))
      case (i, ps) => i -> ps
    }, this)
  }

  override def buyDevelopmentCard(players: Map[Int, PlayerState[PublicInfo]], id: Int, turn: Int, card: Option[DevelopmentCard]): PlayerStateHelper[PublicInfo] = {
    PlayerStateHelper(players.map {
      case (`id`, ps) => id -> ps.updateDevelopmentCard(turn, BuyDevelopmentCard(id, card))
      case (i, ps) => i -> ps
    }, this)
  }

  override def createInventory: PublicInfo = new PublicInfoInventory
}

sealed trait InventoryHelperFactory[T <: Inventory[T]] {
  def createInventoryHelper(implicit gameRules: GameRules[_]): InventoryHelper[T]
}

object InventoryHelper {

  implicit val perfectInfoInventoryManagerFactory = new InventoryHelperFactory[PerfectInfo] {
    override def createInventoryHelper(implicit gameRules: GameRules[_]): InventoryHelper[PerfectInfo] = PerfectInfoInventoryHelper()
  }

  implicit val probableInfoInventoryManagerFactory = new InventoryHelperFactory[ProbableInfo] {
    override def createInventoryHelper(implicit gameRules: GameRules[_]): InventoryHelper[ProbableInfo] = ProbableInfoInventoryHelper(SOCPossibleHands.empty, PossibleDevelopmentCards.empty)
  }

  implicit val publicInfoInventoryManagerFactory = new InventoryHelperFactory[PublicInfo] {
    override def createInventoryHelper(implicit gameRules: GameRules[_]): InventoryHelper[PublicInfo] = PublicInfoInventoryHelper()
  }

  def get[T <: Inventory[T], U <: InventoryHelper[T]](helper: InventoryHelper[T]): U = helper.asInstanceOf[U]
}





