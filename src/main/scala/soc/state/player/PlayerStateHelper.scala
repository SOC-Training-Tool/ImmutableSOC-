package soc.state.player

import soc.board._
import soc.core.GameRules
import soc.inventory.Inventory.PublicInfo
import soc.inventory._
import soc.inventory.resources.SOCTransactions

case class PlayerStateHelper [T <: Inventory[T]] protected (val players: Map[Int, PlayerState[T]])(
  implicit val inventoryHelper: InventoryHelper[T],
  implicit val gameRules: GameRules)  {

  val playerIds = players.keys.toSeq.sorted

  val firstPlayerId = playerIds.min
  val lastPlayerId = playerIds.max

  def nextPlayer(playerId: Int): Int = {
    val indexOf = playerIds.indexOf(playerId)
    playerIds.drop(indexOf + 1).headOption.getOrElse(firstPlayerId)
  }

  def previousPlayer(playerId: Int): Int = {
    val indexOf = playerIds.indexOf(playerId)
    playerIds.dropRight(playerIds.length - indexOf).lastOption.getOrElse(lastPlayerId)
  }

  def getPlayers: Seq[PlayerState[T]] = players.values.toList
  def getPlayer(id: Int): PlayerState[T] = players(id)

  def buildSettlement(id: Int, vertex: Vertex, board: CatanBoard): PlayerStateHelper[T] = copy (
    players = players.map {
      case(`id`, ps) => id -> ps.buildSettlement(board, vertex)
      case (i, ps) => i -> ps
    }
  )
  def buildCity(id: Int, vertex: Vertex, board: CatanBoard): PlayerStateHelper[T] = copy (
    players = players.map {
      case(`id`, ps) => id -> ps.buildCity(board, vertex)
      case (i, ps) => i -> ps
    }
  )

   def buildRoad(id: Int, edge: Edge, board: CatanBoard): PlayerStateHelper[T] = copy (
    players = players.map {
      case(`id`, ps) => id -> ps.buildRoad(board, edge)
      case (i, ps) => i -> ps
    }
  ).updateLongestRoad

  def playKnight(id: Int, turn: Int): PlayerStateHelper[T] = playDevelopmentCard(id, turn, Knight).updateLargestArmy

  def updateResources(transactions: List[SOCTransactions]): PlayerStateHelper[T] = {
    val (newPlayers, newInvManager) = inventoryHelper.updateResources(players, transactions)
    copy(newPlayers)(newInvManager, gameRules)
  }

  def buyDevelopmentCard(turn: Int, id: Int, card: Option[DevelopmentCard]):  PlayerStateHelper[T] = {
    val (newPlayers, newInvManager) = inventoryHelper.buyDevelopmentCard(players, id, turn, card)
    copy(newPlayers)(newInvManager, gameRules)
  }

  def endTurn(id: Int): PlayerStateHelper[T] = copy (
    players = players.map {
      case (`id`, ps) => id -> ps.endTurn
      case (i, ps) => i -> ps
    }
  )

  protected def playDevelopmentCard(id: Int, turn: Int, card: DevelopmentCard): PlayerStateHelper[T] = {
    val (newPlayers, newInvManager) = inventoryHelper.playDevelopmentCard(players, id, turn, card)
    copy(newPlayers)(newInvManager, gameRules)
  }

  def playMonopoly(id: Int, turn: Int): PlayerStateHelper[T] = playDevelopmentCard(id, turn, Monopoly)
  def playYearOfPlenty(id: Int, turn: Int): PlayerStateHelper[T] = playDevelopmentCard(id, turn, YearOfPlenty)
  def playRoadBuilder(id: Int, turn: Int): PlayerStateHelper[T] = playDevelopmentCard(id, turn, RoadBuilder)
  def playPointCard(id: Int, turn: Int): PlayerStateHelper[T] = playDevelopmentCard(id, turn, CatanPoint)

  def updateLongestRoad: PlayerStateHelper[T] = {
    updateSpecialPoints(
      _.roadPoints,
      _.roadLength,
      _.gainLongestRoad,
      _.loseLongestRoad,
      gameRules.longestRoad
    )
  }

  def updateLargestArmy: PlayerStateHelper[T] = {
    import soc.inventory.developmentCard.DevelopmentCardSet._
    updateSpecialPoints(
      _.armyPoints,
      _.inventory.playedDevCards.getAmount(Knight),
      _.gainLargestArmy,
      _.loseLargestArmy,
      gameRules.largetsArmy
    )
  }

  private def updateSpecialPoints(findLargest: PlayerState[T] => Int, getAmount: PlayerState[T] => Int, gain: PlayerState[T] => PlayerState[T], lose: PlayerState[T] => PlayerState[T], pointThreshold: Int): PlayerStateHelper[T] = {
    val largest: Option[PlayerState[T]] = players.values.find(findLargest(_) >= 2)
    val newLargest = players.values.filter(p => getAmount(p) > largest.fold(pointThreshold - 1)(getAmount(_))).headOption.map(_.position)
    copy (
      (largest.map(_.position), newLargest) match {
        case (_, None) => players
        case (None, Some(has)) =>  players.map {
          case (`has`, ps) => has -> gain(ps)
          case (id, ps) => id -> ps
        }
        case (Some(had), Some(has)) => players.map {
          case (`had`, ps) => had -> lose(ps)
          case (`has`, ps) => has -> gain(ps)
          case (id, ps) => id -> ps
        }
      }
    )
  }

  def toPublicInfoHelper: PlayerStateHelper[PublicInfo] = {
    import soc.inventory.InventoryHelper.publicInfoInventoryManagerFactory
    PlayerStateHelper(players.view.mapValues(_.toPublicInfo).toMap)(publicInfoInventoryManagerFactory.createInventoryHelper, gameRules)
  }
}

object PlayerStateHelper {

  def apply[T <: Inventory[T]](s: Seq[Int])(implicit factory: InventoryHelperFactory[T], gameRules: GameRules) = {
    implicit val invHelper = factory.createInventoryHelper
    new PlayerStateHelper[T](s.map {id => id -> PlayerState(id, invHelper.createInventory) }.toMap)
  }

}

