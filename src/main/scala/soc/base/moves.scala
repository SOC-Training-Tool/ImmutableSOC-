package soc.base

import soc.board.{Edge, Vertex}
import soc.core.Roll
import soc.inventory.{CatanSet, InventoryItem}
import soc.inventory.resources.ResourceSet.Resources

trait SOCMove {
  type R <: SOCMoveResult

  def player: Int
}

trait SOCMoveResult {
  type A <: SOCMove

  def move: A
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]]
}

object SOCMoveResult {
  type Move[M <: SOCMove] = SOCMoveResult {type A = M}
}

trait PerfectInformationSOCMove[A0 <: SOCMove] extends SOCMove with SOCMoveResult {
  self: A0 =>
  type A = A0
  type R = A0

  def move: A = self
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]] = playerIds.map(id => id -> this).toMap
}

case class RollDiceMove(player: Int) extends SOCMove {
  type R = RollDiceResult
}

case class RollDiceResult(player: Int, roll: Roll) extends SOCMoveResult {
  override type A = RollDiceMove

  override def move: RollDiceMove = RollDiceMove(player)
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, RollDiceResult] = playerIds.map(id => id -> this).toMap
}

case class EndTurnMove(player: Int) extends PerfectInformationSOCMove[EndTurnMove]

case class PortTradeMove(player: Int, give: Resources, get: Resources) extends PerfectInformationSOCMove[PortTradeMove]

case class RobberMove[II <: InventoryItem](player: Int, robberLocation: Int, playerStole: Option[Int]) extends SOCMove {
  override type R = RobberMoveResult[II]
}

case class RobPlayer[II <: InventoryItem](player: Int, res: Option[II])
case class RobberMoveResult[II <: InventoryItem](player: Int, robberLocation: Int, cardStole: Option[RobPlayer[II]]) extends SOCMoveResult {
  override type A = RobberMove[II]
  override def move: RobberMove[II] = RobberMove(player, robberLocation, cardStole.map(_.player))
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, RobberMoveResult[II]] = playerIds.map {
    case `player` => player -> this
    case p if cardStole.fold(false)(_.player == p) => p -> this
    case p => p -> RobberMoveResult[II](player, robberLocation, cardStole.map(_.copy(res = None)))
  }.toMap
}

case class DiscardMove[II <: InventoryItem](player: Int, cards: CatanSet[II, Int]) extends PerfectInformationSOCMove[DiscardMove[II]]

case class InitialPlacementMove(player: Int, first: Boolean, vertex: Vertex, edge: Edge) extends PerfectInformationSOCMove[InitialPlacementMove]
case class BuildSettlementMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildSettlementMove]
case class BuildCityMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildCityMove]
case class BuildRoadMove(player: Int, edge: Edge) extends PerfectInformationSOCMove[BuildRoadMove]




