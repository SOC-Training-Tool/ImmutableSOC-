package soc.moves

import protos.soc.state.Player
import soc.board.{Edge, Vertex}
import soc.core.Roll
import soc.inventory.resources.CatanResourceSet.Resources
import soc.inventory.resources.Steal
import soc.inventory.{DevelopmentCard, Resource}

case class RobPlayer(player: Int, res: Option[Resource])

sealed trait CatanMove
sealed trait MoveResult {
  val viewableBy: Seq[Int] = Nil
}

trait CatanBuildMove extends CatanMove
trait CatanTradeMove extends CatanMove
sealed trait CatanPlayCardMove extends CatanMove
trait TradeResponse extends CatanTradeMove

sealed trait ImperfectInformation

  case object RollDiceMove extends CatanMove with ImperfectInformation
  case class RollResult(roll: Roll) extends MoveResult

  case object EndTurnMove extends CatanMove with MoveResult

  case class InitialPlacementMove(first: Boolean, settlement: Vertex, road: Edge) extends CatanMove with MoveResult

  case class DiscardResourcesMove(resourceSet: Resources) extends CatanMove
  case class DiscardResourcesResult(resourceLost: Map[Int, Resources]) extends MoveResult

  case class MoveRobberAndStealMove(node: Int, playerStole: Option[Int]) extends CatanMove with ImperfectInformation
  case class MoveRobberAndStealResult(override val viewableBy: Seq[Int], robberLocation: Int, steal: Option[RobPlayer]) extends MoveResult

  case object BuyDevelopmentCardMove extends CatanBuildMove with ImperfectInformation
  case class BuyDevelopmentCardResult(override val viewableBy: Seq[Int],  card: Option[DevelopmentCard]) extends MoveResult

  case class BuildRoadMove(edge: Edge) extends CatanBuildMove with MoveResult
  case class BuildSettlementMove(vertex: Vertex) extends CatanBuildMove with MoveResult
  case class BuildCityMove(vertex: Vertex) extends CatanBuildMove with MoveResult

  case class PortTradeMove(give: Resources, get: Resources) extends CatanTradeMove with MoveResult
  case class PlayerTradeMove(to: Int, give: Resources, get: Resources) extends CatanTradeMove with MoveResult

  case class TradeMove(playerId: Int, give: Resources, get: Resources) extends CatanTradeMove with ImperfectInformation
  case object AcceptTrade extends TradeResponse
  case object RejectTrade extends TradeResponse
  case class CounterTrade(playerIdGive: Int, give: Resources, playerIdGet: Int, get: Resources) extends TradeResponse

  case class KnightMove(robber: MoveRobberAndStealMove) extends CatanPlayCardMove with ImperfectInformation
  case class KnightResult(robber: MoveRobberAndStealResult) extends MoveResult {
    override val viewableBy= robber.viewableBy}
  case class YearOfPlentyMove(res1: Resource, res2: Resource) extends CatanPlayCardMove with MoveResult
  case class MonopolyMove(res: Resource) extends CatanPlayCardMove with ImperfectInformation
  case class MonopolyResult(cardsLost: Map[Int, Resources]) extends MoveResult
  case class RoadBuilderMove(road1: Edge, road2: Option[Edge]) extends CatanPlayCardMove with MoveResult
  //case object PointMove extends soc.moves.CatanPlayCardMove[]