package soc.moves

import soc.board.{Edge, Vertex}
import soc.core.Roll
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.{DevelopmentCard, Resource}
import soc.moves2.RollDiceMove

case class RobPlayer(player: Int, res: Option[Resource])

sealed trait CatanMove
sealed trait MoveResult {
  val viewableBy: Seq[Int] = Nil
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, MoveResult] = playerIds.map(id => id -> this).toMap
  val move: CatanMove
}

sealed trait PerfectInfoMove extends CatanMove with MoveResult {
  override val move: CatanMove = this
}

trait CatanBuildMove extends CatanMove
trait CatanTradeMove extends CatanMove
sealed trait CatanPlayCardMove extends CatanMove
trait TradeResponse extends CatanTradeMove

  case object RollDiceMove extends CatanMove
  case class RollResult(roll: Roll) extends MoveResult { val move = RollDiceMove }

  case object EndTurnMove extends PerfectInfoMove

  case class InitialPlacementMove(first: Boolean, settlement: Vertex, road: Edge) extends PerfectInfoMove

  case class DiscardResourcesMove(resourceSet: Resources) extends CatanMove
  case class DiscardResourcesResult(resourceLost: Map[Int, Resources]) extends MoveResult  { val move = DiscardResourcesMove(resourceLost.head._2) }

  case class MoveRobberAndStealMove(node: Int, playerStole: Option[Int]) extends CatanMove
  case class MoveRobberAndStealResult(override val viewableBy: Seq[Int], robberLocation: Int, steal: Option[RobPlayer]) extends MoveResult {
    val move = MoveRobberAndStealMove(robberLocation, steal.map(_.player))
    override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, MoveResult] = playerIds.map {
      case p if viewableBy.contains(p) => p -> this
      case i => i -> MoveRobberAndStealResult(viewableBy, robberLocation, steal.map(r => RobPlayer(r.player, None)))
    }.toMap
  }

  case object BuyDevelopmentCardMove extends CatanBuildMove
  case class BuyDevelopmentCardResult(override val viewableBy: Seq[Int],  card: Option[DevelopmentCard]) extends MoveResult {
    val move = BuyDevelopmentCardMove
    override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, MoveResult] = playerIds.map {
      case p if viewableBy.contains(p) => p -> this
      case i => i ->  BuyDevelopmentCardResult(viewableBy, None)
    }.toMap
  }

  case class BuildRoadMove(edge: Edge) extends PerfectInfoMove
  case class BuildSettlementMove(vertex: Vertex) extends PerfectInfoMove
  case class BuildCityMove(vertex: Vertex) extends PerfectInfoMove

  case class PortTradeMove(give: Resources, get: Resources) extends PerfectInfoMove
  case class PlayerTradeMove(to: Int, give: Resources, get: Resources) extends PerfectInfoMove

  case class TradeMove(playerId: Int, give: Resources, get: Resources) extends PerfectInfoMove
  case object AcceptTrade extends TradeResponse
  case object RejectTrade extends TradeResponse
  case class CounterTrade(playerIdGive: Int, give: Resources, playerIdGet: Int, get: Resources) extends TradeResponse

  case class KnightMove(robber: MoveRobberAndStealMove) extends CatanPlayCardMove
  case class KnightResult(robber: MoveRobberAndStealResult) extends MoveResult {
    val move = KnightMove(robber.move)
    override val viewableBy= robber.viewableBy
    override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, MoveResult] =
      robber.getPerspectiveResults(playerIds).view.mapValues(m => KnightResult(m.asInstanceOf[MoveRobberAndStealResult])).toMap
  }

  case class YearOfPlentyMove(res1: Resource, res2: Resource) extends PerfectInfoMove
  case class MonopolyMove(res: Resource) extends CatanPlayCardMove
  case class MonopolyResult(cardsLost: Map[Int, Resources]) extends MoveResult {
    val move = MonopolyMove(cardsLost.head._2.getTypes.head)
  }
  case class RoadBuilderMove(road1: Edge, road2: Option[Edge]) extends PerfectInfoMove
  case object RevealPoint extends PerfectInfoMove
  //case object PointMove extends soc.moves.CatanPlayCardMove[]