package soc.moves

import protos.soc.moves.MoveType._
import protos.soc.moves.SOCMove.Payload._
import protos.soc.moves.{SOCMove => PMove, _}
import soc.board.ProtoImplicits._
import soc.board.BaseCatanBoard.baseBoardMapping
import soc.board.Vertex
import soc.proto.ProtoCoder.ops.{proto, _}
import soc.inventory.ProtoImplicits._
import soc.proto.ProtoCoder

object ProtoImplicits {

  implicit val protoMoveRobberAndSteal: ProtoCoder[MoveRobberAndStealMove, MoveRobberAndSteal] = { m =>
    val vertex = Vertex(m.node).proto
    MoveRobberAndSteal(vertex, m.playerStole)
  }

  implicit def protoMove[U <: CatanMove]: ProtoCoder[U, PMove] = move => move match {
    case RollDiceMove => PMove(ROLL_DICE)
    case EndTurnMove => PMove(END_TURN)
    case BuyDevelopmentCardMove => PMove(BUY_DEVELOPMENT_CARD)
    case InitialPlacementMove(first, settlement, road) =>
      PMove(INITIAL_PLACEMENT, InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case BuildSettlementMove(vertex) =>
      PMove(BUILD_SETTLEMENT, BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      PMove(BUILD_CITY, BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      PMove(BUILD_ROAD, BuildRoadPayload(BuildRoad(edge.proto)))
    case m: MoveRobberAndStealMove =>
      PMove(MOVE_ROBBER_AND_STEAL, MoveRobberAndStealPayload(proto[MoveRobberAndStealMove, MoveRobberAndSteal](m)))
    case PortTradeMove(give, get) =>
      PMove(PORT_TRADE, PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightMove(moveRobberAndStealMove) =>
      PMove(PLAY_KNIGHT, PlayKnightPayload(proto[MoveRobberAndStealMove, MoveRobberAndSteal](moveRobberAndStealMove)))
    case MonopolyMove(res) =>
      PMove(PLAY_MONOPOLY, PlayMonopolyPayload(PlayMonopoly(res.proto)))
    case YearOfPlentyMove(res1, res2) =>
      PMove(PLAY_YEAR_OF_PLENTY, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case RoadBuilderMove(edge1, edge2) =>
      PMove(PLAY_ROAD_BUILDER, PlayRoadBuilderPayload(PlayRoadBuilder(edge1.proto, edge2.map(_.proto))))
    case DiscardResourcesMove(player, resourceSet) =>
      PMove(DISCARD_RESOURCES, DiscardResourcesPayload(DiscardResources(player, resourceSet.proto)))
  }

  implicit val moveFromProto: ProtoCoder[PMove, CatanMove] = pMove => pMove match {
    case PMove(ROLL_DICE, _) => RollDiceMove
    case PMove(END_TURN, _) => EndTurnMove
    case PMove(BUY_DEVELOPMENT_CARD, _) => BuyDevelopmentCardMove
    case PMove(INITIAL_PLACEMENT, InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case PMove(BUILD_SETTLEMENT, BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case PMove(BUILD_CITY, BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case PMove(BUILD_ROAD, BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case PMove(MOVE_ROBBER_AND_STEAL, MoveRobberAndStealPayload(MoveRobberAndSteal(robberLoc, player))) =>
      MoveRobberAndStealMove(robberLoc.proto.node, player)
    case PMove(PORT_TRADE, PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case PMove(PLAY_KNIGHT, PlayKnightPayload(MoveRobberAndSteal(robberLoc, player))) =>
      KnightMove(MoveRobberAndStealMove(robberLoc.proto.node, player))
    case PMove(PLAY_MONOPOLY, PlayMonopolyPayload(PlayMonopoly(res))) =>
      MonopolyMove(res.proto)
    case PMove(PLAY_YEAR_OF_PLENTY, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case PMove(PLAY_ROAD_BUILDER, PlayRoadBuilderPayload(PlayRoadBuilder(edge1, edge2))) =>
      RoadBuilderMove(edge1.proto, edge2.map(_.proto))
    case PMove(DISCARD_RESOURCES, DiscardResourcesPayload(DiscardResources(player, resourceSet))) =>
      DiscardResourcesMove(player, resourceSet.proto)
  }
}
