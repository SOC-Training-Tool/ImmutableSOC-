package soc.moves

import protos.soc.moves.ActionType._
import protos.soc.moves.ActionSpecification.{Payload => APayload}
import protos.soc.moves.ActionResult.{Payload => RPayload}
import protos.soc.moves.MoveRobberAndStealResult.StealResource
import protos.soc.moves.{ActionResult => PResult, ActionSpecification => PAction, DiscardResourcesResult => PDiscardResult, MoveRobberAndStealResult => PMoveRobber, _}
import protos.soc.state.Player
import soc.board.ProtoImplicits._
import soc.board.BaseCatanBoard.baseBoardMapping
import soc.board.Vertex
import soc.core.Roll
import soc.proto.ProtoCoder.ops.{proto, _}
import soc.inventory.ProtoImplicits._
import soc.inventory.resources.Steal
import soc.proto.ProtoCoder

object ProtoImplicits {

  implicit val protoMoveRobberAndSteal: ProtoCoder[MoveRobberAndStealMove, MoveRobberAndSteal] = { m =>
    val vertex = Vertex(m.node).proto
    MoveRobberAndSteal(vertex, m.playerStole)
  }

  implicit def protoAction[U <: CatanMove]: ProtoCoder[U, PAction] = move => move match {
    case RollDiceMove => PAction(ROLL_DICE)
    case EndTurnMove => PAction(END_TURN)
    case BuyDevelopmentCardMove => PAction(BUY_DEVELOPMENT_CARD)
    case InitialPlacementMove(first, settlement, road) =>
      PAction(INITIAL_PLACEMENT, APayload.InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case BuildSettlementMove(vertex) =>
      PAction(BUILD_SETTLEMENT, APayload.BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      PAction(BUILD_CITY, APayload.BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      PAction(BUILD_ROAD, APayload.BuildRoadPayload(BuildRoad(edge.proto)))
    case m: MoveRobberAndStealMove =>
      PAction(MOVE_ROBBER_AND_STEAL, APayload.MoveRobberAndStealPayload(proto[MoveRobberAndStealMove, MoveRobberAndSteal](m)))
    case PortTradeMove(give, get) =>
      PAction(PORT_TRADE, APayload.PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightMove(moveRobberAndStealMove) =>
      PAction(PLAY_KNIGHT, APayload.PlayKnightPayload(proto[MoveRobberAndStealMove, MoveRobberAndSteal](moveRobberAndStealMove)))
    case MonopolyMove(res) =>
      PAction(PLAY_MONOPOLY, APayload.PlayMonopolyPayload(PlayMonopoly(res.proto)))
    case YearOfPlentyMove(res1, res2) =>
      PAction(PLAY_YEAR_OF_PLENTY, APayload.PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case RoadBuilderMove(edge1, edge2) =>
      PAction(PLAY_ROAD_BUILDER, APayload.PlayRoadBuilderPayload(PlayRoadBuilder(edge1.proto, edge2.map(_.proto))))
    case DiscardResourcesMove(player, resourceSet) =>
      PAction(DISCARD_RESOURCES, APayload.DiscardResourcesPayload(DiscardResources(player, resourceSet.proto)))
    case _ => PAction(ActionType.END_TURN)
  }

  implicit val moveFromProto: ProtoCoder[PAction, CatanMove] = pMove => pMove match {
    case PAction(ROLL_DICE, _) => RollDiceMove
    case PAction(END_TURN, _) => EndTurnMove
    case PAction(BUY_DEVELOPMENT_CARD, _) => BuyDevelopmentCardMove
    case PAction(INITIAL_PLACEMENT, APayload.InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case PAction(BUILD_SETTLEMENT, APayload.BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case PAction(BUILD_CITY, APayload.BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case PAction(BUILD_ROAD, APayload.BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case PAction(MOVE_ROBBER_AND_STEAL, APayload.MoveRobberAndStealPayload(MoveRobberAndSteal(robberLoc, player))) =>
      MoveRobberAndStealMove(robberLoc.proto.node, player)
    case PAction(PORT_TRADE, APayload.PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case PAction(PLAY_KNIGHT, APayload.PlayKnightPayload(MoveRobberAndSteal(robberLoc, player))) =>
      KnightMove(MoveRobberAndStealMove(robberLoc.proto.node, player))
    case PAction(PLAY_MONOPOLY, APayload.PlayMonopolyPayload(PlayMonopoly(res))) =>
      MonopolyMove(res.proto)
    case PAction(PLAY_YEAR_OF_PLENTY, APayload.PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case PAction(PLAY_ROAD_BUILDER, APayload.PlayRoadBuilderPayload(PlayRoadBuilder(edge1, edge2))) =>
      RoadBuilderMove(edge1.proto, edge2.map(_.proto))
    case PAction(DISCARD_RESOURCES, APayload.DiscardResourcesPayload(DiscardResources(player, resourceSet))) =>
      DiscardResourcesMove(player, resourceSet.proto)
    case _ => EndTurnMove
  }

  implicit val protoMoveRobberAndStealResult: ProtoCoder[MoveRobberAndStealResult, PMoveRobber] = { m =>
    val (viewableBy, robberLocation, steal) = MoveRobberAndStealResult.unapply(m).get
    PMoveRobber(Vertex(robberLocation).proto, steal.map { s =>
      StealResource(s.player, HiddenCard(viewableBy, s.res.fold[HiddenCard.Card](HiddenCard.Card.Empty)(r => HiddenCard.Card.ResourceCard(r.proto))))
    })
  }

  implicit def protoResult[U <: MoveResult]: ProtoCoder[U, PResult] = result => result match {
    case RollResult(roll) =>
      PResult(ROLL_DICE, RPayload.RollDicePayload(RollDice(roll.number)))
    case EndTurnMove => PResult(END_TURN)
    case InitialPlacementMove(first, settlement, road) =>
      PResult(INITIAL_PLACEMENT, RPayload.InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case DiscardResourcesResult(cardsLost) =>
      PResult(DISCARD_RESOURCES, RPayload.DiscardResourcesPayload(
        PDiscardResult(cardsLost.toSeq.map { case (player, res) =>
          DiscardResources(player, res.proto)
        })
      ))
    case moveRobberAndStealResult: MoveRobberAndStealResult =>
      PResult(MOVE_ROBBER_AND_STEAL, RPayload.MoveRobberAndStealPayload(proto[MoveRobberAndStealResult, PMoveRobber](moveRobberAndStealResult)))
    case BuyDevelopmentCardResult(viewableBy, card) =>
      PResult(BUY_DEVELOPMENT_CARD, RPayload.BuyDevelopmentCardPayload(
        BuyDevelopmentCard(HiddenCard(viewableBy, card.fold[HiddenCard.Card](HiddenCard.Card.Empty)(r => HiddenCard.Card.DevelopmentCard(r.proto))))
      ))
    case BuildSettlementMove(vertex) =>
      PResult(BUILD_SETTLEMENT, RPayload.BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      PResult(BUILD_CITY, RPayload.BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      PResult(BUILD_ROAD, RPayload.BuildRoadPayload(BuildRoad(edge.proto)))
    case PortTradeMove(give, get) =>
      PResult(PORT_TRADE, RPayload.PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightResult(moveRobberAndStealResult) =>
      PResult(PLAY_KNIGHT, RPayload.PlayKnightPayload(proto[MoveRobberAndStealResult, PMoveRobber](moveRobberAndStealResult)))
    case MonopolyResult(resLost) =>
      PResult(PLAY_MONOPOLY, RPayload.PlayMonopolyPayload(PlayMonopolyResult(resLost.view.mapValues(_.proto.headOption.get).toMap)))
    case YearOfPlentyMove(res1, res2) =>
      PResult(PLAY_YEAR_OF_PLENTY, RPayload.PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case RoadBuilderMove(edge1, edge2) =>
      PResult(PLAY_ROAD_BUILDER, RPayload.PlayRoadBuilderPayload(PlayRoadBuilder(edge1.proto, edge2.map(_.proto))))
    case _ => PResult(END_TURN)
  }

  implicit val resultFromProto: ProtoCoder[PResult, MoveResult] = pResult => pResult match {
    case PResult(ROLL_DICE, RPayload.RollDicePayload(RollDice(number))) => RollResult(Roll(number))
    case PResult(END_TURN, _) => EndTurnMove
    case PResult(INITIAL_PLACEMENT, RPayload.InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case PResult(DISCARD_RESOURCES, RPayload.DiscardResourcesPayload(PDiscardResult(discards))) =>
      DiscardResourcesResult(discards.map { case  DiscardResources(player, res) => player -> res.proto }.toMap)
    case PResult(MOVE_ROBBER_AND_STEAL, RPayload.MoveRobberAndStealPayload(PMoveRobber(vertex, steal))) =>
      MoveRobberAndStealResult(
        steal.fold[Seq[Player]](Nil)(_.cardStolen.viewableBy),
        vertex.proto.node,
        steal.map(s => RobPlayer(s.playerRobbed, s.cardStolen.card.resourceCard.map(_.proto)))
      )
    case PResult(BUY_DEVELOPMENT_CARD, RPayload.BuyDevelopmentCardPayload(BuyDevelopmentCard(HiddenCard(viewableBy, card)))) =>
      BuyDevelopmentCardResult(viewableBy, card.developmentCard.map(_.proto))
    case PResult(BUILD_SETTLEMENT, RPayload.BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case PResult(BUILD_CITY, RPayload.BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case PResult(BUILD_ROAD, RPayload.BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case PResult(PORT_TRADE, RPayload.PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case PResult(PLAY_KNIGHT, RPayload.PlayKnightPayload(PMoveRobber(vertex, steal))) =>
      KnightResult(MoveRobberAndStealResult(
        steal.fold[Seq[Player]](Nil)(_.cardStolen.viewableBy),
        vertex.proto.node,
        steal.map(s => RobPlayer(s.playerRobbed, s.cardStolen.card.resourceCard.map(_.proto)))
      ))
    case PResult(PLAY_MONOPOLY, RPayload.PlayMonopolyPayload(PlayMonopolyResult(resLost))) =>
      MonopolyResult(resLost.view.mapValues(Seq(_).proto).toMap)
    case PResult(PLAY_YEAR_OF_PLENTY, RPayload.PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case PResult(PLAY_ROAD_BUILDER, RPayload.PlayRoadBuilderPayload(PlayRoadBuilder(edge1, edge2))) =>
      RoadBuilderMove(edge1.proto, edge2.map(_.proto))
    case _ => EndTurnMove
  }
}
