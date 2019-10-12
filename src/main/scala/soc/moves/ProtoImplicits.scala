package soc.moves

import protos.soc.moves._
import protos.soc.moves.ActionEvent.Specification._
import protos.soc.moves.ActionType._
import protos.soc.moves.HiddenCard.Card.{DevelopmentCard, ResourceCard}
import protos.soc.moves.ResourceTransaction.TransactionType._
import protos.soc.state.Player
import soc.board.ProtoImplicits._
import soc.board.BaseCatanBoard.baseBoardMapping
import soc.board.Vertex
import soc.core.Roll
import soc.proto.ProtoCoder.ops.{proto, _}
import soc.inventory.ProtoImplicits._
import soc.inventory.resources.{Lose, Steal}
import soc.proto.ProtoCoder

object ProtoImplicits {

  implicit def protoAction[U <: CatanMove]: ProtoCoder[U, ActionEvent] = move => move match {
    case RollDiceMove => ActionEvent(ROLL_DICE)
    case EndTurnMove => ActionEvent(END_TURN)
    case InitialPlacementMove(first, settlement, road) =>
      ActionEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case DiscardResourcesMove(res) =>
      ActionEvent(DISCARD_RESOURCES, None, DiscardResourcesPayload(DiscardResources(res.proto)))
    case MoveRobberAndStealMove(node, playerStole) =>
      ActionEvent(MOVE_ROBBER_AND_STEAL, None, MoveRobberAndStealPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case BuyDevelopmentCardMove => ActionEvent(BUY_DEVELOPMENT_CARD)
    case BuildSettlementMove(vertex) =>
      ActionEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      ActionEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      ActionEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge.proto)))
    case PortTradeMove(give, get) =>
      ActionEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightMove(MoveRobberAndStealMove(node, playerStole)) =>
      ActionEvent(PLAY_KNIGHT, None, PlayKnightPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case YearOfPlentyMove(res1, res2) =>
      ActionEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case MonopolyMove(res) =>
      ActionEvent(PLAY_MONOPOLY, None, PlayMonopolyPayload(PlayMonopoly(res.proto)))
    case RoadBuilderMove(road1, road2) =>
      ActionEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1.proto, road2.map(_.proto))))
  }

  implicit val actionFromProto: ProtoCoder[ActionEvent, CatanMove] = pMove => pMove match {
    case ActionEvent(ROLL_DICE, _, _) => RollDiceMove
    case ActionEvent(END_TURN, _, _) => EndTurnMove
    case ActionEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case  ActionEvent(DISCARD_RESOURCES, None, DiscardResourcesPayload(DiscardResources(resources))) =>
      DiscardResourcesMove(resources.proto)
    case ActionEvent(MOVE_ROBBER_AND_STEAL, None, MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, playerStole))) =>
      MoveRobberAndStealMove(vertex.proto.node, playerStole)
    case ActionEvent(BUY_DEVELOPMENT_CARD, _, _) => BuyDevelopmentCardMove
    case ActionEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case ActionEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case ActionEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case ActionEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case ActionEvent(PLAY_KNIGHT, None, PlayKnightPayload(MoveRobberAndSteal(vertex, playerStole))) =>
      KnightMove(MoveRobberAndStealMove(vertex.proto.node, playerStole))
    case ActionEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case ActionEvent(PLAY_MONOPOLY, None, PlayMonopolyPayload(PlayMonopoly(res))) =>
      MonopolyMove(res.proto)
    case ActionEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1, road2))) =>
      RoadBuilderMove(road1.proto, road2.map(_.proto))
  }

  implicit def protoResult[U <: MoveResult]: ProtoCoder[U, ActionEvent] = move => move match {
    case RollResult(roll) =>
      val result = Some(ActionResult(roll = Some(roll.number)))
      ActionEvent(ROLL_DICE, result)
    case EndTurnMove => ActionEvent(END_TURN)
    case InitialPlacementMove(first, settlement, road) =>
      ActionEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case DiscardResourcesResult(resMap) =>
      val result = Some(ActionResult(resourceTransactions = resMap.view.mapValues(r => ResourceTransaction(LOSE, r.proto)).toMap))
      ActionEvent(DISCARD_RESOURCES, result)

    case MoveRobberAndStealResult(viewableBy, node, steal) =>
      val result = steal.map { p =>
        ActionResult(hiddenCard = Some(HiddenCard(viewableBy, p.res.fold[HiddenCard.Card](HiddenCard.Card.Empty)(r => ResourceCard(r.proto)))))
      }
      val playerStole = steal.map(_.player)
      ActionEvent(MOVE_ROBBER_AND_STEAL, result, MoveRobberAndStealPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case BuyDevelopmentCardResult(viewableBy, card) =>
      val result = card.map ( c => ActionResult(hiddenCard = Some(HiddenCard(viewableBy, DevelopmentCard(c.proto)))) )
      ActionEvent(BUY_DEVELOPMENT_CARD, result)
    case BuildSettlementMove(vertex) =>
      ActionEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      ActionEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      ActionEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge.proto)))
    case PortTradeMove(give, get) =>
      ActionEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightResult( MoveRobberAndStealResult(viewableBy, node, steal)) =>
      val result = steal.map { p =>
        ActionResult(hiddenCard = Some(HiddenCard(viewableBy, p.res.fold[HiddenCard.Card](HiddenCard.Card.Empty)(r => ResourceCard(r.proto)))))
      }
      val playerStole = steal.map(_.player)
      ActionEvent(PLAY_KNIGHT, result, PlayKnightPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case YearOfPlentyMove(res1, res2) =>
      ActionEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case MonopolyResult(cardsLost) =>
      val result = Some(ActionResult(resourceTransactions = cardsLost.view.mapValues(r => ResourceTransaction(LOSE, r.proto)).toMap))
      ActionEvent(PLAY_MONOPOLY, result)
    case RoadBuilderMove(road1, road2) =>
      ActionEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1.proto, road2.map(_.proto))))
  }

  implicit val resultFromProto: ProtoCoder[ActionEvent, MoveResult] = pMove => pMove match {
    // public actions
    case ActionEvent(ROLL_DICE, Some(ActionResult(Some(number), None, _)), _) => RollResult(Roll(number))
    case ActionEvent(END_TURN, _, _) => EndTurnMove
    case ActionEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case ActionEvent(DISCARD_RESOURCES, Some(ActionResult(None, None, discards)), _) =>
      DiscardResourcesResult(discards.view.mapValues { case ResourceTransaction(ResourceTransaction.TransactionType.LOSE, res) => res.proto }.toMap)
    case ActionEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case ActionEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case ActionEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case ActionEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case ActionEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case ActionEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1, road2))) =>
      RoadBuilderMove(road1.proto, road2.map(_.proto))
    case ActionEvent(PLAY_MONOPOLY, Some(ActionResult(None, None, cardsLost)), PlayMonopolyPayload(PlayMonopoly(res))) =>
      MonopolyResult(cardsLost.view.mapValues { case ResourceTransaction(ResourceTransaction.TransactionType.LOSE, res) => res.proto }.toMap)

    // actions with private info

    // No card was stolen
    case ActionEvent(MOVE_ROBBER_AND_STEAL, None, MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, None))) =>
      MoveRobberAndStealResult(Nil, vertex.proto.node, None)
    // Card was stolen but card is not viewable
    case ActionEvent(MOVE_ROBBER_AND_STEAL, Some(ActionResult(None, Some(HiddenCard(viewableBy, HiddenCard.Card.Empty)), _)), MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, None)))
    // Card was stolen and card is viewable
    case ActionEvent(MOVE_ROBBER_AND_STEAL, Some(ActionResult(None, Some(HiddenCard(viewableBy, ResourceCard(res))), _)), MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, Some(res.proto))))

    // No card was stolen
    case ActionEvent(PLAY_KNIGHT, None, PlayKnightPayload(MoveRobberAndSteal(vertex, None))) =>
      KnightResult(MoveRobberAndStealResult(Nil, vertex.proto.node, None))
    // Card was stolen but card is not viewable
    case ActionEvent(PLAY_KNIGHT, Some(ActionResult(None, Some(HiddenCard(viewableBy, HiddenCard.Card.Empty)), _)), PlayKnightPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      KnightResult(MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, None))))
    // Card was stolen and card is viewable
    case ActionEvent(PLAY_KNIGHT, Some(ActionResult(None, Some(HiddenCard(viewableBy, ResourceCard(res))), _)), PlayKnightPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      KnightResult(MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, Some(res.proto)))))

    // No development cards left in deck
    case ActionEvent(BUY_DEVELOPMENT_CARD, Some(ActionResult(None, None, _)), _) =>
      BuyDevelopmentCardResult(Nil, None)
    // development card was bought but card is not viewable
    case ActionEvent(BUY_DEVELOPMENT_CARD, Some(ActionResult(None, Some(HiddenCard(viewableBy, HiddenCard.Card.Empty)),_)), _) =>
      BuyDevelopmentCardResult(viewableBy, None)
    case ActionEvent(BUY_DEVELOPMENT_CARD, Some(ActionResult(None, Some(HiddenCard(viewableBy, DevelopmentCard(dev))), _)), _) =>
      BuyDevelopmentCardResult(viewableBy, Some(dev.proto))
  }

}
