package soc.moves

import protos.soc.moves._
import protos.soc.moves.GameEvent.Specification._
import protos.soc.moves.GameAction._
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

  implicit def protoAction[U <: CatanMove]: ProtoCoder[U, GameEvent] = move => move match {
    case RollDiceMove => GameEvent(ROLL_DICE)
    case EndTurnMove => GameEvent(END_TURN)
    case InitialPlacementMove(first, settlement, road) =>
      GameEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case DiscardResourcesMove(res) =>
      GameEvent(DISCARD_RESOURCES, None, DiscardResourcesPayload(DiscardResources(res.proto)))
    case MoveRobberAndStealMove(node, playerStole) =>
      GameEvent(MOVE_ROBBER_AND_STEAL, None, MoveRobberAndStealPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case BuyDevelopmentCardMove => GameEvent(BUY_DEVELOPMENT_CARD)
    case BuildSettlementMove(vertex) =>
      GameEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      GameEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      GameEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge.proto)))
    case PortTradeMove(give, get) =>
      GameEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightMove(MoveRobberAndStealMove(node, playerStole)) =>
      GameEvent(PLAY_KNIGHT, None, PlayKnightPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case YearOfPlentyMove(res1, res2) =>
      GameEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case MonopolyMove(res) =>
      GameEvent(PLAY_MONOPOLY, None, PlayMonopolyPayload(PlayMonopoly(res.proto)))
    case RoadBuilderMove(road1, road2) =>
      GameEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1.proto, road2.map(_.proto))))
  }

  implicit val actionFromProto: ProtoCoder[GameEvent, CatanMove] = pMove => pMove match {
    case GameEvent(ROLL_DICE, _, _) => RollDiceMove
    case GameEvent(END_TURN, _, _) => EndTurnMove
    case GameEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case  GameEvent(DISCARD_RESOURCES, None, DiscardResourcesPayload(DiscardResources(resources))) =>
      DiscardResourcesMove(resources.proto)
    case GameEvent(MOVE_ROBBER_AND_STEAL, None, MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, playerStole))) =>
      MoveRobberAndStealMove(vertex.proto.node, playerStole)
    case GameEvent(BUY_DEVELOPMENT_CARD, _, _) => BuyDevelopmentCardMove
    case GameEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case GameEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case GameEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case GameEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case GameEvent(PLAY_KNIGHT, None, PlayKnightPayload(MoveRobberAndSteal(vertex, playerStole))) =>
      KnightMove(MoveRobberAndStealMove(vertex.proto.node, playerStole))
    case GameEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case GameEvent(PLAY_MONOPOLY, None, PlayMonopolyPayload(PlayMonopoly(res))) =>
      MonopolyMove(res.proto)
    case GameEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1, road2))) =>
      RoadBuilderMove(road1.proto, road2.map(_.proto))
  }

  implicit def protoResult[U <: MoveResult]: ProtoCoder[U, GameEvent] = move => move match {
    case RollResult(roll) =>
      val result = Some(ActionResult(roll = Some(roll.number)))
      GameEvent(ROLL_DICE, result)
    case EndTurnMove => GameEvent(END_TURN)
    case InitialPlacementMove(first, settlement, road) =>
      GameEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement.proto, road.proto, first)))
    case DiscardResourcesResult(resMap) =>
      val result = Some(ActionResult(resourceTransactions = resMap.view.mapValues(r => ResourceTransaction(LOSE, r.proto)).toMap))
      GameEvent(DISCARD_RESOURCES, result)

    case MoveRobberAndStealResult(viewableBy, node, steal) =>
      val result = steal.map { p =>
        ActionResult(hiddenCard = Some(HiddenCard(viewableBy, p.res.fold[HiddenCard.Card](HiddenCard.Card.Empty)(r => ResourceCard(r.proto)))))
      }
      val playerStole = steal.map(_.player)
      GameEvent(MOVE_ROBBER_AND_STEAL, result, MoveRobberAndStealPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case BuyDevelopmentCardResult(viewableBy, card) =>
      val result = card.map ( c => ActionResult(hiddenCard = Some(HiddenCard(viewableBy, DevelopmentCard(c.proto)))) )
      GameEvent(BUY_DEVELOPMENT_CARD, result)
    case BuildSettlementMove(vertex) =>
      GameEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex.proto)))
    case BuildCityMove(vertex) =>
      GameEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex.proto)))
    case BuildRoadMove(edge) =>
      GameEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge.proto)))
    case PortTradeMove(give, get) =>
      GameEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give.proto, get.proto)))
    case KnightResult( MoveRobberAndStealResult(viewableBy, node, steal)) =>
      val result = steal.map { p =>
        ActionResult(hiddenCard = Some(HiddenCard(viewableBy, p.res.fold[HiddenCard.Card](HiddenCard.Card.Empty)(r => ResourceCard(r.proto)))))
      }
      val playerStole = steal.map(_.player)
      GameEvent(PLAY_KNIGHT, result, PlayKnightPayload(MoveRobberAndSteal(Vertex(node).proto, playerStole)))
    case YearOfPlentyMove(res1, res2) =>
      GameEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1.proto, res2.proto)))
    case MonopolyResult(cardsLost) =>
      val result = Some(ActionResult(resourceTransactions = cardsLost.view.mapValues(r => ResourceTransaction(LOSE, r.proto)).toMap))
      GameEvent(PLAY_MONOPOLY, result)
    case RoadBuilderMove(road1, road2) =>
      GameEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1.proto, road2.map(_.proto))))
  }

  implicit val resultFromProto: ProtoCoder[GameEvent, MoveResult] = pMove => pMove match {
    // public actions
    case GameEvent(ROLL_DICE, Some(ActionResult(Some(number), None, _)), _) => RollResult(Roll(number))
    case GameEvent(END_TURN, _, _) => EndTurnMove
    case GameEvent(INITIAL_PLACEMENT, None, InitialPlacementPayload(InitialPlacement(settlement, road, first))) =>
      InitialPlacementMove(first, settlement.proto, road.proto)
    case GameEvent(DISCARD_RESOURCES, Some(ActionResult(None, None, discards)), _) =>
      DiscardResourcesResult(discards.view.mapValues { case ResourceTransaction(ResourceTransaction.TransactionType.LOSE, res) => res.proto }.toMap)
    case GameEvent(BUILD_SETTLEMENT, None, BuildSettlementPayload(BuildSettlement(vertex))) =>
      BuildSettlementMove(vertex.proto)
    case GameEvent(BUILD_CITY, None, BuildCityPayload(BuildCity(vertex))) =>
      BuildCityMove(vertex.proto)
    case GameEvent(BUILD_ROAD, None, BuildRoadPayload(BuildRoad(edge))) =>
      BuildRoadMove(edge.proto)
    case GameEvent(PORT_TRADE, None, PortTradePayload(PortTrade(give, get))) =>
      PortTradeMove(give.proto, get.proto)
    case GameEvent(PLAY_YEAR_OF_PLENTY, None, PlayYearOfPlentyPayload(PlayYearOfPlenty(res1, res2))) =>
      YearOfPlentyMove(res1.proto, res2.proto)
    case GameEvent(PLAY_ROAD_BUILDER, None, PlayRoadBuilderPayload(PlayRoadBuilder(road1, road2))) =>
      RoadBuilderMove(road1.proto, road2.map(_.proto))
    case GameEvent(PLAY_MONOPOLY, Some(ActionResult(None, None, cardsLost)), PlayMonopolyPayload(PlayMonopoly(res))) =>
      MonopolyResult(cardsLost.view.mapValues { case ResourceTransaction(ResourceTransaction.TransactionType.LOSE, res) => res.proto }.toMap)

    // actions with private info

    // No card was stolen
    case GameEvent(MOVE_ROBBER_AND_STEAL, None, MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, None))) =>
      MoveRobberAndStealResult(Nil, vertex.proto.node, None)
    // Card was stolen but card is not viewable
    case GameEvent(MOVE_ROBBER_AND_STEAL, Some(ActionResult(None, Some(HiddenCard(viewableBy, HiddenCard.Card.Empty)), _)), MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, None)))
    // Card was stolen and card is viewable
    case GameEvent(MOVE_ROBBER_AND_STEAL, Some(ActionResult(None, Some(HiddenCard(viewableBy, ResourceCard(res))), _)), MoveRobberAndStealPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, Some(res.proto))))

    // No card was stolen
    case GameEvent(PLAY_KNIGHT, None, PlayKnightPayload(MoveRobberAndSteal(vertex, None))) =>
      KnightResult(MoveRobberAndStealResult(Nil, vertex.proto.node, None))
    // Card was stolen but card is not viewable
    case GameEvent(PLAY_KNIGHT, Some(ActionResult(None, Some(HiddenCard(viewableBy, HiddenCard.Card.Empty)), _)), PlayKnightPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      KnightResult(MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, None))))
    // Card was stolen and card is viewable
    case GameEvent(PLAY_KNIGHT, Some(ActionResult(None, Some(HiddenCard(viewableBy, ResourceCard(res))), _)), PlayKnightPayload(MoveRobberAndSteal(vertex, Some(playerStole)))) =>
      KnightResult(MoveRobberAndStealResult(viewableBy, vertex.proto.node, Some(RobPlayer(playerStole, Some(res.proto)))))

    // No development cards left in deck
    case GameEvent(BUY_DEVELOPMENT_CARD, Some(ActionResult(None, None, _)), _) =>
      BuyDevelopmentCardResult(Nil, None)
    // development card was bought but card is not viewable
    case GameEvent(BUY_DEVELOPMENT_CARD, Some(ActionResult(None, Some(HiddenCard(viewableBy, HiddenCard.Card.Empty)),_)), _) =>
      BuyDevelopmentCardResult(viewableBy, None)
    case GameEvent(BUY_DEVELOPMENT_CARD, Some(ActionResult(None, Some(HiddenCard(viewableBy, DevelopmentCard(dev))), _)), _) =>
      BuyDevelopmentCardResult(viewableBy, Some(dev.proto))
  }

}
