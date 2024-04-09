package soc.base

import game.{ImmutableGame, PerfectInfoMoveResult}
import shapeless.ops.coproduct
import shapeless.{:+:, ::, CNil, Coproduct, HNil, Poly1}
import soc.ToImperfectInfo
import soc.base.actions._
import soc.base.actions.build._
import soc.base.actions.developmentcards._
import soc.base.actions.special.{LargestArmyExtension, LongestRoadExtension}
import soc.base.state._
import soc.core.DevTransactions.ImperfectInfoBuyCard
import soc.core.DevelopmentCardInventories._
import soc.core.ResourceInventories._
import soc.core.Resources._
import soc.core.Transactions.{ImperfectInfo, PerfectInfo}
import soc.core.VertexBuilding.{cityValue, settlementValue}
import soc.core._
import soc.core.actions.{DiscardAction, EndTurnAction, TradeAction}
import soc.core.state.{EdgeBuildingState, VertexBuildingState}

object BaseGame {

  type BaseVertexBuilding = City.type :+: Settlement.type :+: CNil
  type BaseEdgeBuilding = Road.type :+: CNil

  type BaseGameState[ResInv[_], DevInv[_], DevCardState] = RobberLocation :: ResInv[Resource] :: VertexBuildingState[BaseVertexBuilding] :: BaseBoard[Resource] :: state.Bank[Resource] :: SOCRoadLengths :: SOCLongestRoadPlayer :: EdgeBuildingState[BaseEdgeBuilding] :: state.PlayerPoints :: state.Turn :: LargestArmyPlayer :: PlayerArmyCount :: DevInv[DevelopmentCard] :: DevCardState :: HNil

  type BaseMoves = PortTradeMove[Resource] :+: RobberMove[Resource] :+: moves.CORE_MOVES[Resource]
  type BaseMovesDevelopmentCard = BuyDevelopmentCardMove[DevelopmentCard] :+: PlayKnightMove[Resource] :+: PlayMonopolyMove[Resource] :+: PlayRoadBuilderMove :+: PlayYearOfPlentyMove[Resource] :+: BaseMoves

  type PerfectInfoState = BaseGameState[PrivateInventories, PrivateDevelopmentCards, DevelopmentCardDeck[DevelopmentCard]]
  type PublicInfoState = BaseGameState[PublicInventories, PublicDevelopmentCards, DevelopmentCardDeckSize]
  type ProbableInfoState = BaseGameState[ProbableInventories, PublicDevelopmentCards, DevelopmentCardDeckSize]

  private def longestRoadExtension[M] = LongestRoadExtension[M, Resource, BaseVertexBuilding, BaseEdgeBuilding, BaseBoard[Resource]]

  private def corePerfectInfoBuilder[Inv[_]](implicit inv: ResourceInventories[Resource, PerfectInfo[Resource], Inv]) = {
    ImmutableGame.builder
      .addAction(BuildSettlementAction[Resource, Inv, BaseVertexBuilding](ResourceSet(WOOD, BRICK, WHEAT, SHEEP))
        .extend(longestRoadExtension))
      .addAction(BuildCityAction[Resource, Inv, BaseVertexBuilding](ResourceSet(ORE, ORE, ORE, WHEAT, WHEAT)))
      .addAction(BuildRoadAction[Resource, Inv, BaseEdgeBuilding](ResourceSet(WOOD, BRICK))
        .extend(longestRoadExtension))
      .addAction(EndTurnAction())
      .addAction(PortTradeAction[Resource, Inv])
      .addAction(DiscardAction[Resource, Inv])
      .addAction(InitialPlacementAction[Resource, Inv, BaseVertexBuilding, BaseEdgeBuilding, BaseBoard[Resource]]
        .extend(longestRoadExtension))
      .addAction(RollDiceAction[Resource, BaseVertexBuilding, BaseBoard[Resource], Inv])
      .addAction(TradeAction[Resource, Inv])
  }

  private def corePerfectInfoDevCardBuilder[ResInv[_], DevInv[_]]
  (implicit inv: ResourceInventories[Resource, PerfectInfo[Resource], ResInv],
   dev: DevelopmentCardInventories[DevelopmentCard, DevInv]) = {
    ImmutableGame.builder
      .addAction(PlayMonopolyAction[Resource, ResInv, DevelopmentCard, DevInv])
      .addAction(PlayRoadBuilderAction[DevelopmentCard, DevInv, BaseEdgeBuilding]
        .extend(longestRoadExtension))
      .addAction(PlayYearOfPlentyAction[Resource, ResInv, DevelopmentCard, DevInv])
  }

  type PerfectInfoMoves = PerfectInfoRobberMoveResult[Resource] :+: TradeMove[Resource] :+: RollDiceMoveResult :+: InitialPlacementMove :+: DiscardMove[Resource] :+: PortTradeMove[Resource] :+: EndTurnMove :+: BuildRoadMove :+: BuildCityMove :+: BuildSettlementMove :+: PerfectInfoPlayKnightResult[Resource] :+: PerfectInfoBuyDevelopmentCardMoveResult[DevelopmentCard] :+: PlayYearOfPlentyMove[Resource] :+: PlayRoadBuilderMove :+: PlayMonopolyMoveResult[Resource] :+: CNil

  val perfectInfoBaseGame: ImmutableGame[PerfectInfoState, PerfectInfoMoves] = {
    val perfectInfoBaseBuilder = corePerfectInfoBuilder[PrivateInventories]
      .addAction(MoveRobberAndStealAction.perfectInfo[Resource, PrivateInventories])
    val perfectInfoDevCardBuilder =
      corePerfectInfoDevCardBuilder[PrivateInventories, PrivateDevelopmentCards]
        .addAction(BuyDevelopmentCardAction.perfectInfo[Resource, PrivateInventories, DevelopmentCard, PrivateDevelopmentCards](ResourceSet(ORE, WHEAT, SHEEP))
          .extend(PlayPointAction.extension[DevelopmentCard]))
        .addAction(PlayKnightAction.perfectInfo[Resource, PrivateInventories, DevelopmentCard, PrivateDevelopmentCards]
          .extend(LargestArmyExtension[Resource, PerfectInfoPlayKnightResult[Resource]]))
    perfectInfoBaseBuilder.merge(perfectInfoDevCardBuilder).build
  }

  def publicInfoBaseGame[ResInv[_], DevInv[_]]
  (implicit inv: ResourceInventories[Resource, ImperfectInfo[Resource], ResInv],
   dev: DevelopmentCardInventories[DevelopmentCard, DevInv],
   buyDev: BuyDevelopmentCard[ImperfectInfoBuyCard[DevelopmentCard], DevInv[DevelopmentCard]]) = {
    val publicInfoBaseBuilder = corePerfectInfoBuilder[ResInv]
      .addAction(MoveRobberAndStealAction[Resource, ResInv])
    val publicInfoDevCardBuilder =
      corePerfectInfoDevCardBuilder[ResInv, DevInv]
        .addAction(BuyDevelopmentCardAction[Resource, ResInv, DevelopmentCard, DevInv](ResourceSet(ORE, WHEAT, SHEEP)))
        .addAction(PlayPointAction[DevelopmentCard, DevInv])
        .addAction(PlayKnightAction[Resource, ResInv, DevelopmentCard, DevInv]
          .extend(LargestArmyExtension[Resource, PlayKnightResult[Resource]]))
    publicInfoBaseBuilder.merge(publicInfoDevCardBuilder).build
  }

  type ImperfectInfoMoves = RobberMoveResult[Resource] :+: TradeMove[Resource] :+: RollDiceMoveResult :+: InitialPlacementMove :+: DiscardMove[Resource] :+: PortTradeMove[Resource] :+: EndTurnMove :+: BuildRoadMove :+: BuildCityMove :+: BuildSettlementMove :+: PlayKnightResult[Resource] :+: PlayPointMove :+: BuyDevelopmentCardMoveResult[DevelopmentCard] :+: PlayYearOfPlentyMove[Resource] :+: PlayRoadBuilderMove :+: PlayMonopolyMoveResult[Resource] :+: CNil

  val publicInfoBase: ImmutableGame[PublicInfoState, ImperfectInfoMoves] =
    publicInfoBaseGame[PublicInventories, PublicDevelopmentCards]
  val probableInfoBase: ImmutableGame[ProbableInfoState, ImperfectInfoMoves] =
    publicInfoBaseGame[ProbableInventories, PublicDevelopmentCards]

}
