package soc.base

import game.ImmutableGame.InitializeOp
import game.{ImmutableGame, InventorySet, PerfectInfoMoveResult}
import shapeless.ops.coproduct
import shapeless.ops.hlist.FillWith
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Poly1}
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
import util.DependsOn

object BaseGame {

  type BaseVertexBuilding = City.type :+: Settlement.type :+: CNil
  type BaseEdgeBuilding = Road.type :+: CNil

  type DevelopmentCards = Knight.type :+: Monopoly.type :+: RoadBuilder.type :+: YearOfPlenty.type :+: Point.type :+: CNil

  type BaseGameState[ResInv[_], DevInv[_]] = RobberLocation :: ResInv[Resource] :: VertexBuildingState[BaseVertexBuilding] :: BaseBoard[Resource] :: state.Bank[Resource] :: SOCRoadLengths :: SOCLongestRoadPlayer :: EdgeBuildingState[BaseEdgeBuilding] :: state.PlayerPoints :: state.Turn :: LargestArmyPlayer :: PlayerArmyCount :: DevInv[DevelopmentCards] :: DevelopmentCardDeckSize :: HNil

  type PerfectInfoState = BaseGameState[PrivateInventories, PrivateDevelopmentCards]
  type PublicInfoState = BaseGameState[PublicInventories, PublicDevelopmentCards]
  type ProbableInfoState = BaseGameState[ProbableInventories, PublicDevelopmentCards]

  private def longestRoadExtension[M] = LongestRoadExtension[M, Resource, BaseVertexBuilding, BaseEdgeBuilding, BaseBoard[Resource]]

  private def corePerfectInfoBuilder[Inv[_]](implicit inv: ResourceInventories[Resource, PerfectInfo[Resource], Inv]) = {
    ImmutableGame.builder
      .addAction(BuildSettlementAction[Resource, Inv, BaseVertexBuilding](ResourceSet(WOOD, BRICK, WHEAT, SHEEP))
        .extend(longestRoadExtension))
      .addAction(BuildCityAction[Resource, Inv,  BaseVertexBuilding](ResourceSet(ORE, ORE, ORE, WHEAT, WHEAT)))
      .addAction(BuildRoadAction[Resource, Inv,  BaseEdgeBuilding](ResourceSet(WOOD, BRICK))
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
  (implicit inv: ResourceInventories[Resource,  PerfectInfo[Resource], ResInv],
   dev: DevelopmentCardInventories[DevelopmentCards, DevInv]) = {
    ImmutableGame.builder
      .addAction(PlayMonopolyAction[Resource, ResInv, DevelopmentCards, DevInv])
      .addAction(PlayRoadBuilderAction[DevelopmentCards, DevInv, BaseEdgeBuilding]
        .extend(longestRoadExtension))
      .addAction(PlayYearOfPlentyAction[Resource, ResInv, DevelopmentCards, DevInv])
  }

  type PerfectInfoMoves = PerfectInfoRobberMoveResult[Resource] :+: TradeMove[Resource] :+: RollDiceMoveResult :+: InitialPlacementMove :+: DiscardMove[Resource] :+: PortTradeMove[Resource] :+: EndTurnMove :+: BuildRoadMove :+: BuildCityMove :+: BuildSettlementMove :+: PerfectInfoPlayKnightResult[Resource] :+: PerfectInfoBuyDevelopmentCardMoveResult[DevelopmentCards] :+: PlayYearOfPlentyMove[Resource] :+: PlayRoadBuilderMove :+: PlayMonopolyMoveResult[Resource] :+: CNil

  val perfectInfoBaseGame: ImmutableGame[PerfectInfoState, PerfectInfoMoves] = {
    val perfectInfoBaseBuilder = corePerfectInfoBuilder[PrivateInventories]
      .addAction(MoveRobberAndStealAction.perfectInfo[Resource, PrivateInventories])
    val perfectInfoDevCardBuilder =
      corePerfectInfoDevCardBuilder[PrivateInventories, PrivateDevelopmentCards]
        .addAction(BuyDevelopmentCardAction.perfectInfo[Resource, PrivateInventories, DevelopmentCards, PrivateDevelopmentCards](ResourceSet(ORE, WHEAT, SHEEP))
          .extend(PlayPointAction.extension[DevelopmentCards]))
        .addAction(PlayKnightAction.perfectInfo[Resource, PrivateInventories, DevelopmentCards, PrivateDevelopmentCards]
          .extend(LargestArmyExtension[PerfectInfoPlayKnightResult[Resource]]))
    perfectInfoBaseBuilder.merge(perfectInfoDevCardBuilder).build
  }

  def publicInfoBaseGame[ResInv[_], DevInv[_]]
  (implicit inv: ResourceInventories[Resource, ImperfectInfo[Resource], ResInv],
   dev: DevelopmentCardInventories[DevelopmentCards, DevInv],
   buyDev: BuyDevelopmentCard[ImperfectInfoBuyCard[DevelopmentCards], DevInv[DevelopmentCards]]) = {
    val publicInfoBaseBuilder = corePerfectInfoBuilder[ResInv]
      .addAction(MoveRobberAndStealAction[Resource, ResInv])
    val publicInfoDevCardBuilder =
      corePerfectInfoDevCardBuilder[ResInv, DevInv]
        .addAction(BuyDevelopmentCardAction[Resource, ResInv, DevelopmentCards, DevInv](ResourceSet(ORE, WHEAT, SHEEP)))
        .addAction(PlayPointAction[DevelopmentCards, DevInv])
        .addAction(PlayKnightAction[Resource, ResInv, DevelopmentCards, DevInv]
          .extend(LargestArmyExtension[PlayKnightResult[Resource]]))
    publicInfoBaseBuilder.merge(publicInfoDevCardBuilder).build
  }

  type ImperfectInfoMoves = RobberMoveResult[Resource] :+: TradeMove[Resource] :+: RollDiceMoveResult :+: InitialPlacementMove :+: DiscardMove[Resource] :+: PortTradeMove[Resource] :+: EndTurnMove :+: BuildRoadMove :+: BuildCityMove :+: BuildSettlementMove :+: PlayKnightResult[Resource] :+: PlayPointMove :+: BuyDevelopmentCardMoveResult[DevelopmentCards] :+: PlayYearOfPlentyMove[Resource] :+: PlayRoadBuilderMove :+: PlayMonopolyMoveResult[Resource] :+: CNil

  val publicInfoBase: ImmutableGame[PublicInfoState, ImperfectInfoMoves] =
    publicInfoBaseGame[PublicInventories, PublicDevelopmentCards]
  val probableInfoBase: ImmutableGame[ProbableInfoState, ImperfectInfoMoves] =
    publicInfoBaseGame[ProbableInventories, PublicDevelopmentCards]

  object PerfectToImperfectMovesPoly extends Poly1 {
    implicit def toImperfect[P <: PerfectInfoMoveResult]: PerfectToImperfectMovesPoly.Case.Aux[P, P#ImperfectInfoMoveResult] = at[P](_.getPerspectiveResults(Seq(-1)).head._2)

    implicit def rollDice: Case.Aux[RollDiceMoveResult, RollDiceMoveResult] = at[RollDiceMoveResult](identity)

  }

  case class MoveTransformer[O <: Coproduct]
  ()
  (implicit mapper: coproduct.Mapper.Aux[PerfectToImperfectMovesPoly.type, PerfectInfoMoves, O],
   basis: coproduct.Basis[PlayPointMove :+: O, O],
   align: coproduct.Align[PlayPointMove :+: O, ImperfectInfoMoves]) {
    def apply(perfectInfoMove: PerfectInfoMoves) = {
      val outCoproduct: O = perfectInfoMove.map(PerfectToImperfectMovesPoly)
      align.apply(outCoproduct.embed[PlayPointMove :+: O])
    }
  }

  def initGame[S <: HList]
  (board: BaseBoard[Resource], bank: InventorySet[Resource, Int], devDeckSize: Int)
  (implicit fillWith: FillWith[InitializeOp.type, S], dep: DependsOn[S, BaseBoard[Resource] :: RobberLocation :: state.Bank[Resource] :: DevelopmentCardDeckSize :: HNil]): S = {
    val rl = RobberLocation(board.hexes.zipWithIndex.find(_._1.getResource.isEmpty).fold(0)(_._2))
    dep.updateAll(ImmutableGame.initialize[S])(_ => board :: rl :: state.Bank(bank) :: DevelopmentCardDeckSize(devDeckSize) :: HNil)
  }
}
