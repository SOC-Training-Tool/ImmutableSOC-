package soc.base

import game.{ImmutableGame, InventorySet, StateInitializer}
import shapeless.{:+:, ::, CNil, HNil}
import soc.base.actions._
import soc.base.actions.build._
import soc.base.actions.developmentcards.{BuyDevelopmentCardAction, DevelopmentCardDeckSize, Knight, Monopoly, PlayKnightAction, PlayMonopolyAction, PlayPointAction, PlayRoadBuilderAction, PlayYearOfPlentyAction, Point, RoadBuilder, YearOfPlenty}
import soc.base.actions.special.{LargestArmyExtension, LongestRoadExtension}
import soc.base.state.{EdgeBuildingState, VertexBuildingState}
import soc.core.Resources._
import soc.core.VertexBuilding.{cityValue, settlementValue}
import soc.core._
import soc.inventory.DevelopmentCardInventories._
import soc.inventory.ResourceInventories._
import soc.inventory.Transactions.PerfectInfo
import soc.inventory.{DevelopmentCardInventories, ResourceInventories}

object BaseGame {

  type BaseVertexBuilding = City.type :+: Settlement.type :+: CNil
  type BaseEdgeBuilding = Road.type :+: CNil

  type DevelopmentCards = Knight.type :+: Monopoly.type :+: RoadBuilder.type :+: YearOfPlenty.type :+: Point.type :+: CNil

  type PerfectInfoState = state.RobberLocation :: PrivateInventories[Resource] :: VertexBuildingState[BaseVertexBuilding] :: BaseBoard[Resource] :: state.Bank[Resource] :: state.SOCRoadLengths :: state.SOCLongestRoadPlayer :: EdgeBuildingState[BaseEdgeBuilding] :: state.PlayerPoints :: state.Turn :: state.LargestArmyPlayer :: state.PlayerArmyCount :: PrivateDevelopmentCards[DevelopmentCards] :: DevelopmentCardDeckSize :: HNil
  type PerfectInfoMoves = PerfectInfoRobberMoveResult[Resource] :+: RollDiceMoveResult :+: InitialPlacementMove :+: DiscardMove[Resource] :+: PortTradeMove[Resource] :+: EndTurnMove :+: BuildRoadMove :+: BuildCityMove :+: BuildSettlementMove :+: PerfectInfoPlayKnightResult[Resource] :+: PerfectInfoBuyDevelopmentCardMoveResult[DevelopmentCards] :+: PlayYearOfPlentyMove[Resource] :+: PlayRoadBuilderMove :+: PlayMonopolyMoveResult[Resource] :+: CNil

  private def longestRoadExtension[M] = LongestRoadExtension[M, Resource, BaseVertexBuilding, BaseEdgeBuilding, BaseBoard[Resource]]

  private def corePerfectInfoBuilder[Inv[_]](implicit inv: ResourceInventories[Resource, PerfectInfo[Resource], Inv]) = {
    ImmutableGame.builder
      .addAction(BuildSettlementAction[Resource, Inv, BaseVertexBuilding](ResourceSet(WOOD, BRICK, WHEAT, SHEEP))
        .extend(longestRoadExtension).apply())
      .addAction(BuildCityAction[Resource, Inv,  BaseVertexBuilding](ResourceSet(ORE, ORE, ORE, WHEAT, WHEAT)))
      .addAction(BuildRoadAction[Resource, Inv,  BaseEdgeBuilding](ResourceSet(WOOD, BRICK))
        .extend(longestRoadExtension).apply())
      .addAction(EndTurnAction())
      .addAction(PortTradeAction[Resource, Inv])
      .addAction(DiscardAction[Resource, Inv])
      .addAction(InitialPlacementAction[Resource, Inv, BaseVertexBuilding, BaseEdgeBuilding, BaseBoard[Resource]]
        .extend(longestRoadExtension).apply())
      .addAction(RollDiceAction[Resource, BaseVertexBuilding, BaseBoard[Resource], Inv])
  }

  private def corePerfectInfoDevCardBuilder[ResInv[_], DevInv[_]]
  (implicit inv: ResourceInventories[Resource,  PerfectInfo[Resource], ResInv],
   dev: DevelopmentCardInventories[DevelopmentCards, DevInv]) = {
    ImmutableGame.builder
      .addAction(PlayMonopolyAction[Resource, ResInv, DevelopmentCards, DevInv])
      .addAction(PlayRoadBuilderAction[DevelopmentCards, DevInv, BaseEdgeBuilding]
        .extend(longestRoadExtension).apply())
      .addAction(PlayYearOfPlentyAction[Resource, ResInv, DevelopmentCards, DevInv])
  }

  private val perfectInfoBaseBuilder = corePerfectInfoBuilder[PrivateInventories]
    .addAction(MoveRobberAndStealAction.perfectInfo[Resource, PrivateInventories])

  private val perfectInfoDevCardBuilder = {
    corePerfectInfoDevCardBuilder[PrivateInventories, PrivateDevelopmentCards]
      .addAction(BuyDevelopmentCardAction.perfectInfo[Resource, PrivateInventories, DevelopmentCards, PrivateDevelopmentCards](ResourceSet(ORE, WHEAT, SHEEP))
        .extend(PlayPointAction.extension[DevelopmentCards]).apply())
      .addAction(PlayKnightAction.perfectInfo[Resource, PrivateInventories, DevelopmentCards, PrivateDevelopmentCards]
        .extend(LargestArmyExtension[PerfectInfoPlayKnightResult[Resource]]).apply())
  }

  val perfectInfoBaseGame: ImmutableGame[PerfectInfoState, PerfectInfoMoves] =
    perfectInfoBaseBuilder.merge(perfectInfoDevCardBuilder).apply().build.apply()



  def initRobberLocation[Res <: Coproduct](board: BaseBoard[Res]) = new StateInitializer[RobberLocation] {
    override def apply(): RobberLocation = RobberLocation(board.hexes.zipWithIndex.find(_._1.getResource.isEmpty).fold(0)(_._2))
  }

  def initBoard[Res <: Coproduct](board: BaseBoard[Res]) = new StateInitializer[BaseBoard[Res]] {
    override def apply(): BaseBoard[Res] = board
  }



  def perfectInfoInitialState(board: BaseBoard[Resource], players: List[Int], bank: InventorySet[Resource, Int], devDeckSize: Int) : PerfectInfoState = {
    implicit val fillPlayerIds = PlayerIds(players)
    implicit val fillRobberLoc = initRobberLocation(board)
    implicit val fillBoard = initBoard(board)
    implicit val fillDevDec = BuyDevelopmentCardAction.initDevCardSize(devDeckSize)
    implicit val fillBank = new StateInitializer[Bank[Resource]] {
      override def apply(): Bank[Resource] = Bank(bank)
    }
    ImmutableGame.initialize[PerfectInfoState]
  }


}
