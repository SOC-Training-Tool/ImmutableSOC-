package soc.gametype

import soc.board.BaseCatanBoard.BASE
import soc.board.{BoardConfiguration, BoardGenerator, CatanBoard, Edge, Vertex}
import soc.inventory._
import soc.moves2._
import soc.moves2.build._
import soc.moves2.developmentcard.{DevelopmentCardSOCState, LargestArmySOCState, SOCDevelopmentCardsInDeck, SOCLargestArmyPlayer, SOCNumKnights}

case class BaseGameSOCState[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]](
  board: CatanBoard[BOARD],
  bank: CatanSet[Resource, Int] = CatanSet.empty,
  inventoryHelper: PERSPECTIVE,
  settlements: SOCSettlementMap,
  cities: SOCCityMap,
  roads: SOCRoadMap,
  playerPoints: SOCPlayerPointsMap,
  turn: SOCTurn,
  robberLocation: SOCRobberLocation,
  developmentCardsLeft: SOCDevelopmentCardsInDeck,
  longestRoadPlayer: SOCLongestRoadPlayer,
  roadLengths: SOCRoadLengths,
  largestArmyPlayer: SOCLargestArmyPlayer,
  numKnights: SOCNumKnights
) extends SettlementSOCState[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with CitySOCState[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with RoadSOCState[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with InitialPlacementSOCState[BOARD, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with RobberSOCState[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with PortSOCState[BOARD, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with RollDiceSOCState[BOARD, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with DevelopmentCardSOCState[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with LongestRoadSOCState[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]]
    with LargestArmySOCState[BOARD, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]] {
  override def updateSettlements(settlements: SOCSettlementMap): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(settlements = settlements)
  override def updateCities(cities: SOCCityMap): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(cities = cities)
  override def updateRoads(roads: SOCRoadMap): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(roads = roads)
  override def updateRobberLocation(v: SOCRobberLocation): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(robberLocation = v)
  override def updateInvHelper(inventoryHelper: PERSPECTIVE): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(inventoryHelper = inventoryHelper)
  override def updateBank(bank: CatanSet[Resource, Int]): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(bank = bank)
  override def incrementTurn: BaseGameSOCState[BOARD, PERSPECTIVE] = copy(turn = turn.copy(turn.t + 1))
  override def updatePoints(playerPoints: SOCPlayerPointsMap): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(playerPoints = playerPoints)
  override def decrementNumDevelopmentCards: BaseGameSOCState[BOARD, PERSPECTIVE] = copy(developmentCardsLeft = developmentCardsLeft.copy(developmentCardsLeft.d - 1))
  override def updateLongestRoadPlayer(player: SOCLongestRoadPlayer): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(longestRoadPlayer = player)
  override def updateRoadLengths(roadLengths: SOCRoadLengths): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(roadLengths = roadLengths)
  override def updateLargestArmyPlayer(player: SOCLargestArmyPlayer): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(largestArmyPlayer = player)
  override def updateNumKnights(numKnights: SOCNumKnights): BaseGameSOCState[BOARD, PERSPECTIVE] = copy(numKnights = numKnights)
}


object BaseGame {

  implicit val baseBoardRobberLocator: InitialRobberLocationLocator[BASE, BaseGameSOCState[BASE, _]] = { board =>
    board.hexesWithNodes.find(_.hex.getNumber.isEmpty).get.node
  }

  implicit def baseGameFactory[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]](implicit robberLocator: InitialRobberLocationLocator[BOARD, BaseGameSOCState[BOARD, _]]): SOCStateFactory[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]] =
    new SOCStateFactory[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]] {
      override def create(boardConf: BOARD, initBank: CatanSet[Resource, Int], playerIds: Seq[Int])(implicit boardGenerator: BoardGenerator[BOARD], invHelperFactory: InventoryHelperFactory[Resource, PERSPECTIVE]): BaseGameSOCState[BOARD, PERSPECTIVE] = {
        val board: CatanBoard[BOARD] = boardGenerator.apply(boardConf)
        BaseGameSOCState(
          board,
          initBank,
          invHelperFactory.create(playerIds.toList),
          Map.empty,
          Map.empty,
          Map.empty,
          Map.empty,
          0,
          robberLocator.getInitialRobberLocation(board),
          25
        )
      }
    }

//  val boardConf: BaseBoardConfiguration = BaseCatanBoard.randomBoard()
//
//  val settlementAction = BuildSettlementAction[BASE](5, ResourceSet(Wood, Brick, Wheat, Sheep))
//  val roadAction = BuildRoadAction[BASE](15, ResourceSet(Wood, Brick))
//  val cityAction = BuildCityAction[BASE](4, ResourceSet(Ore, Ore, Ore, Wheat, Wheat))
//
//  val initialPlacementAction = InitialPlacementAction[BASE](settlementAction, roadAction)
//  val rollDiceAction = RollDiceAction[BASE](null, null)
//  val endTurnAction = EndTurnAction[BASE]
//  val portTradeAction = PortTradeAction[BASE]
//
//  val robberAction = RobberAction[BASE](null)
//  val discardAction = DiscardAction[BASE](7)
//
//  val playKnightAction = PlayKnightAction[BASE](14, robberAction)
//  val playMonopolyAction = PlayMonopolyAction[BASE](2, null)
//  val playRoadBuilderAction = PlayRoadBuilderAction[BASE](2, roadAction)
//  val playYopAction = PlayYearOfPlentyAction[BASE](2)
//
//  val buyDevelopmentCardAction = BuyDevelopmentCardAction[BASE](ResourceSet(Ore, Wheat, Sheep), null, playKnightAction, playMonopolyAction, playRoadBuilderAction, playYopAction)
//
//  def buildTradePortPhase[A <: SOCMove[A]]: MoveTransition[A, BASE] = {
//    case (_, state) if state.canPlayCard => Seq(settlementAction, roadAction, cityAction, portTradeAction, buyDevelopmentCardAction, playYopAction, playRoadBuilderAction, playMonopolyAction, playKnightAction, endTurnAction)
//    case _ => Seq(settlementAction, roadAction, cityAction, portTradeAction, buyDevelopmentCardAction, endTurnAction)
//  }
//  def preRollPhase[A <: PlayDevelopmentCardMove[A]]: MoveTransition[A, BASE] = {
//    case (_, state) if state.canRollDice => Seq(rollDiceAction)
//  }
//  def rollDicePhase: MoveTransition[RollDiceMove, BASE] = {
//    case (RollDiceResult(_, Roll(7)), state) if !state.expectingDiscard.isEmpty => Seq(discardAction)
//    case (RollDiceResult(_, Roll(7)), _) => Seq(robberAction)
//  }
//  def playDevelopmentCardMoveTransition[A <: PlayDevelopmentCardMove[A]]: MoveTransition[A, BASE] = preRollPhase[A].orElse(buildTradePortPhase[A])
//
//  val moveTransitionMap = MoveTransitionMap.builder.addMoveTransition(initialPlacementAction) {
//    case (_, state) if state.currentPlayer == state.firstPlayerId && settlementAction.getAmountForPlayer(state.board, state.currentPlayer) > 1 => Seq(rollDiceAction)
//    case _ => Seq(initialPlacementAction)
//  }.addMoveTransition(discardAction) {
//    case (_, state) if !state.expectingDiscard.isEmpty => Seq(discardAction)
//    case _ => Seq(robberAction)
//  }.addMoveTransition(endTurnAction) {
//    case (_, state) if state.canPlayCard => Seq(rollDiceAction, playYopAction, playRoadBuilderAction, playMonopolyAction, playKnightAction)
//    case _ => Seq(rollDiceAction)
//  }.addMoveTransition(rollDiceAction) (rollDicePhase.orElse(buildTradePortPhase[RollDiceMove]))
//    .addMoveTransition(robberAction)(buildTradePortPhase)
//    .addMoveTransition(settlementAction)(buildTradePortPhase)
//    .addMoveTransition(cityAction)(buildTradePortPhase)
//    .addMoveTransition(roadAction)(buildTradePortPhase)
//    .addMoveTransition(portTradeAction)(buildTradePortPhase)
//    .addMoveTransition(buyDevelopmentCardAction)(buildTradePortPhase)
//    .addMoveTransition(playKnightAction)(playDevelopmentCardMoveTransition)
//    .addMoveTransition(playYopAction)(playDevelopmentCardMoveTransition)
//    .addMoveTransition(playRoadBuilderAction)(playDevelopmentCardMoveTransition)
//    .addMoveTransition(playMonopolyAction)(playDevelopmentCardMoveTransition)
//    .create(initialPlacementAction)


}
