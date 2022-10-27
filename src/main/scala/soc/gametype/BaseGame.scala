//package soc.gametype
//
//import shapeless.ops.hlist.{SelectAll, Selector}
//import shapeless.{::, HList, HNil}
//import soc.board.BoardConfiguration
//import soc.inventory._
//import soc.inventory.resources.ResourceSet
//import soc.inventory.resources.ResourceSet.Resources
//import soc.state.SOCState.SOCState
//import soc.moves2._
//import soc.moves2.build._
//import soc.moves2.developmentcard.DevelopmentCardInventory.PerfectInfoDevInv
//import soc.moves2.developmentcard._
//import soc.state.UpdateState
//import soc.state.build.BoardOps
//import util.DependsOn
//
//object BaseGame {
//
//  implicit def baseRollOps[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](implicit boardOps: BoardOps[BOARD, Resource, PERSPECTIVE, STATE], dep: DependsOn[STATE, SOCCanRollDice :: SOCRobberLocation :: SOCState[BOARD, Resource, PERSPECTIVE]]) = new RollDiceOps[BOARD, Resource, PERSPECTIVE, STATE] {
//
//    import RollDiceSOCState._
//
//    implicit val rollOps = this
//
//    override def getVertexBuildingValue: PartialFunction[VertexBuilding, Int] = Map(Settlement -> 1, City -> 2)
//
//    override def onRoll(rollDiceResult: RollDiceResult, s: STATE)(f: STATE => STATE): STATE = {
//      f(s.distributeResources(s.getResourcesGainedOnRoll(rollDiceResult.roll.number)))
//    }
//  }
//
////  type BASE_NEXT_MOVES[W[_ <: SOCMoveResult]] = W[BuildSettlementMove] :: W[BuildRoadMove] :: W[BuildCityMove] :: W[EndTurnMove] :: HNil // TODO add full list
////  implicit def baseRollNextMoves[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], W[_ <: SOCMoveResult], A <: HList, STATE <: HList](implicit ws: Selector[A, W[RollDiceResult]], sa: SelectAll[A, BASE_NEXT_MOVES[W]]): NextMove[BOARD, II, PERSPECTIVE, W, A, STATE, RollDiceResult] = (a: A) => {
////    // TODO implement robber
////    val func = (_: STATE, r: RollDiceResult) => Map(r.player -> sa.apply(a).toList)
////    ws.apply(a) -> func
////  }
////
////  implicit def applyRollDiceMoveResult[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](implicit boardOps: BoardOps[BOARD, Resource, PERSPECTIVE, STATE], dep: DependsOn[STATE, SOCCanRollDice :: SOCRobberLocation :: SOCState[BOARD, Resource, PERSPECTIVE]]) = new ApplyMoveResult[BOARD, Resource, PERSPECTIVE, RollDiceResult, STATE] {
////    override def apply(state: STATE, moveResult: RollDiceResult): STATE = {
////      import RollDiceSOCState._
////      state.rollDice(moveResult)
////    }
////  }
//
//  //  type BaseGameSOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE]] =
//  //    SOCSettlementMap :: SOCCityMap :: SOCRoadMap :: SOCRobberLocation :: SOCDevelopmentCardsInDeck ::
//  //      SOCLongestRoadPlayer :: SOCRoadLengths :: SOCLargestArmyPlayer :: SOCNumKnights ::
//  //      SOCPlayersToDiscard :: SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]
//
//  type BaseGameSOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE]] =
//    SOCSettlementMap :: SOCCityMap :: SOCRoadMap :: SOCLongestRoadPlayer :: SOCRoadLengths :: SOCCanRollDice :: SOCRobberLocation :: SOCState[BOARD, II, PERSPECTIVE]
//
//
//  type PlayDevelopmentCardMoves = PlayKnightMove :: PlayMonopolyMove :: PlayYearOfPlentyMove :: PlayRoadBuilderMove :: HNil
//  type RegularMoves = EndTurnMove :: BuildSettlementMove :: BuildCityMove :: BuildRoadMove :: PortTradeMove :: BuyDevelopmentCardMove :: PlayDevelopmentCardMoves
//
//  def builder[BOARD <: BoardConfiguration, PERSPECTIVE <: DevelopmentCardInventoryHelper[Resource, PERSPECTIVE]] = {
//
//    import CitySOCState._
//    import RoadSOCState._
//    import RollDiceMove._
//    import SettlementSOCState._
//
//    implicit val settlementCost = new Cost[Resource, BuildSettlementMove] {
//      override def getCost: Resources = ResourceSet(Wood, Brick, Wheat, Sheep)
//    }
//    implicit val cityCost = new Cost[Resource, BuildCityMove] {
//      override def getCost: Resources = ResourceSet(Ore, Ore, Ore, Wheat, Wheat)
//    }
//    implicit val roadCost = new Cost[Resource, BuildRoadMove] {
//      override def getCost: Resources = ResourceSet(Wood, Brick)
//    }
//
//    import soc.state.UpdateState._
//    UpdateState.build[BOARD, Resource, PERSPECTIVE, BuildSettlementMove :: BuildCityMove :: BuildRoadMove :: HNil, BaseGameSOCState[BOARD, Resource, PERSPECTIVE]]
//
//    SOCGame.build[BOARD, Resource, PERSPECTIVE, PerfectInfoDevInv[Resource], BaseGameSOCState[BOARD, Resource, PERSPECTIVE]] {
//      fact =>
//        import fact._
//        type ALL_MOVES = BuildSettlementMove :: BuildCityMove :: BuildRoadMove :: RollDiceMove :: EndTurnMove :: InitialPlacementMove :: HNil
//
////        fact.apply[ALL_MOVES].startGame[InitialPlacementMove]
//
//    }
//
//
//    //    implicit val boardOps = BoardOps.baseBoardOps[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, Resource, PERSPECTIVE]]
//    //    implicit val rollOps = baseRollOps[BOARD, PERSPECTIVE, BaseGameSOCState[BOARD, Resource, PERSPECTIVE]]
//    //
//    //    implicit val settlementCost = Cost[Resource, BuildSettlementMove](ResourceSet(Wood, Brick, Wheat, Sheep))
//    //
//    //    implicit val settlementNextMoves: GetPossibleActions[BuildSettlementMove, BaseGameSOCState[BOARD, Resource, PERSPECTIVE]]
//
//
//    //    SOCGame.builder[BaseGameSOCState[BOARD, Resource, PERSPECTIVE]]
//    //      .addMove[BuildSettlementMove](
//    //        ~>[RegularMoves].apply{ (state, move: BuildSettlementMove) =>
//    //          Some(state.buildSettlement(move, Some(CatanSet.empty)))}) // TODO add correct costs
//    //      .addMove[BuildCityMove](
//    //        ~>[RegularMoves].apply{ (state, move: BuildCityMove) =>
//    //          Some(state.buildCity(move, CatanSet.empty))})
//    //      .addMove[BuildRoadMove](
//    //        ~>[RegularMoves].apply{ (state, move: BuildRoadMove) =>
//    //          Some(state.buildRoad(move, Some(CatanSet.empty)))})
//    //      .addMove[PortTradeMove](
//    //        ~> [RegularMoves].apply { (state, move: PortTradeMove) => Some(state) /*.doPortTrade(move)*/})
//
//    //      .addTransition[PortTradeMove, RegularMoves] { case (state, move: PortTradeMove) => state /*.doPortTrade(move)*/}
//    //      .addTransition[BuyDevelopmentCard, RegularMoves] { case (state, move: BuyDevelopmentCardsMoveResult) => state.buyDevelopmentCard(move) }
//    //      .addTransition[PlayKnightMove, RollDiceMove :: HNil] {
//    //        case (state, move: PlayKnightMoveResult[Resource]) if !state.rolledDice =>
//    //          state.playDevelopmentCard(move)
//    //      }
//    //      .addTransition[EndTurnMove, RollDiceMove :: PlayDevelopmentCardMoves] { case (state, _) => state.incrementTurn/*.setRollDice(true)*/ }
//
//  }
//}
//
//
////
////  val state: BaseGameSOCState[BaseBoardConfiguration, Resource, PerfectInfoDevInv[Resource]] = null
////  implicit val mod = SOCState.socStateModifier[BaseBoardConfiguration, Resource, PerfectInfoDevInv[Resource]]
////
////  implicit val publicMod = SOCState.socStateModifier[BaseBoardConfiguration, Resource, PublicInfoDevInv[Resource]]
//
////  implicit def bankFactory[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]]: SOCStateFieldGenerator[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE], Resources] = {case (_, _) =>  ResourceSet.fullBank}
////  implicit def robberLocation[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]]:  SOCStateFieldGenerator[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE], SOCRobberLocation] = { case(board, _) =>
////    SOCRobberLocation(board.hexesWithNodes.find(_.hex.getResourceAndNumber.isEmpty).get.node)
////  }
////  implicit def dCardsInDeck[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]]:  SOCStateFieldGenerator[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE], SOCDevelopmentCardsInDeck] = DevelopmentCardSOCState.fieldGenerator(25)
////  implicit def stateFactory[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]]: SOCStateFactory[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]] = new SOCStateFactory[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]] {
////    override def create(boardConf: BOARD, playerIds: Seq[Int])(implicit boardGenerator: BoardGenerator[BOARD], invHelperFactory: InventoryHelperFactory[Resource, PERSPECTIVE]): BaseGameSOCState[BOARD, PERSPECTIVE] = {
////      SOCStateFactory[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE]](boardConf, playerIds.toList).apply(BaseGameSOCState[BOARD, PERSPECTIVE] _)
////    }
////  }
////
////
////
////  def baseGame[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE] = {
////    SOCGame.builder[BOARD, Resource, PERSPECTIVE, PerfectInfo, BaseGameSOCState]
////      .addMove[BuildSettlementMove] {
////        case (state, move: BuildSettlementMove) => (state.buildSettlement(move), )
////      }
////
////  }
////
////
////
////
////
//
//
////
////      val settlementAction = BuildSettlementAction[BOARD, Resource, PERSPECTIVE, PerfectInfo, BaseGameSOCState](5, ResourceSet(Wood, Brick, Wheat, Sheep))
////    val roadAction = BuildRoadAction[BOARD, Resource, PERSPECTIVE, PerfectInfo, BaseGameSOCState](15, ResourceSet(Wood, Brick))
////      val cityAction = BuildCityAction[BOARD, Resource, PERSPECTIVE, PerfectInfo, BaseGameSOCState](4, ResourceSet(Ore, Ore, Ore, Wheat, Wheat))
////
////      val initialPlacementAction = InitialPlacementAction[BOARD, STATE](settlementAction, roadAction)
////      val endTurnAction = EndTurnAction[BOARD, Resource, STATE]
////      val portTradeAction = PortTradeAction[BOARD, STATE]
////
////      SOCGame.builder[BOARD, Resource, PERSPECTIVE, PerfectInfo, BaseGameSOCState]
////        .addAction { case (state, bsm: BuildSettlementMove) =>
////          (state.buildSettlement(bsm, Some(ResourceSet(Wood, Brick, Wheat, Sheep))), Map.empty)}
////        .addAction { case (state, brm: BuildRoadMove) =>
////          (state.buildRoad(brm, Some( ResourceSet(Wood, Brick))), Map.empty)}
////        .addAction { case (state, bcm: BuildCityMove) =>
////          (state.buildCity(bcm, ResourceSet(Ore, Ore, Ore, Wheat, Wheat)), Map.empty)}
////
////
////
////
////
////        .addAction(settlementAction) { case (m, state) =>
////          (state.buildSettlement(m, Some(settlementAction.cost)), Map.empty)}
////        .addAction(cityAction) { case (m: BuildCityMove, state) =>
////          (state.buildCity(m, cityAction.cost), Map.empty)}
////        .addAction(roadAction) { case (m: BuildRoadMove, state) =>
////          (state.buildRoad(m, Some(roadAction.cost)), Map.empty)}
////        .addAction(initialPlacementAction) { case (m: InitialPlacementMove, state) =>
////          (state.placeInitialPlacement(m), Map.empty)}
////        .addAction(endTurnAction) { case (_:EndTurnMove, state) =>
////          (state.incrementTurn, Map.empty)}
////        .addAction(portTradeAction) { case (m: PortTradeMove, state) =>
////          (state.doPortTrade(m), Map.empty)}
////        .initGame(playerIds.head, initialPlacementAction)(boardConf, playerIds)
////
////  val boardConf: BaseBoardConfiguration = BaseCatanBoard.randomBoard()
////
////  val a = game[BaseBoardConfiguration, PerfectInfoInv[Resource]](boardConf, (1 to 4).toList)
////
////  val gameActionMoves = a.getAllPlayerActions(a.currentPlayer).flatMap { action =>
////    action.getAllPossibleMovesForState(a.state, a.getInventories(a.currentPlayer), a.currentPlayer)
////  }
//
//
////
////
////  val settlementAction = BuildSettlementAction[BASE](5, ResourceSet(Wood, Brick, Wheat, Sheep))
////  val roadAction = BuildRoadAction[BASE](15, ResourceSet(Wood, Brick))
////  val cityAction = BuildCityAction[BASE](4, ResourceSet(Ore, Ore, Ore, Wheat, Wheat))
////
////  val initialPlacementAction = InitialPlacementAction[BASE](settlementAction, roadAction)
////  val rollDiceAction = RollDiceAction[BASE](null, null)
////  val endTurnAction = EndTurnAction[BASE]
////  val portTradeAction = PortTradeAction[BASE]
////
////  val robberAction = RobberAction[BASE](null)
////  val discardAction = DiscardAction[BASE](7)
////
////  val playKnightAction = PlayKnightAction[BASE](14, robberAction)
////  val playMonopolyAction = PlayMonopolyAction[BASE](2, null)
////  val playRoadBuilderAction = PlayRoadBuilderAction[BASE](2, roadAction)
////  val playYopAction = PlayYearOfPlentyAction[BASE](2)
////
////  val buyDevelopmentCardAction = BuyDevelopmentCardAction[BASE](ResourceSet(Ore, Wheat, Sheep), null, playKnightAction, playMonopolyAction, playRoadBuilderAction, playYopAction)
////
////  def buildTradePortPhase[A <: SOCMove[A]]: MoveTransition[A, BASE] = {
////    case (_, state) if state.canPlayCard => Seq(settlementAction, roadAction, cityAction, portTradeAction, buyDevelopmentCardAction, playYopAction, playRoadBuilderAction, playMonopolyAction, playKnightAction, endTurnAction)
////    case _ => Seq(settlementAction, roadAction, cityAction, portTradeAction, buyDevelopmentCardAction, endTurnAction)
////  }
////  def preRollPhase[A <: PlayDevelopmentCardMove[A]]: MoveTransition[A, BASE] = {
////    case (_, state) if state.canRollDice => Seq(rollDiceAction)
////  }
////  def rollDicePhase: MoveTransition[RollDiceMove, BASE] = {
////    case (RollDiceResult(_, Roll(7)), state) if !state.expectingDiscard.isEmpty => Seq(discardAction)
////    case (RollDiceResult(_, Roll(7)), _) => Seq(robberAction)
////  }
////  def playDevelopmentCardMoveTransition[A <: PlayDevelopmentCardMove[A]]: MoveTransition[A, BASE] = preRollPhase[A].orElse(buildTradePortPhase[A])
////
////  val moveTransitionMap = MoveTransitionMap.builder.addMoveTransition(initialPlacementAction) {
////    case (_, state) if state.currentPlayer == state.firstPlayerId && settlementAction.getAmountForPlayer(state.board, state.currentPlayer) > 1 => Seq(rollDiceAction)
////    case _ => Seq(initialPlacementAction)
////  }.addMoveTransition(discardAction) {
////    case (_, state) if !state.expectingDiscard.isEmpty => Seq(discardAction)
////    case _ => Seq(robberAction)
////  }.addMoveTransition(endTurnAction) {
////    case (_, state) if state.canPlayCard => Seq(rollDiceAction, playYopAction, playRoadBuilderAction, playMonopolyAction, playKnightAction)
////    case _ => Seq(rollDiceAction)
////  }.addMoveTransition(rollDiceAction) (rollDicePhase.orElse(buildTradePortPhase[RollDiceMove]))
////    .addMoveTransition(robberAction)(buildTradePortPhase)
////    .addMoveTransition(settlementAction)(buildTradePortPhase)
////    .addMoveTransition(cityAction)(buildTradePortPhase)
////    .addMoveTransition(roadAction)(buildTradePortPhase)
////    .addMoveTransition(portTradeAction)(buildTradePortPhase)
////    .addMoveTransition(buyDevelopmentCardAction)(buildTradePortPhase)
////    .addMoveTransition(playKnightAction)(playDevelopmentCardMoveTransition)
////    .addMoveTransition(playYopAction)(playDevelopmentCardMoveTransition)
////    .addMoveTransition(playRoadBuilderAction)(playDevelopmentCardMoveTransition)
////    .addMoveTransition(playMonopolyAction)(playDevelopmentCardMoveTransition)
////    .create(initialPlacementAction)
//
//
