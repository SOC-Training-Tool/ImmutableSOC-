package soc.moves2.developmentcard

import shapeless.{HList, ::}
import soc.board.BoardConfiguration
import soc.inventory._
import soc.moves2.SOCState.SOCState
import soc.moves2._
import soc.moves2.build.{BuildCityMove, CityBoardOps, SOCCityMap}

case class BuyDevelopmentCardMove(player: Int) extends SOCMove

case class BuyDevelopmentCardsMoveResult(player: Int, card: Option[DevelopmentCard]) extends SOCMoveResult[BuyDevelopmentCardMove] {
  override def move: BuyDevelopmentCardMove = BuyDevelopmentCardMove(player)

  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[BuyDevelopmentCardMove]] = playerIds.map {
    case `player` => player -> this
    case p => p -> BuyDevelopmentCardsMoveResult(player, None)
  }.toMap
}

//object BuyDevelopmentCardMove {
//
//  implicit def generator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoDevelopmentCardInventory[II],  STATE[B, I, P] <: HList]: MoveGenerator[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, BuyDevelopmentCardMove] =
//    (_, _, pos) => Seq(BuyDevelopmentCardMove(pos))
//
//  implicit def canDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoDevelopmentCardInventory[II],  STATE[B, I, P] <: HList](implicit cost: Cost[II, BuyDevelopmentCardMove], ops: SOCBaseOps1[BOARD, II, PERSPECTIVE, STATE, SOCDevelopmentCardsInDeck]): CanDoMove[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, BuyDevelopmentCardMove] =
//    (state, perfectInfo, _) => {
//      perfectInfo.canSpend(cost.getCost) && state.developmentCardsLeft.d > 0
//    }
//}


//case class BuyDevelopmentCardAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: DevelopmentCardSOCState[BOARD, II, P, STATE]](cost: CatanSet[II, Int], playDevelopmentCardActions: PlayDevelopmentCardAction[BOARD, II, STATE, _, _]*)(implicit val moveResultProvider: MoveResultProvider[BOARD, II, STATE, BuyDevelopmentCardMove, BuyDevelopmentCardsMoveResult]) extends GameAction[BOARD, II, STATE, BuyDevelopmentCardAction[BOARD, II, STATE]] {
//  val developmentCardDeck = playDevelopmentCardActions.map(_.cardsInDeck).sum
//  override type A = BuyDevelopmentCardMove
//  override type R = BuyDevelopmentCardsMoveResult
//  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
//    val devCardsDeck = state.developmentCardsLeft
//    inv.canSpend(cost) && devCardsDeck.d > 0
//  }
//  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[BuyDevelopmentCardMove] =  List(BuyDevelopmentCardMove(position))
//}

case class SOCDevelopmentCardsInDeck(d: Int)

object DevelopmentCardSOCState {
  //implicit def fieldGenerator[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE]](numCards: Int):  SOCStateFieldGenerator[BOARD, Resource, PERSPECTIVE, BaseGameSOCState[BOARD, PERSPECTIVE], SOCDevelopmentCardsInDeck] = { case(_, _) => SOCDevelopmentCardsInDeck(numCards)}

  import SOCState.SOCStateOps

  implicit class DevelopmentCardSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], STATE[B, I, P] <: HList](state: STATE[BOARD, II, PERSPECTIVE])(implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCDevelopmentCardsInDeck :: SOCState[BOARD, II, PERSPECTIVE]]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val developmentCardsLeft: Int = dep.get[SOCDevelopmentCardsInDeck](state).d

    def updateDevelopmentCardsInDeck(cid: SOCDevelopmentCardsInDeck): STATE[BOARD, II, PERSPECTIVE] = dep.update(cid, state)

    def decrementNumDevelopmentCards: STATE[BOARD, II, PERSPECTIVE] = updateDevelopmentCardsInDeck(SOCDevelopmentCardsInDeck(developmentCardsLeft - 1))

    def buyDevelopmentCard(buy: BuyDevelopmentCardsMoveResult): STATE[BOARD, II, PERSPECTIVE] =
      state.updateInvHelper(state.inventoryHelper.buyDevelopmentCard(buy.player, state.turn.t, buy.card)).decrementNumDevelopmentCards

    def playDevelopmentCard[A <: PlayDevelopmentCardMove](card: PlayDevelopmentCardMoveResult[A]): STATE[BOARD, II, PERSPECTIVE] =
      state.updateInvHelper(state.inventoryHelper.playDevelopmentCard(card.move.player, state.turn.t, card.move.card))

  }

  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], STATE[_, _, _] <: HList](implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCDevelopmentCardsInDeck :: SOCState[BOARD, II, PERSPECTIVE]], cost: Cost[II, BuyDevelopmentCardMove]): ApplyMoveResult[BuyDevelopmentCardsMoveResult, STATE[BOARD, II, PERSPECTIVE]] = {
    (s, m) =>
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      s.buyDevelopmentCard(BuyDevelopmentCardsMoveResult(s.currentPlayer, m.card))
  }

  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoDevelopmentCardInventory[II], STATE[_, _, _] <: HList](implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCDevelopmentCardsInDeck :: SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]], cost: Cost[II, BuyDevelopmentCardMove]): CanDoAction[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, BuyDevelopmentCardMove] = {
    (state, inv, player) =>
      import soc.moves2.RollDiceSOCState.RollDiceSOCStateOps
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      implicit val cityDep = dep.innerDependency[SOCDevelopmentCardsInDeck :: SOCState[BOARD, II, PERSPECTIVE]]
      implicit val rollDep = dep.innerDependency[SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]
      state.rolledDice && state.currentPlayer == player && state.developmentCardsLeft > 0 && inv.canSpend(cost.getCost)
  }

  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoDevelopmentCardInventory[II], STATE[_, _, _] <: HList](implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCDevelopmentCardsInDeck :: SOCState[BOARD, II, PERSPECTIVE]], canDoAction: CanDoAction[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, BuyDevelopmentCardMove]): CanDoMove[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, BuyDevelopmentCardMove] = {
    (state, inv, move) => canDoAction(state, inv, move.player)
  }
}




