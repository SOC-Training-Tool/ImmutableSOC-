package soc.gametype

import soc.board.{BoardConfiguration, BoardGenerator}
import soc.gametype.SOCGame.{ACTION_FUNC, ACTION_MAP}
import soc.inventory.{CatanSet, InventoryHelper, InventoryHelperFactory, InventoryItem}
import soc.moves2.{GameAction, GameActionMoveResult, SOCMove, SOCMoveResult, SOCState, SOCStateFactory}

case class SOCGame[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE] , STATE <: SOCState[BOARD, II, PERSPECTIVE, STATE]](
  private val actionTransitionMap: ACTION_MAP[BOARD, II, PERSPECTIVE, STATE],
  playerActions: Map[Int, Seq[GameAction[BOARD, II, STATE, _]]],
  state: STATE
) {

  def getAllPlayerActions: Map[Int, Seq[GameAction[BOARD, II, STATE, _]]] = playerActions

  def doMove[A <: SOCMove[A], R <: SOCMoveResult[A]](actionResult: GameActionMoveResult[BOARD, II, STATE, A, R]): SOCGame[BOARD, II, PERSPECTIVE, STATE] = {
    val (newState, nextPlayerActions) = actionTransitionMap(actionResult.action)((actionResult.moveResult, state))
    copy(playerActions = nextPlayerActions, state = newState)
  }
}

class SOCGameBuilder[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE[P]]](
  actionMap: ACTION_MAP[BOARD, II, PERSPECTIVE, STATE] = Map.empty
) {

  def addAction[A <: SOCMove[A]](action: GameAction[BOARD, II, STATE, A])(pf: ACTION_FUNC[BOARD, II, PERSPECTIVE, STATE]): SOCGameBuilder[BOARD, II, PERSPECTIVE, STATE]  = {
    require(!actionMap.keys.toList.contains(action), "action already added; cannot have multiple instances of same action")
    new SOCGameBuilder(actionMap + (action -> pf))
  }

  def initGame[A <: SOCMove[A]](startingPlayer: Int, startingAction: GameAction[BOARD, II, STATE, A])(boardConf: BOARD, initBank: CatanSet[II, Int], playerIds: Seq[Int])(
    implicit stateFactory: SOCStateFactory[BOARD, II, PERSPECTIVE, STATE],
         boardGenerator: BoardGenerator[BOARD],
         inventoryHelperFactory: InventoryHelperFactory[II, PERSPECTIVE]
  ): SOCGame[BOARD, II, PERSPECTIVE, STATE] = {
    require(actionMap.keys.toList.contains(startingAction), "init action must already be added")
    SOCGame(actionMap, Map(startingPlayer -> Seq(startingAction)), stateFactory.create(boardConf, initBank, playerIds))
  }
}

object SOCGame {

  type ACTION_FUNC[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: SOCState[BOARD, PERSPECTIVE, II, STATE]] = PartialFunction[(SOCMoveResult[_], STATE), (STATE, Map[Int, Seq[GameAction[BOARD, II, STATE, _]]])]
  type ACTION_MAP[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: SOCState[BOARD, PERSPECTIVE, II, STATE]] = Map[GameAction[BOARD, II, STATE, _], ACTION_FUNC[BOARD, II, PERSPECTIVE, STATE]]

  def builder[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE<: SOCState[BOARD, II, PERSPECTIVE, STATE]]: SOCGameBuilder[BOARD, II, PERSPECTIVE, STATE] = new SOCGameBuilder[BOARD, II, PERSPECTIVE, STATE](Map.empty)

}


//game
//actions = game.getAllPlayerActions -> Map[Int, GameAction[_, B]]
//moves = actions.view.mapValues(_.getMoves)
//move = moves(player).head
//newGame = game.doMove(player, move)