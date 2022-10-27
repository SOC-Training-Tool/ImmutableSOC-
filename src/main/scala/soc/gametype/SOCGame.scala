//package soc.gametype
//
//import shapeless.ops.hlist.{Selector, ToTraversable}
//import shapeless.{::, DepFn1, HList, HNil}
//import soc.board.{BaseBoardConfiguration, BoardConfiguration}
//import soc.inventory.InventoryHelper.PerfectInfoInv
//import soc.inventory._
//import soc.state.SOCState._
//import soc.moves2.build.BuildSettlementMove
//import soc.moves2._
//import soc.state.{SOCState => _}
//import util.hlist.{HListWrapper, TypeWrapper}
//
//class SOCGame[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[_, _, _] <: HList, ACTIONS <: HList](actions: ACTIONS,
//                                                                                                                                                                                                         state: STATE[BOARD, II, PerfectInfo])(implicit toLisT: ToTraversable.Aux[ACTIONS, List, GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, _]]) {
//  private val actionsList: List[GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE[BOARD, II, PERSPECTIVE], _]] = toLisT.apply(actions)
//
//  def getActionsForState(inv: PerfectInfo, pos: Int): Seq[GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, _]] = actionsList.filter(_.canDoAction(state, inv, pos))
//
//  def getAllMovesForState(inv: PerfectInfo, pos: Int): Seq[GameActionMove[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, _]] = getActionsForState(inv, pos).flatMap(_.getAllMovesForState(state, inv, pos))
//
//  def doMove[A <: SOCMove](move: A)(implicit selector: Selector[ACTIONS, GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, A]]): (A#R, STATE) = ??? //selector.apply(actions).applyMove(state, move)
//
//}
//
//object SOCGame {
//
//  def build[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](f: SOCGameFactory[BOARD, II, PERSPECTIVE, PerfectInfo, STATE] => ()) = {
//    val factory = new SOCGameFactory[BOARD, II, PERSPECTIVE, PerfectInfo, STATE]
//    f(factory)
//  }
//}
//
//class SOCGameFactory[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList] {
//
//  type GA[A <: SOCMove] = GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, A]
//
//  implicit def GameActionWrapper[A <: SOCMove](implicit moveGenerator: MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, A],
//                                               cdA: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, A],
//                                               cdM: CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, A],
//                                               resultProvider: MoveResultProvider[BOARD, II, PERSPECTIVE, STATE, A]): TypeWrapper[SOCMove, GA, A] = new TypeWrapper[SOCMove, GA, A] {
//    override def apply(): GA[A] = new GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, A] {
//
//      override def canDoAction(state: STATE, inv: PerfectInfo, pos: Int): Boolean = cdA.apply(state, inv, pos)
//
//      override def canDoMove(state: STATE, inv: PerfectInfo, move: A): Boolean = cdM.apply(state, inv, move)
//
//      override def getAllMoves(state: STATE, inv: PerfectInfo, pos: Int): Seq[A] = moveGenerator.getAllMoves(state, inv, pos)
//
//      override def getMoveResult(state: STATE, move: A): A#R = resultProvider.getMoveResult(move, state)
//    }
//  }
//
//  class SOCGameBuilder[ACTIONS <: HList](actions: ACTIONS) {
//
//    def startGame[InitialMove <: SOCMove](implicit ev: Selector[ACTIONS, GA[InitialMove]], toList: ToTraversable[ACTIONS, List]) = {
//
//    }
//  }
//
//  def apply[Moves <: HList](implicit hlw: HListWrapper[SOCMove, GA, Moves]): SOCGameBuilder[hlw.Out] = new SOCGameBuilder[hlw.Out](hlw.apply)
//}
//
////game
////actions = game.getAllPlayerActions -> Map[Int, GameAction[_, B]]
////moves = actions.view.mapValues(_.getMoves)
////move = moves(player).head
////newGame = game.doMove(player, move)
////newGame.playerActions