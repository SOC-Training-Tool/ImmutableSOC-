package soc.gametype

import shapeless.ops.hlist.Selector
import shapeless.{::, DepFn1, HList, HNil}
import soc.board.{BaseBoardConfiguration, BoardConfiguration}
import soc.inventory.InventoryHelper.PerfectInfoInv
import soc.inventory._
import soc.moves2.SOCState._
import soc.moves2.build.BuildSettlementMove
import soc.moves2.{SOCState => _, _}
import util.hlist.{HListWrapper, TypeWrapper}

class SOCGame[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](state: STATE,
                                                                                                                                                                                       moveMap: Map[Int, Seq[GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, _]]]) {
  def getAllMovesForState(inv: PerfectInfo, pos: Int): Seq[GameActionMove[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, _]] = moveMap.getOrElse(pos, Nil).flatMap(_.getAllMovesForState(state, inv, pos))

}

object SOCGame {

  def build[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](f: SOCGameFactory[BOARD, II, PERSPECTIVE, PerfectInfo, STATE] => ()) = {
    val factory = new SOCGameFactory[BOARD, II, PERSPECTIVE, PerfectInfo, STATE]
    f(factory)
  }
}

class SOCGameFactory[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList] {

  type GA[R <: SOCMoveResult] = GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, R]

  implicit def GameActionWrapper[R <: SOCMoveResult](implicit moveGenerator: MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, R#A],
                                                     cdA: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, R#A],
                                                     cdM: CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, R#A],
                                                     updateState: UpdateState[BOARD, II, PERSPECTIVE, R, STATE],
                                                     resultProvider: MoveResultProvider.Aux[BOARD, II, PERSPECTIVE, STATE, R]): TypeWrapper[SOCMoveResult, GA, R] = new TypeWrapper[SOCMoveResult, GA, R] {
    override def apply(): GA[R] = new GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, R] {

      override def canDoAction(state: STATE, inv: PerfectInfo, pos: Int): Boolean = cdA.apply(state, inv, pos)

      override def canDoMove(state: STATE, inv: PerfectInfo, move: R#A): Boolean = cdM.apply(state, inv, move)

      override def getAllMoves(state: STATE, inv: PerfectInfo, pos: Int): Seq[R#A] = moveGenerator.getAllMoves(state, inv, pos)

      override def applyMove(state: STATE, move: R#A): (R, STATE) = {
        val result = resultProvider.getMoveResult(move, state)
        (result, updateState(state, result))
      }
    }
  }

  trait GameContextBuilder[Actions <: HList, MOVES <: HList] extends DepFn1[Actions] {
    override type Out = Map[GameAction[_], Function[(STATE, SOCMoveResult), (STATE, Map[Int, Seq[GameAction[_]]])]]
  }

  object GameContextBuilder {

    def apply[ACTIONS <: HList](implicit gcb: GameContextBuilder[ACTIONS, ACTIONS]): GameContextBuilder[ACTIONS, ACTIONS] = gcb

    implicit def contextBuilder[ACTIONS <: HList, H <: SOCMoveResult, T <: HList](implicit s: Selector[ACTIONS, GameAction[H]], applyMoveResult: ApplyMoveResult[BOARD, II, PERSPECTIVE, H, STATE, GameAction, ACTIONS], nextContext: GameContextBuilder[ACTIONS, T]) = new GameContextBuilder[ACTIONS, GameAction[H] :: T] {
      override def apply(actions: ACTIONS): Map[GameAction[_], (STATE, SOCMoveResult) => (STATE, Map[Int, Seq[GameAction[_]]])] = {
        val f: Function[(STATE, SOCMoveResult), (STATE, Map[Int, Seq[GameAction[_]]])] = applyMoveResult.apply(actions)
        nextContext.apply(actions) + (s.apply(actions) -> f)
      }
    }

    implicit def hNil[ACTIONS <: HList]: GameContextBuilder[ACTIONS, HNil] = (_: ACTIONS) => Map.empty
  }


  class SOCGameBuilder[ACTIONS <: HList](actions: ACTIONS, m: Map[GameAction[_], Function[(STATE, SOCMoveResult), (STATE, Map[Int, Seq[GameAction[_]]])]] = Map.empty) {

    def build(implicit contextBuilder: GameContextBuilder[ACTIONS, ACTIONS]) = new SOCGameBuilder[ACTIONS](actions, contextBuilder.apply(actions))

    def startGame[InitialMove <: SOCMoveResult](ev: Selector[ACTIONS, GameAction[InitialMove]]) = {

    }
  }

  def apply[Moves <: HList](implicit hlw: HListWrapper[SOCMoveResult, GA, Moves]): SOCGameBuilder[hlw.Out] = new SOCGameBuilder[hlw.Out](hlw.apply)
}

//game
//actions = game.getAllPlayerActions -> Map[Int, GameAction[_, B]]
//moves = actions.view.mapValues(_.getMoves)
//move = moves(player).head
//newGame = game.doMove(player, move)
//newGame.playerActions