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

object SOCGame {

  def build[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](f: SOCGameFactory[BOARD, II, PERSPECTIVE, PerfectInfo, STATE] => ()) = {
    val factory = new SOCGameFactory[BOARD, II, PERSPECTIVE, PerfectInfo, STATE]
    f(factory)
  }
}

class SOCGameFactory[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList] {

  trait GameAction[R <: SOCMoveResult] {
    def getAllMovesForState(state: STATE, inv: PerfectInfo, pos: Int): Seq[R#A]
  }

  implicit def GameActionWrapper[R <: SOCMoveResult](implicit moveGenerator: MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, R#A],
                                                     canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, R#A],
                                                     canDoMove: CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, R#A]): TypeWrapper[SOCMoveResult, GameAction, R] = new TypeWrapper[SOCMoveResult, GameAction, R] {
    override def apply(): GameAction[R] = (state: STATE, inv: PerfectInfo, pos: Int) =>
      if (canDoAction(state, inv, pos))
        moveGenerator.getAllMoves(state, inv, pos).filter(canDoMove(state, inv, _))
      else Nil
  }

  trait GameContextBuilder[Actions <: HList, MOVES <: HList] extends DepFn1[Actions] {
    override type Out = Map[GameAction[_], Function[(STATE, SOCMoveResult), (STATE, Map[Int, Seq[GameAction[_]]])]]
  }

  object GameContextBuilder {

    def apply[ACTIONS <: HList](implicit gcb: GameContextBuilder[ACTIONS, ACTIONS]): GameContextBuilder[ACTIONS, ACTIONS] = gcb

    implicit def contextBuilder[ACTIONS <: HList, H <: SOCMoveResult, T <: HList](implicit nextMove: NextMove[BOARD, II, PERSPECTIVE, GameAction, ACTIONS, STATE, H], applyMoveResult: ApplyMoveResult[BOARD, II, PERSPECTIVE, H, STATE], nextContext: GameContextBuilder[ACTIONS, T]): GameContextBuilder[ACTIONS, GameAction[H] :: T] = (actions: ACTIONS) => {
      val (action, func) = nextMove.apply(actions)
      val f: Function[(STATE, SOCMoveResult), (STATE, Map[Int, Seq[GameAction[_]]])] = { tup =>
        val (s: STATE, r: H) = tup
        (applyMoveResult(s, r), func(s, r))
      }
      nextContext.apply(actions) + (action -> f)
    }

    implicit def hNil[ACTIONS <: HList]: GameContextBuilder[ACTIONS, HNil] = (_: ACTIONS) => Map.empty
  }


  class SOCGameBuilder[ACTIONS <: HList](actions: ACTIONS, m: Map[GameAction[_], Function[(STATE, SOCMoveResult), (STATE, Map[Int, Seq[GameAction[_]]])]] = Map.empty) {

    def build(implicit contextBuilder: GameContextBuilder[ACTIONS, ACTIONS]) = new SOCGameBuilder[ACTIONS](actions, contextBuilder.apply(actions))

    def startGame[InitialMove <: SOCMoveResult](ev: Selector[ACTIONS, GameAction[InitialMove]]) = {

    }
  }

  def apply[Moves <: HList](implicit hlw: HListWrapper[SOCMoveResult, GameAction, Moves]): SOCGameBuilder[hlw.Out] = new SOCGameBuilder[hlw.Out](hlw.apply)
}

//game
//actions = game.getAllPlayerActions -> Map[Int, GameAction[_, B]]
//moves = actions.view.mapValues(_.getMoves)
//move = moves(player).head
//newGame = game.doMove(player, move)
//newGame.playerActions