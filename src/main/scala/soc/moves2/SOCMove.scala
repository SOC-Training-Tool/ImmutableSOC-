package soc.moves2

import shapeless.ops.hlist.{SelectAll, ToTraversable}
import shapeless.{DepFn1, DepFn2, HList}
import soc.inventory.{CatanSet, InventoryItem}

trait SOCMove {
  def player: Int
}

object SOCMove {
  implicit class SOCMoveOps[A <: SOCMove](move: A) {
    def getMoveResult[STATE <: HList](state: STATE)(implicit moveResultProvider: MoveResultProvider[STATE, A])
    : moveResultProvider.R = moveResultProvider.getMoveResult(move, state)
  }
}

trait SOCMoveResult {
  type A <: SOCMove

  def move: A

  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]]
}

object SOCMoveResult {

  type Move[M <: SOCMove] = SOCMoveResult {type A = M}
}

trait PerfectInformationSOCMove extends SOCMove with SOCMoveResult {
  self =>
  type A = self.type

  def move = this

  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]] = playerIds.map(id => id -> this).toMap
}

trait MoveGenerator[B, I, P, S, PI, A] {
  def getAllMoves(state: S, perfectInfo: PI, pos: Int): Seq[A]
}

object MoveGenerator {
  def apply[B, I, P, S, PI, A](implicit mg: MoveGenerator[B, I, P, S, PI, A]): MoveGenerator[B, I, P, S, PI, A] = mg
}

trait CanDoAction[B, I, P, S, PI, A <: SOCMove] {
  def apply(state: S, perfectInfo: PI, player: Int): Boolean
}

trait CanDoMove[B, I, P, S, PI, A <: SOCMove] {
  def apply(state: S, perfectInfo: PI, move: A): Boolean
}

trait Cost[II <: InventoryItem, A <: SOCMove] {
  def getCost: CatanSet[II, Int]
}

object Cost {
  def apply[II <: InventoryItem, A <: SOCMove](set: CatanSet[II, Int]): Cost[II, A] = new Cost[II, A] {
    override def getCost: CatanSet[II, Int] = set
  }
}


trait UpdateState[B, I, P, R <: SOCMoveResult, STATE <: HList] extends DepFn2[STATE, R] {
  override type Out = STATE
}

/**
 * applies the move result to the current state resulting in a function of state and move result
 * to the updated state and a map of players to the possible "actions" they can perform
 */
trait ApplyMoveResult[B, I, P, R <: SOCMoveResult, STATE <: HList, W[_ <: SOCMoveResult], ACTIONS <: HList] extends DepFn1[ACTIONS] {
  override type Out = (STATE, R) => (STATE, Map[Int, Seq[W[_]]])
}

object ApplyMoveResult {
  def simpleApplyMoveResult[B, I, P, R <: SOCMoveResult, STATE <: HList, W[_ <: SOCMoveResult], ALL_ACTIONS <: HList, NEXT_ACTIONS <: HList]
  (implicit sa: SelectAll[ALL_ACTIONS, NEXT_ACTIONS], us: UpdateState[B, I, P, R, STATE], toTraversableAux: ToTraversable.Aux[NEXT_ACTIONS, List, W[_]]): ApplyMoveResult[B, I, P, R, STATE, W, ALL_ACTIONS]
  = new ApplyMoveResult[B, I, P, R, STATE, W, ALL_ACTIONS] {
    override def apply(t: ALL_ACTIONS): (STATE, R) => (STATE, Map[Int, Seq[W[_]]]) = { (s: STATE, r: R) =>
      (us.apply(s, r), Map(r.move.player -> sa.apply(t).toList[W[_]]))
    }
  }
}



