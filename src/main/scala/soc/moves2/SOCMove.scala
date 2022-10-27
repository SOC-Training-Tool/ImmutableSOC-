package soc.moves2

import shapeless.ops.hlist.{SelectAll, Selector, ToTraversable}
import shapeless.{::, DepFn1, DepFn2, HList, TypeCase, Typeable}
import soc.inventory.{CatanSet, InventoryItem}
import soc.moves2.build.BuildSettlementMove
import soc.state.UpdateState
import util.hlist.WrappedSelectAll

import scala.collection.{Factory, mutable}

trait SOCMove {
  type R <: SOCMoveResult

  def player: Int
}

trait SOCMoveResult {
  type A <: SOCMove

  def move: A

  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]]
}

object SOCMoveResult {

  type Move[M <: SOCMove] = SOCMoveResult {type A = M}
}

trait PerfectInformationSOCMove[A0 <: SOCMove] extends SOCMove with SOCMoveResult {
  self: A0 =>
  type A = A0
  type R = A0

  def move: A = self

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



///**
// * applies the move result to the current state resulting in a function of state and move result
// * to the updated state and a map of players to the possible "actions" they can perform
// */
//trait ApplyMoveResult[B, I, P, R <: SOCMoveResult, STATE <: HList, W[_ <: SOCMove], ACTIONS <: HList] extends DepFn1[ACTIONS] {
//  override type Out = (STATE, R) => (STATE, Map[Int, Seq[W[_]]])
//}
//
//object ApplyMoveResult {
//
//  trait A
//  class B extends A
//  val f: B => Int = (_: B) => 2
//
//  def simpleApplyMoveResult[A <: SOCMove, NEXT_ACTIONS <: HList] = new {
//    implicit def gen[B, I, P, STATE <: HList, W[_ <: SOCMove], ALL_ACTIONS <: HList]
//    (implicit sa: WrappedSelectAll[SOCMove, W, ALL_ACTIONS, NEXT_ACTIONS], us: UpdateState[B, I, P, A#R, STATE]): ApplyMoveResult[B, I, P, A#R, STATE, W, ALL_ACTIONS]
//    = new ApplyMoveResult[B, I, P, A#R, STATE, W, ALL_ACTIONS] {
//      override def apply(t: ALL_ACTIONS): (STATE, A#R) => (STATE, Map[Int, Seq[W[_]]]) = { (s: STATE, r: A#R) =>
//        (us.apply(s, r), Map(r.move.player -> sa.traversable(t)))
//      }
//    }
//  }
//}