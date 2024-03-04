package game

import shapeless.HList
import shapeless.ops.hlist
import util.DependsOn

trait GameAction[MOVE, STATE <: HList] {

  def applyMove[GAME_STATE <: HList](move: MOVE, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, STATE]): GAME_STATE

  def extend[S <: HList](implicit union: hlist.Union[STATE, S]) = new ExtendApply[S, union.Out](this)

  final case class ExtendApply[S <: HList, U <: HList](original: GameAction[MOVE, STATE]) {
    def apply(update: (MOVE, S) => S)(implicit dep1: DependsOn[U, S], dep2: DependsOn[U, STATE]): GameAction[MOVE, U] = new GameAction[MOVE, U] {
      override def applyMove[GAME_STATE <: HList](move: MOVE, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, U]): GAME_STATE = {
        implicit val depS: DependsOn[GAME_STATE, S] = dep.innerDependency[S]
        implicit val depF: DependsOn[GAME_STATE, STATE] = dep.innerDependency[STATE]
        depS.updateAll(original.applyMove[GAME_STATE](move, state))(d => update(move, d))
      }
    }
  }
}



