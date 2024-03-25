package game

import shapeless.HList
import shapeless.ops.hlist
import util.DependsOn

trait GameAction[MOVE, STATE <: HList] {
  self =>

  def applyMove[GAME_STATE <: HList](move: MOVE, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, STATE]): GAME_STATE

  def compose[M](f: M => MOVE): GameAction[M, STATE] = new GameAction[M, STATE] {
    override def applyMove[GAME_STATE <: HList](move: M, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, STATE]): GAME_STATE = {
      self.applyMove(f(move), state)
    }
  }

  final case class ExtendApply[S <: HList](original: GameAction[MOVE, STATE]) {
    def apply[U <: HList](extension: ActionExtension[MOVE, S])(implicit union: hlist.Union.Aux[S, STATE, U], dep1: DependsOn[U, S], dep2: DependsOn[U, STATE]): GameAction[MOVE, U] = {
      new GameAction[MOVE, U] {
        override def applyMove[GAME_STATE <: HList](move: MOVE, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, U]): GAME_STATE = {
          implicit val depS: DependsOn[GAME_STATE, S] = dep.innerDependency[S]
          implicit val depF: DependsOn[GAME_STATE, STATE] = dep.innerDependency[STATE]
          val pre = depS.getAll(state)
          depS.updateAll(original.applyMove[GAME_STATE](move, state))(d => extension.apply(move, pre, d))
        }
      }
    }
  }

  def extend[S <: HList] = ExtendApply[S](this)

}

object GameAction {

  def apply[MOVE, S <: HList](f: (MOVE, S) => S) = new GameAction[MOVE, S] {
    override def applyMove[GAME_STATE <: HList](move: MOVE, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, S]): GAME_STATE = {
      dep.updateAll(state)(f(move, _))
    }
  }

  def extension[S <: HList](f: S => S) = new ActionExtension[Nothing, S] {
    override def apply(move: Nothing, pre: STATE, post: STATE): STATE = f(post)
  }
}

trait ActionExtension[-MOVE, S <: HList] {

  type STATE = S

  def apply(move: MOVE, pre: STATE, post: STATE): STATE
}


