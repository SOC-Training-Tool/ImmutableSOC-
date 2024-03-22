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

  def extend[S <: HList](update: ActionExtension[MOVE, S])(implicit union: hlist.Union[S, STATE]) = new ExtendApply[S, union.Out](this, update)

  //  def extend[S <: HList](f: (MOVE, S) => S)(implicit union: hlist.Union[S, STATE]) = extend(new ActionExtension[MOVE, S] {
  //    override def apply(move: MOVE, pre: STATE, post: STATE): STATE = f(move, post)
  //  })

  final case class ExtendApply[S <: HList, U <: HList](original: GameAction[MOVE, STATE], extension: ActionExtension[MOVE, S]) {
    def apply(dummy: Boolean = false)(implicit dep1: DependsOn[U, S], dep2: DependsOn[U, STATE]): GameAction[MOVE, U] = new GameAction[MOVE, U] {
      override def applyMove[GAME_STATE <: HList](move: MOVE, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, U]): GAME_STATE = {
        implicit val depS: DependsOn[GAME_STATE, S] = dep.innerDependency[S]
        implicit val depF: DependsOn[GAME_STATE, STATE] = dep.innerDependency[STATE]
        val pre = depS.getAll(state)
        depS.updateAll(original.applyMove[GAME_STATE](move, state))(d => extension.apply(move, pre, d))
      }
    }
  }
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


