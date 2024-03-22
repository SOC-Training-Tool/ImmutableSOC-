package game

import game.ImmutableGame.FolderOp
import shapeless.PolyDefns.Case0
import shapeless.ops.coproduct.ZipConst
import shapeless.ops.hlist.{FillWith, Union}
import shapeless.ops.{coproduct, hlist}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Poly0, Poly1}
import util.DependsOn

trait ImmutableGame[STATE <: HList, MOVES <: Coproduct] {

  protected type Actions <: HList
  protected type MoveActions <: Coproduct
  protected type StateMoveActions <: Coproduct

  protected val actions: Actions
  protected implicit val moveZipper: coproduct.ZipWith.Aux[Actions, MOVES, MoveActions]
  protected implicit val stateZipper: coproduct.ZipConst.Aux[STATE, MoveActions, StateMoveActions]
  protected implicit val folder: coproduct.Folder.Aux[FolderOp.type, StateMoveActions, STATE]

  def apply[S <: HList](move: MOVES, state: S)(implicit dep: DependsOn[S, STATE]): S = {
    dep.updateAll(state)(s => move.zipWith(actions).zipConst(s).fold(ImmutableGame.FolderOp))
  }
}

class ImmutableGameBuilder[Moves <: Coproduct, State <: HList, Actions0 <: HList, MoveActions0 <: Coproduct]
(val actionList: Actions0)
(implicit zipper: coproduct.ZipWith.Aux[Actions0, Moves, MoveActions0]) {

  def addAction[M, S <: HList]
  (action: GameAction[M, S])
  (implicit union: Union[S, State],
   zipper: coproduct.ZipWith.Aux[GameAction[M, S] :: Actions0, M :+: Moves, (M, GameAction[M, S]) :+: MoveActions0]
  ) = new ImmutableGameBuilder[M :+: Moves, union.Out, GameAction[M, S] :: Actions0, (M, GameAction[M, S]) :+: MoveActions0](action :: actionList)

  final case class BuildApply[C <: Coproduct](zipConst: ZipConst.Aux[State, MoveActions0, C]) {
    def apply(dummy: Boolean = false)(implicit moveFolder: coproduct.Folder.Aux[ImmutableGame.FolderOp.type, C, State]): ImmutableGame[State, Moves] = new ImmutableGame[State, Moves] {
      override protected type Actions = Actions0
      override protected type MoveActions = MoveActions0
      override protected type StateMoveActions = C
      override protected val actions: Actions = actionList
      override protected implicit val moveZipper: coproduct.ZipWith.Aux[Actions, Moves, MoveActions] = zipper
      override protected implicit val stateZipper: coproduct.ZipConst.Aux[State, MoveActions, StateMoveActions] = zipConst
      override protected implicit val folder: coproduct.Folder.Aux[ImmutableGame.FolderOp.type, StateMoveActions, State] = moveFolder
    }
  }

  final case class MergeApply[M <: Coproduct, S <: HList, A <: HList, MA <: Coproduct](a: A) {
    def apply(dummy: Boolean = false)(implicit mergeZipper: coproduct.ZipWith.Aux[A, M, MA]) = new ImmutableGameBuilder[M, S, A, MA](a)
  }

  def merge[M <: Coproduct, S <: HList, A <: HList, MA <: Coproduct]
  (builder: ImmutableGameBuilder[M, S, A, MA])
  (implicit mUnion: coproduct.ExtendLeftBy[Moves, M],
   sUnion: hlist.Union[State, S],
   prepend: hlist.Prepend[Actions0, A],
   maExtend: coproduct.ExtendLeftBy[MoveActions0, MA]
  ): MergeApply[mUnion.Out, sUnion.Out, prepend.Out, maExtend.Out] = {
    new MergeApply[mUnion.Out, sUnion.Out, prepend.Out, maExtend.Out](prepend.apply(actionList, builder.actionList))
  }

  def build(implicit zipConst: ZipConst[State, MoveActions0]) = new BuildApply[zipConst.Out](zipConst)

}

trait StateInitializer[T] extends shapeless.DepFn0 {
  override type Out = T
}

object ImmutableGame {

  def initialize[S <: HList](implicit fillWith: FillWith[InitializeOp.type, S]): S = HList.fillWith[S](InitializeOp)

  def builder = new ImmutableGameBuilder[CNil, HNil, HNil, CNil](HNil)

  object FolderOp extends Poly1 {
    implicit def moveCase[M, MS <: HList, S <: HList](implicit dep: DependsOn[S, MS]): Case.Aux[((M, GameAction[M, MS]), S), S] =
      at { case ((move, action), state) => action.applyMove[S](move, state) }
  }

  object InitializeOp extends Poly0 {
    implicit def initializeState[A](implicit si: StateInitializer[A]): Case0[A] = at(si.apply())

  }
}


