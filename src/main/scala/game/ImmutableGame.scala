package game

import game.ImmutableGame.FolderOp
import shapeless.ops.coproduct.ZipConst
import shapeless.ops.hlist.{FillWith, Union}
import shapeless.ops.{coproduct, hlist}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Poly0, Poly1}
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

  final case class MergeApply[M <: Coproduct, S <: HList, A <: HList, MA <: Coproduct](a: A) {
    def apply(dummy: Boolean = false)(implicit mergeZipper: coproduct.ZipWith.Aux[A, M, MA]) = new ImmutableGameBuilder[M, S, A, MA](a)
  }

  def merge[M <: Coproduct, MO <: Coproduct, S <: HList, SO <: HList, A <: HList, AO <: HList, MA <: Coproduct, MAO <: Coproduct]
  (builder: ImmutableGameBuilder[M, S, A, MA])
  (implicit mUnion: coproduct.ExtendLeftBy.Aux[Moves, M, MO],
   sUnion: hlist.Union.Aux[State, S, SO],
   prepend: hlist.Prepend.Aux[Actions0, A, AO],
   maExtend: coproduct.ExtendLeftBy.Aux[MoveActions0, MA, MAO],
   mergeZipper: coproduct.ZipWith.Aux[AO, MO, MAO]
  ): ImmutableGameBuilder[MO, SO, AO, MAO] = {
    new ImmutableGameBuilder[MO, SO, AO, MAO](prepend.apply(actionList, builder.actionList))
  }

  def build[ZCO <: Coproduct](implicit zipConst: ZipConst.Aux[State, MoveActions0, ZCO], moveFolder: coproduct.Folder.Aux[ImmutableGame.FolderOp.type, ZCO, State]) = new ImmutableGame[State, Moves] {
    override protected type Actions = Actions0
    override protected type MoveActions = MoveActions0
    override protected type StateMoveActions = ZCO
    override protected val actions: Actions = actionList
    override protected implicit val moveZipper: coproduct.ZipWith.Aux[Actions, Moves, MoveActions] = zipper
    override protected implicit val stateZipper: coproduct.ZipConst.Aux[State, MoveActions, StateMoveActions] = zipConst
    override protected implicit val folder: coproduct.Folder.Aux[ImmutableGame.FolderOp.type, StateMoveActions, State] = moveFolder
  }
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

    implicit val initInt: Case0[Int] = at(0)
    implicit val initDouble: Case0[Double] = at(0.0)
    implicit val initString: Case0[String] = at("")
    implicit def initList[A]: Case0[List[A]] = at(List.empty[A])
    implicit def initMap[K, V]: Case0[Map[K, V]] = at(Map.empty[K, V])
    implicit def initOpt[A]: Case0[Option[A]] = at(None)
    implicit def initSet[A, T: Numeric]: Case0[InventorySet[A, T]] = at(InventorySet.empty[A, T])
    implicit def initHList[L <: HList](implicit fill: FillWith[InitializeOp.type, L]): Case0[L] = at[L](HList.fillWith[L](InitializeOp))
    implicit def initObj[A, Repr <: HList](implicit gen: Generic.Aux[A, Repr], c: Case0[Repr]): Case0[A] = at[A] {
      gen.from(c.apply())

    }

  }
}


