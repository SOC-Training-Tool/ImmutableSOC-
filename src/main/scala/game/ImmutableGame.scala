package game

import shapeless.ops.coproduct
import shapeless.ops.hlist.Union
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Poly1}
import util.DependsOn

class ImmutableGame[ACTIONS <: HList, STATE <: HList, MOVES <: Coproduct] private(actions: ACTIONS) {

  final case class ApplyMove[C <: Coproduct](zipper: coproduct.ZipWith.Aux[ACTIONS, MOVES, C]) {
    def apply()(implicit zipConst: coproduct.ZipConst[STATE, C]) =
      ApplyMove2[C, zipConst.Out](zipper, zipConst)
  }

  final case class ApplyMove2[C1 <: Coproduct, C2 <: Coproduct](zipper: coproduct.ZipWith.Aux[ACTIONS, MOVES, C1], zipConst: coproduct.ZipConst.Aux[STATE, C1, C2]) {
    implicit val zip = zipper
    implicit val zipC = zipConst

    def apply(move: MOVES, state: STATE)(implicit folder: coproduct.Folder.Aux[ImmutableGame.FolderOp.type, C2, STATE]): STATE =
      move.zipWith(actions).zipConst(state).fold(ImmutableGame.FolderOp)
  }

  def applyMove()(implicit zipper: coproduct.ZipWith[ACTIONS, MOVES]): ApplyMove[zipper.Out] = ApplyMove[zipper.Out](zipper)

  def addAction[S <: HList, M](action: GameAction[M, S])(implicit union: Union[S, STATE]) = {
    new ImmutableGame[GameAction[M, S] :: ACTIONS, union.Out, M :+: MOVES](action :: actions)
  }

}

object ImmutableGame {

  val init = new ImmutableGame[HNil, HNil, CNil](HNil)

  object FolderOp extends Poly1 {
    implicit def moveCase[M, MS <: HList, S <: HList](implicit dep: DependsOn[S, MS]): Case.Aux[((M, GameAction[M, MS]), S), S] =
      at { case (((move, action), state)) => action.applyMove[S](move, state) }
  }
}


