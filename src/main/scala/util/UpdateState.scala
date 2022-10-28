package util

import shapeless.{::, DepFn0, DepFn2, HList, HNil, TypeCase, Typeable}
import soc.board.BoardConfiguration
import soc.core.SOCMoveResult
import soc.inventory.{InventoryHelper, InventoryItem}

trait UpdateState[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], R <: SOCMoveResult, STATE <: HList] extends DepFn2[STATE, R] {
  override type Out = STATE
}

trait UpdateStateAll[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], MOVE_RESULTS <: HList, STATE <: HList] extends DepFn0 {
  override type Out = PartialFunction[(STATE, SOCMoveResult), STATE]
}

object UpdateState {

  private def addToPartial[U, V, A <: U : Typeable](pf: PartialFunction[U, V])(f: A => V): PartialFunction[U, V] = {
    val aType = TypeCase[A]
    val func: PartialFunction[U, V] = {
      case aType(a) => f(a)
      case r => pf(r)
    }
    func
  }

  def build[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], MOVE_RESULTS <: HList, STATE <: HList](implicit usa: UpdateStateAll[B, I, P, MOVE_RESULTS, STATE]): PartialFunction[(STATE, SOCMoveResult), STATE] = usa.apply()

  implicit def updater[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], R <: SOCMoveResult, T <: HList, STATE <: HList](implicit ev: Typeable[(R, STATE)], us: UpdateState[B, I, P, R, STATE], usa: UpdateStateAll[B, I, P, T, STATE]): UpdateStateAll[B, I, P, R :: T, STATE] = new UpdateStateAll[B, I, P, R :: T, STATE] {

    override def apply(): PartialFunction[(STATE, SOCMoveResult), STATE] = {
      addToPartial[(STATE, SOCMoveResult), STATE, (STATE, R)](usa.apply()) {
        case (state, result) =>
          us.apply(state, result)
      }
    }
  }

  implicit def hnil[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], STATE <: HList]: UpdateStateAll[B, I, P, HNil, STATE] = new UpdateStateAll[B, I, P, HNil, STATE] {
    override def apply(): PartialFunction[(STATE, SOCMoveResult), STATE] = PartialFunction.empty
  }
}