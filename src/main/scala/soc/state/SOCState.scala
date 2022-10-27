package soc.state

import shapeless.ops.hlist.Modifier
import shapeless.{::, DepFn0, DepFn2, HList, HNil, TypeCase, Typeable}
import soc.board.{BoardConfiguration, CatanBoard}
import soc.inventory.resources.{Gain, Lose, SOCTransactions}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem}
import soc.moves2.SOCMoveResult
import util.{DependsOn, MapWrapper}

case class SOCTurn(t: Int)
case class SOCPlayerPointsMap(m: Map[Int, Int]) extends MapWrapper[Int, Int]

object SOCState {
  type SOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE]] =
    PERSPECTIVE :: CatanBoard[BOARD] :: CatanSet[II, Int] :: SOCTurn :: SOCPlayerPointsMap :: HNil

  val BANK_PLAYER_ID = -1
  //implicit def playerPointsMapFieldGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE]]: SOCStateFieldGenerator[BOARD, II, PERSPECTIVE, STATE, SOCPlayerPointsMap] = {case(_, p) => SOCPlayerPointsMap(p.map(i => i -> 0).toMap) }
  //implicit def turnFieldGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE]]: SOCStateFieldGenerator[BOARD, II, PERSPECTIVE, STATE, SOCTurn] = {case(_, _) => SOCTurn(0) }

  implicit class SOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCState[BOARD, II, PERSPECTIVE]]) {

    val turn: Int = dep.get[SOCTurn](state).t

    def updateTurns(turns: Int): STATE = dep.update(SOCTurn(turns), state)

    val playerPoints: SOCPlayerPointsMap = dep.get(state)

    def updatePoints(playerPoints: SOCPlayerPointsMap): STATE = dep.update(playerPoints, state)

    val inventoryHelper: PERSPECTIVE = dep.get(state)

    def updateInvHelper(inventoryHelper: PERSPECTIVE): STATE = dep.update(inventoryHelper, state)

    val bank: CatanSet[II, Int] = dep.get(state)

    def updateBank(bank: CatanSet[II, Int]): STATE = dep.update(bank, state)

    val board: CatanBoard[BOARD] = dep.get(state)

    val playerInventories: Map[Int, PERSPECTIVE#INV] = inventoryHelper.playerInventories
    val playerIds = playerInventories.keys.toList.sorted
    val numPlayers = playerIds.length
    val currentPlayer = playerIds.toIndexedSeq.apply(turn % numPlayers)

    def nextPlayer(playerId: Int): Int = {
      val indexOf = playerIds.indexOf(playerId)
      playerIds.drop(indexOf + 1).headOption.getOrElse(playerIds.min)
    }

    def previousPlayer(playerId: Int): Int = {
      val indexOf = playerIds.indexOf(playerId)
      playerIds.dropRight(numPlayers - indexOf).lastOption.getOrElse(playerIds.max)
    }

    lazy val incrementTurn: STATE = updateTurns(turn + 1)

    def updateTransactions(transactions: List[SOCTransactions[II]]): STATE = {
      import SOCState.BANK_PLAYER_ID
      def isBankTransaction(t: SOCTransactions[II]) = t match {
        case Gain(BANK_PLAYER_ID, _) | Lose(BANK_PLAYER_ID, _) => true
        case _ => false
      }

      val bankTransactions = transactions.filter(isBankTransaction)
      val playerTransactions = transactions.filterNot(isBankTransaction)
      //TODO make sure transactions come in correct order (add then subtract)
      updateBank(bankTransactions.foldLeft(bank) {
        case (b, Gain(_, s)) => b.add(s)
        case (b, Lose(_, s)) => b.subtract(s)
      }).updateInvHelper(inventoryHelper.updateInventory(playerTransactions))
    }

    def toPublic(implicit modifier: Modifier[STATE, SOCState[BOARD, II, PERSPECTIVE], SOCState[BOARD, II, PERSPECTIVE#PUBLIC]]): modifier.Out = {
      val f: SOCState[BOARD, II, PERSPECTIVE] => SOCState[BOARD, II, PERSPECTIVE#PUBLIC] = Modifier[SOCState[BOARD, II, PERSPECTIVE], PERSPECTIVE, PERSPECTIVE#PUBLIC].apply(_, _.toPublic)._2
      modifier.apply(state, f)
    }
  }
}

trait UpdateState[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], R <: SOCMoveResult, STATE <: HList] extends DepFn2[STATE, R] {
  override type Out = STATE
}

trait UpdateStateAll[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], MOVE_RESULTS <: HList, STATE <: HList] extends DepFn0 {
  override type Out = PartialFunction[(STATE, SOCMoveResult), STATE]
}

object UpdateState {

  private def addToPartial[U, V, A <: U: Typeable](pf: PartialFunction[U, V])(f: A => V): PartialFunction[U, V] = {
    val aType = TypeCase[A]
    val func: PartialFunction[U, V] = {
      case aType(a) => f(a)
      case r => pf(r)
    }
    func
  }

  def build[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], MOVE_RESULTS <: HList, STATE <: HList](implicit usa: UpdateStateAll[B, I, P, MOVE_RESULTS, STATE]) = usa.apply()

  implicit def updater[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], R <: SOCMoveResult, T <: HList, STATE <: HList](implicit ev: Typeable[(R, STATE)], us: UpdateState[B, I, P, R, STATE], usa: UpdateStateAll[B, I, P, T, STATE]): UpdateStateAll[B, I, P, R :: T, STATE] = new UpdateStateAll[B, I, P, R :: T, STATE] {

    override def apply(): PartialFunction[(STATE, SOCMoveResult), STATE] = {
      addToPartial[(STATE, SOCMoveResult), STATE, (STATE, R)](usa.apply()) {
        case (state, result) =>
          us.apply(state, result)
      }
    }
  }

  implicit def hnil[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], STATE <: HList] = new UpdateStateAll[B, I, P, HNil, STATE] {
    override def apply(): PartialFunction[(STATE, SOCMoveResult), STATE] = PartialFunction.empty
  }
}

trait StateBuilder[STATE <: HList] extends DepFn0 {
  override type Out = STATE
}

trait InitialState[T] extends DepFn0 {
  override type Out = T
}

object StateBuilder {

  implicit def hlist[H, T <: HList](implicit init: InitialState[H], builder: StateBuilder[T]) = new StateBuilder[H :: T] {
    override def apply(): H :: T = init.apply() :: builder.apply()
  }

  implicit def hnil: StateBuilder[HNil] = new StateBuilder[HNil] {
    override def apply(): HNil = HNil
  }

}





