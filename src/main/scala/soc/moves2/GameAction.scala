package soc.moves2

import shapeless._
import shapeless.ops.hlist.Replacer.Aux
import shapeless.ops.hlist.{Modifier, RemoveAll, Replacer, SelectAll, Selector}
import soc.board.{BoardConfiguration, CatanBoard}
import soc.inventory.resources.{Gain, Lose, SOCTransactions}
import soc.inventory.{InventoryItem, PerfectInfoInventory, _}
import util.MapWrapper

//case class GameAction[
//  R <: SOCMoveResult,
//  II <: InventoryItem,
//  PerfectInfo <: PerfectInfoInventory[II, PerfectInfo],
//  STATE <: HList](implicit moveGenerator: MoveGenerator[STATE, PerfectInfo, R#A], canDoAction: CanDoAction[STATE, PerfectInfo, R#A], canDoMove: CanDoMove[STATE, PerfectInfo, R#A]) {
//
//  final def getAllMovesForState(state: STATE, inv: PerfectInfo, pos: Int): Seq[R#A] = {
//    if (canDoAction(state, inv, pos))
//      moveGenerator.getAllMoves(state, inv, pos).filter(canDoMove(state, inv, _))
//    else Nil
//  }
//}

//case class GameAction[R <: SOCMoveResult]()

trait GameActionListBuilder[Actions <: HList,
  II <: InventoryItem,
  PerfectInfo <: PerfectInfoInventory[II, PerfectInfo],
  STATE <: HList] extends DepFn0 { type Out <: HList }

object GameActionListBuilder {

  type Aux[Actions <: HList,
    II <: InventoryItem,
    PerfectInfo <: PerfectInfoInventory[II, PerfectInfo],
    STATE <: HList,
    Out0] = GameActionListBuilder[Actions, II, PerfectInfo, STATE] { type Out = Out0 }

  implicit def builder[R <: SOCMoveResult,
    T <: HList,
    II <: InventoryItem,
    PerfectInfo <: PerfectInfoInventory[II, PerfectInfo],
    STATE <: HList](implicit moveGenerator: MoveGenerator[STATE, PerfectInfo, R#A],
                    canDoAction: CanDoAction[STATE, PerfectInfo, R#A],
                    canDoMove: CanDoMove[STATE, PerfectInfo, R#A],
                    builder: GameActionListBuilder[T, II, PerfectInfo, STATE]
                   ): Aux[R :: T, II, PerfectInfo, STATE, GameAction[R] :: builder.Out] = {
    new GameActionListBuilder[R :: T, II, PerfectInfo, STATE] {
      type Out = GameAction[R] :: builder.Out
      override def apply(): GameAction[R] :: builder.Out = ???
    }
  }


}



trait GameResult[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, A <: SOCMove] {

}

trait GetGameActions[MOVES <: HList, STATE <: HList] {
  type Out <: HList

  def apply: Out
}

object GetGameActions {
  type Aux[Ms <: HList, STATE <: HList, Out0 <: HList] = GetGameActions[Ms, STATE] {type Out = Out0}

  def apply[Ms <: HList, STATE <: HList](implicit gga: GetGameActions[Ms, STATE]) = gga

  implicit def getGameActions[A, T <: HList, STATE <: HList](implicit ev: A <:< SOCMove, getGameActions: GetGameActions[T, STATE], gameAction: GameAction.Move[STATE, A]): GetGameActions.Aux[A :: T, STATE, GameAction.Move[STATE, A] :: getGameActions.Out] = {
    new GetGameActions[A :: T, STATE] {
      override type Out = GameAction.Move[STATE, A] :: getGameActions.Out

      override def apply: Out = gameAction :: getGameActions.apply
    }
  }

  implicit def getNil: Aux[HNil, _, HNil] = {
    new GetGameActions[HNil, _] {
      override type Out = HNil

      override def apply: Out = HNil
    }
  }
}

case class GameActionMove[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, GA <: GameAction[BOARD, II, STATE, GA]](action: GA, move: GA#A) {
  def getMoveResult[I <: InventoryHelper[II, I]](state: STATE[BOARD, II, I])(implicit moveResultProvider: MoveResultProvider[BOARD, II, STATE, GA#A, GA#R]): GameActionMoveResult[BOARD, II, STATE, GA] = GameActionMoveResult[BOARD, II, STATE, GA](action, moveResultProvider.getMoveResult(move, state))

}

case class GameActionMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, GA <: GameAction[BOARD, II, STATE, GA]](action: GA, moveResult: GA#R) {
  val move: GameActionMove[BOARD, II, STATE, GA] = GameActionMove[BOARD, II, STATE, GA](action, moveResult.move.getMove)
}

//trait GameAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, GA <: GameAction[BOARD, II, STATE, GA]] { this: GA =>
//  type A <: SOCMove
//  type R <: SOCMoveResult[A]
//  type OpsImplicits[P <: InventoryHelper[II, P]]
//
//  implicit def moveResultProvider: MoveResultProvider[BOARD, II, STATE, A, R]
//
//  def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[BOARD, II, PERSPECTIVE], inv: PerfectInfo, position: Int)(implicit ops: OpsImplicits[PERSPECTIVE]): Boolean
//
//  def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[BOARD, II, PERSPECTIVE], inv: PerfectInfo, position: Int)(implicit ops: OpsImplicits[PERSPECTIVE]): Seq[GA#A]
//
//  final def getAllPossibleMovesForState[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[BOARD, II, PERSPECTIVE], inv: PerfectInfo, position: Int)(implicit ops: OpsImplicits[PERSPECTIVE]): Seq[GameActionMove[BOARD, II, STATE, GA]] = {
//    if (canDoAction(state, inv, position)) getAllMoves(state, inv, position).map(GameActionMove[BOARD, II, STATE, GA](this, _)) else Nil
//  }
//}

trait PerfectInformationMoveGameAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList] extends GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE] {
  override type A <: PerfectInformationSOCMove[A]
  override type R = A

  //override implicit val moveResultProvider: MoveResultProvider[BOARD, II, STATE, A, R] = MoveResultProvider.perfectInformationMoveProvider[BOARD, II, STATE, A]
}

trait MoveResultProvider[S, M <: SOCMove] {
  type R <: SOCMoveResult.Move[M]

  def getMoveResult(move: M, state: S): R
}

object MoveResultProvider {

  def perfectInformationMoveProvider[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, M <: PerfectInformationSOCMove]: MoveResultProvider[BOARD, II, STATE, M, M] = new MoveResultProvider[BOARD, II, STATE, M, M] {
    override def getMoveResult[I <: Inventory[II, I]](move: M, state: STATE[BOARD, II, I]): M = move
  }

  implicit class MoveResultProviderTransformer[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, A <: SOCMove[A], R <: SOCMoveResult[A]](a: MoveResultProvider[BOARD, II, STATE, A, R]) {
    def transform[T <: SOCMove[T], TR <: SOCMoveResult[T]](f: T => A, h: R => TR): MoveResultProvider[BOARD, II, STATE, T, TR] = new MoveResultProvider[BOARD, II, STATE, T, TR] {
      override def getMoveResult[I <: InventoryHelper[II, I]](move: T, state: STATE[BOARD, II, I]): TR = {
        h(a.getMoveResult[I](f(move), state))
      }
    }
  }

}

trait DependsOn[S, D <: HList] {
  def get[T](s: S)(implicit selector: Selector[D, T]): T

  def update[T](t: T, s: S)(implicit replacer: Replacer.Aux[D, T, T, (T, D)]): S

}

object DependsOn {

  def apply[S <: HList, D <: HList](implicit dependsOn: DependsOn[S, D]): DependsOn[S, D] = dependsOn

  implicit def dependsOn[S <: HList, D <: HList, R <: HList](implicit ra: RemoveAll.Aux[S, D, (D, R)]): DependsOn[S, D] = new DependsOn[S, D] {

    override def get[T](s: S)(implicit selector: Selector[D, T]): T = selector.apply(ra.apply(s)._1)

    override def update[T](t: T, s: S)(implicit replacer: Replacer.Aux[D, T, T, (T, D)]): S = {
      val (d, rem) = ra.apply(s)
      val (_, d1) = replacer.apply(d, t)
      ra.reinsert((d1, rem))
    }
  }

  implicit class InnerDependency[S, D <: HList](dp: DependsOn[S, D]) {

    def innerDependency[D1 <: HList](implicit inner: DependsOn[D, D1]): DependsOn[S, D1] = new DependsOn[S, D1] {

      override def get[T](s: S)(implicit selector: Selector[D1, T]): T = {
        implicit val sel = new Selector[D, T] {
          override def apply(t: D): T = inner.get(t)
        }
        dp.get(s)
      }

      override def update[T](t: T, s: S)(implicit replacer: Aux[D1, T, T, (T, D1)]): S = {
        implicit val rep: Replacer.Aux[D, T, T, (T, D)] = new Replacer[D, T, T] {
          override type Out = (T, D)

          override def apply(t: D, u: T): Out = {
            val d = inner.update(u, t)
            (u, d)
          }
        }
        dp.update(t, s)
      }
    }
  }
}

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
      def isBankTransaction(t: SOCTransactions[II]) = t match {
        case Gain(p, _) if p == SOCState.BANK_PLAYER_ID => true
        case Lose(p, _) if p == SOCState.BANK_PLAYER_ID => true
        case _ => false
      }

      val bankTransactions = transactions.filter(isBankTransaction)
      val playerTransactions = transactions.filterNot(isBankTransaction)
      //TODO make sure transactions come in correct order
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


//object SOCStateFactory {
//
//  def apply[BOARD <: BoardConfiguration: BoardGenerator, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE]](board: BOARD, players: List[Int]) = new SOCStateFactoryBuilder[BOARD, II, PERSPECTIVE, STATE](board, players)
//  type FG[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE], FIELD] = SOCStateFieldGenerator[BOARD, II, PERSPECTIVE, STATE, FIELD]
//}
//class SOCStateFactoryBuilder[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE]] private[moves2] (val boardConf: BOARD, val players: List[Int])(implicit bg: BoardGenerator[BOARD]) {
//
//  val board = bg.apply(boardConf)
//
//  type RS = STATE[PERSPECTIVE]
//
//  def apply[E1](f: (CatanBoard[BOARD], E1) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1]): RS = f(board, fg1.apply(board, players))
//
//  def apply[E1, E2](f: (CatanBoard[BOARD], E1, E2) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players))
//  }
//
//  def apply[E1, E2, E3](f: (CatanBoard[BOARD], E1, E2, E3) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4](f: (CatanBoard[BOARD], E1, E2, E3, E4) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5]): RS = {
//    val board = bg.apply(boardConf)
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12], fg13: FG[BOARD, II, PERSPECTIVE, STATE, E13]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players), fg13.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12], fg13: FG[BOARD, II, PERSPECTIVE, STATE, E13], fg14: FG[BOARD, II, PERSPECTIVE, STATE, E14]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players), fg13.apply(board, players), fg14.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12], fg13: FG[BOARD, II, PERSPECTIVE, STATE, E13], fg14: FG[BOARD, II, PERSPECTIVE, STATE, E14], fg15: FG[BOARD, II, PERSPECTIVE, STATE, E15]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players), fg13.apply(board, players), fg14.apply(board, players), fg15.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12], fg13: FG[BOARD, II, PERSPECTIVE, STATE, E13], fg14: FG[BOARD, II, PERSPECTIVE, STATE, E14], fg15: FG[BOARD, II, PERSPECTIVE, STATE, E15], fg16: FG[BOARD, II, PERSPECTIVE, STATE, E16]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players), fg13.apply(board, players), fg14.apply(board, players), fg15.apply(board, players), fg16.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17) => RS)(implicit fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12], fg13: FG[BOARD, II, PERSPECTIVE, STATE, E13], fg14: FG[BOARD, II, PERSPECTIVE, STATE, E14], fg15: FG[BOARD, II, PERSPECTIVE, STATE, E15], fg16: FG[BOARD, II, PERSPECTIVE, STATE, E16], fg17: FG[BOARD, II, PERSPECTIVE, STATE, E17]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players), fg13.apply(board, players), fg14.apply(board, players), fg15.apply(board, players), fg16.apply(board, players), fg17.apply(board, players))
//  }
//
//  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18](f: (CatanBoard[BOARD], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18) => RS)(fg1: FG[BOARD, II, PERSPECTIVE, STATE, E1], fg2: FG[BOARD, II, PERSPECTIVE, STATE, E2], fg3: FG[BOARD, II, PERSPECTIVE, STATE, E3], fg4: FG[BOARD, II, PERSPECTIVE, STATE, E4], fg5: FG[BOARD, II, PERSPECTIVE, STATE, E5], fg6: FG[BOARD, II, PERSPECTIVE, STATE, E6], fg7: FG[BOARD, II, PERSPECTIVE, STATE, E7], fg8: FG[BOARD, II, PERSPECTIVE, STATE, E8], fg9: FG[BOARD, II, PERSPECTIVE, STATE, E9], fg10: FG[BOARD, II, PERSPECTIVE, STATE, E10], fg11: FG[BOARD, II, PERSPECTIVE, STATE, E11], fg12: FG[BOARD, II, PERSPECTIVE, STATE, E12], fg13: FG[BOARD, II, PERSPECTIVE, STATE, E13], fg14: FG[BOARD, II, PERSPECTIVE, STATE, E14], fg15: FG[BOARD, II, PERSPECTIVE, STATE, E15], fg16: FG[BOARD, II, PERSPECTIVE, STATE, E16], fg17: FG[BOARD, II, PERSPECTIVE, STATE, E17], fg18: FG[BOARD, II, PERSPECTIVE, STATE, E18]): RS = {
//    f(board, fg1.apply(board, players), fg2.apply(board, players), fg3.apply(board, players), fg4.apply(board, players), fg5.apply(board, players), fg6.apply(board, players), fg7.apply(board, players), fg8.apply(board, players), fg9.apply(board, players), fg10.apply(board, players), fg11.apply(board, players), fg12.apply(board, players), fg13.apply(board, players), fg14.apply(board, players), fg15.apply(board, players), fg16.apply(board, players), fg17.apply(board, players), fg18.apply(board, players))
//  }
//}
//trait SOCStateFieldGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, P, STATE], FIELD] {
//  def apply(board: CatanBoard[BOARD], players: List[Int]): FIELD
//}
//trait SOCStateFactory[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[P] <: SOCState[BOARD, II, PERSPECTIVE, STATE]] {
//  def create(boardConf: BOARD, playerIds: Seq[Int])(implicit boardGenerator: BoardGenerator[BOARD], invHelperFactory: InventoryHelperFactory[II, PERSPECTIVE]): STATE[PERSPECTIVE]
//}


