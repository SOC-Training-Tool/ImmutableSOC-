package soc.moves2

import shapeless._
import shapeless.ops.hlist.Replacer.Aux
import shapeless.ops.hlist.{Modifier, RemoveAll, Replacer, SelectAll, Selector}
import soc.board.{BoardConfiguration, CatanBoard}
import soc.inventory.resources.{Gain, Lose, SOCTransactions}
import soc.inventory.{InventoryItem, PerfectInfoInventory, _}
import util.MapWrapper

case class GameActionMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo, STATE <: HList, R <: SOCMoveResult](private val gameAction: GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, R], move: R#A) {
  def getMove: SOCMove = move
  def canDoMove(state: STATE, perfectInfo: PerfectInfo): Boolean = gameAction.canDoMove(state, perfectInfo, move)
  def applyMove(state: STATE): (SOCMoveResult, STATE) = gameAction.applyMove(state, move) //NOTE for imperfect info moves results will not e consistent
}

trait GameAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo, STATE <: HList, R <: SOCMoveResult] {
  def canDoAction(state: STATE, inv: PerfectInfo, pos: Int): Boolean

  def canDoMove(state: STATE, inv: PerfectInfo, move: R#A): Boolean

  def getAllMoves(state: STATE, inv: PerfectInfo, pos: Int): Seq[R#A]

  final def getAllMovesForState(state: STATE, inv: PerfectInfo, pos: Int): Seq[GameActionMove[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, R]] = {
    if (canDoAction(state, inv, pos)) getAllMoves(state, inv, pos).filter(canDoMove(state, inv, _)).map(m => GameActionMove(this, m))
    else Nil
  }

  def applyMove(state: STATE, move: R#A): (R, STATE)
}



trait MoveResultProvider[BOARD, II, PERSPECTIVE, S, M <: SOCMove] {
  type R <: SOCMoveResult.Move[M]

  def getMoveResult(move: M, state: S): R
}

object MoveResultProvider {

  type Aux[B, I, P, S, R0 <: SOCMoveResult] = MoveResultProvider[B, I, P, S, R0#A] { type R = R0 }

  implicit def perfectInformationMoveProvider[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList, M <: PerfectInformationSOCMove[M]]: Aux[BOARD, II, PERSPECTIVE, STATE, M] = new MoveResultProvider[BOARD, II, PERSPECTIVE, STATE, M] {
    override type R = M
    override def getMoveResult(move: M, state: STATE): R = move
  }
//
//  implicit class MoveResultProviderTransformer[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, A <: SOCMove[A], R <: SOCMoveResult[A]](a: MoveResultProvider[BOARD, II, STATE, A, R]) {
//    def transform[T <: SOCMove[T], TR <: SOCMoveResult[T]](f: T => A, h: R => TR): MoveResultProvider[BOARD, II, STATE, T, TR] = new MoveResultProvider[BOARD, II, STATE, T, TR] {
//      override def getMoveResult[I <: InventoryHelper[II, I]](move: T, state: STATE[BOARD, II, I]): TR = {
//        h(a.getMoveResult[I](f(move), state))
//      }
//    }
//  }

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


