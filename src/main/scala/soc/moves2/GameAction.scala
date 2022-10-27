package soc.moves2

import shapeless._
import shapeless.ops.hlist.Replacer.Aux
import shapeless.ops.hlist.{Modifier, RemoveAll, Replacer, SelectAll, Selector}
import soc.board.{BoardConfiguration, CatanBoard}
import soc.inventory.resources.{Gain, Lose, SOCTransactions}
import soc.inventory.{InventoryItem, PerfectInfoInventory, _}
import util.MapWrapper

case class GameActionMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo, STATE <: HList, A <: SOCMove](private val gameAction: GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, A], move: A) {
  def getMove: SOCMove = move
  def canDoMove(state: STATE, perfectInfo: PerfectInfo): Boolean = gameAction.canDoMove(state, perfectInfo, move)
  def getMoveResult(state: STATE): A#R = gameAction.getMoveResult(state, move) //NOTE for imperfect info moves results will not e consistent
}

trait GameAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo, STATE <: HList, A <: SOCMove] {
  def canDoAction(state: STATE, inv: PerfectInfo, pos: Int): Boolean

  def canDoMove(state: STATE, inv: PerfectInfo, move: A): Boolean

  def getAllMoves(state: STATE, inv: PerfectInfo, pos: Int): Seq[A]

  final def getAllMovesForState(state: STATE, inv: PerfectInfo, pos: Int): Seq[GameActionMove[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, A]] = {
    if (canDoAction(state, inv, pos)) getAllMoves(state, inv, pos).filter(canDoMove(state, inv, _)).map(m => GameActionMove(this, m))
    else Nil
  }

  def getMoveResult(state: STATE, move: A): A#R
}


trait MoveResultProvider[BOARD, II, PERSPECTIVE, S, M <: SOCMove] {
  def getMoveResult(move: M, state: S): M#R
}

object MoveResultProvider {

  implicit def perfectInformationMoveProvider[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList, M <: PerfectInformationSOCMove[M]]: MoveResultProvider[BOARD, II, PERSPECTIVE, STATE, M] = new MoveResultProvider[BOARD, II, PERSPECTIVE, STATE, M] {
    override def getMoveResult(move: M, state: STATE): M = move
  }
//
//  implicit class MoveResultProviderTransformer[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList, A <: SOCMove[A], R <: SOCMoveResult[A]](a: MoveResultProvider[BOARD, II, STATE, A, R]) {
//    def transform[T <: SOCMove[T], TR <: SOCMoveResult[T]](f: T => A, h: R => TR): MoveResultProvider[BOARD, II, STATE, T, TR] = new MoveResultProvider[BOARD, II, STATE, T, TR] {
//      override def getMoveResult[I <: InventoryHelper[II, I]](move: T, state: STATE[BOARD, II, I]): TR = {
//        h(a.getMoveResult[I](f(move), state))
//      }
//    }
//  }

//}

//  implicit class StateActionOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE) {
//    def getActions[ALL_ACTIONS <: HList, PerfectInfo]: List[GameAction[BOARD, II, PERSPECTIVE, PerfectInfo, STATE, _]]
//  }
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

