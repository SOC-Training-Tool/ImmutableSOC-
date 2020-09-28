package soc.moves2

import soc.board.{BoardConfiguration, BoardGenerator, CatanBoard}
import soc.inventory._
import soc.inventory.resources.{Gain, Lose, SOCTransactions}
import util.MapWrapper

trait SOCMove[A <: SOCMove[A]] {
  private[moves2] def getMove: A = this.asInstanceOf[A]
  def player: Int
}
trait SOCMoveResult[A <: SOCMove[A]] {
  def move: A
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[A]]
}
trait PerfectInformationSOCMove[A <: SOCMove[A]] extends SOCMove[A] with SOCMoveResult[A] {
  val move = this.getMove
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[A]] = playerIds.map(id => id -> this).toMap
}

case class GameActionMove[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], A <: SOCMove[A]](action: GameAction[BOARD, II, STATE, A], move: A) {
  def getMoveResult[I <: InventoryHelper[II, I], R <: SOCMoveResult[A]](state: STATE[I])(implicit moveResultProvider: MoveResultProvider[BOARD, II, STATE, A, R]): GameActionMoveResult[BOARD, II, STATE, A, R] = GameActionMoveResult[BOARD, II, STATE, A, R](action, moveResultProvider.getMoveResult(move, state))
}

case class GameActionMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], A <: SOCMove[A], R <: SOCMoveResult[A]](action: GameAction[BOARD, II, STATE, A], moveResult: R) {
  val move = GameActionMove[BOARD, II, STATE, A](action, moveResult.move.getMove)
}
trait GameAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], A <: SOCMove[A]] {
  type R <: SOCMoveResult[A]
  implicit def moveResultProvider: MoveResultProvider[BOARD, II, STATE, A, R]

  def getMoveResult[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE]](move: GameActionMove[BOARD, II, STATE, A], state: STATE[PERSPECTIVE]): GameActionMoveResult[BOARD, II, STATE, A, R] = move.getMoveResult[PERSPECTIVE, R](state)
  def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean
  def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[A]

  final def getAllPossibleMovesForState[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[GameActionMove[BOARD, II, STATE, A]] = {
    if (canDoAction(state, inv, position)) getAllMoves(state, inv, position).map(GameActionMove(this, _)) else Nil
  }
}

trait PerfectInformationMoveGameAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], A <: PerfectInformationSOCMove[A]] extends GameAction[BOARD, II, STATE, A] {
  override type R = A
  override implicit val moveResultProvider: MoveResultProvider[BOARD, II, STATE, A, A] = MoveResultProvider.perfectInformationMoveProvider[BOARD, II, STATE, A]
}

trait MoveResultProvider[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], M <: SOCMove[M], R <: SOCMoveResult[M]] {
  def getMoveResult[I <: InventoryHelper[II, I]](move: M, state: STATE[I]): R
}
object MoveResultProvider {

  def perfectInformationMoveProvider[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], M <: PerfectInformationSOCMove[M]]: MoveResultProvider[BOARD, II, STATE, M, M] = new MoveResultProvider[BOARD, II, STATE, M, M] {
    override def getMoveResult[I <: Inventory[II, I]](move: M, state: STATE[I]): M = move
  }
  implicit class MoveResultProviderTransformer[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: SOCState[BOARD, II, P, STATE[P]], A <: SOCMove[A], R <: SOCMoveResult[A]](a: MoveResultProvider[BOARD, II, STATE, A, R]) {
    def transform[T <: SOCMove[T], TR <: SOCMoveResult[T]](f: T => A, h: R => TR): MoveResultProvider[BOARD, II, STATE, T, TR] = new MoveResultProvider[BOARD, II, STATE, T, TR] {
      override def getMoveResult[I <: InventoryHelper[II, I]](move: T, state: STATE[I]): TR = {
        h(a.getMoveResult[I](f(move), state))
      }
    }
  }
}

case class SOCTurn(t: Int)
case class SOCPlayerPointsMap(m: Map[Int, Int]) extends MapWrapper[Int, Int]

trait SOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: SOCState[BOARD, II, PERSPECTIVE, STATE]] {
  def board: CatanBoard[BOARD]
  def bank: CatanSet[II, Int]
  def turn: SOCTurn
  def inventoryHelper: PERSPECTIVE
  def playerPoints: SOCPlayerPointsMap

  val playerInventories: Map[Int, PERSPECTIVE#INV] = inventoryHelper.playerInventories
  val playerIds = playerInventories.keys.toList.sorted
  val numPlayers = playerIds.length
  val currentPlayer = playerIds.toIndexedSeq.apply(turn.t % numPlayers)
  def nextPlayer(playerId: Int): Int = {
    val indexOf = playerIds.indexOf(playerId)
    playerIds.drop(indexOf + 1).headOption.getOrElse(playerIds.min)
  }
  def previousPlayer(playerId: Int): Int = {
    val indexOf = playerIds.indexOf(playerId)
    playerIds.dropRight(numPlayers - indexOf).lastOption.getOrElse(playerIds.max)
  }

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

  def updatePoints(playerPoints: SOCPlayerPointsMap): STATE
  def updateInvHelper(inventoryHelper: PERSPECTIVE): STATE
  def updateBank(bank: CatanSet[II, Int]): STATE
  def incrementTurn : STATE
}



object SOCStateFactory {

  def apply[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: SOCState[BOARD, II, PERSPECTIVE, STATE]]


}

trait SOCStateFactory[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: SOCState[BOARD, II, PERSPECTIVE, STATE]] {
  def create(boardConf: BOARD, playerIds: Seq[Int])(implicit boardGenerator: BoardGenerator[BOARD], invHelperFactory: InventoryHelperFactory[II, PERSPECTIVE]): STATE
}

object SOCState {
  val BANK_PLAYER_ID = -1
}




