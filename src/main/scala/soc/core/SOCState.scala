package soc.core

import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.Modifier
import soc.base.{SOCPlayerPointsMap, SOCTurn}
import soc.board.{BoardConfiguration, CatanBoard}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem}
import soc.inventory.resources.{Gain, Lose, SOCTransactions}
import util.DependsOn

object SOCState {
  type SOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE]] =
    PERSPECTIVE :: CatanBoard[BOARD] :: CatanSet[II, Int] :: SOCTurn :: SOCPlayerPointsMap :: HNil

  val BANK_PLAYER_ID = -1

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