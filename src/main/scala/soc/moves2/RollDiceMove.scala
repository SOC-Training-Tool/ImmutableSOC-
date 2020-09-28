package soc.moves2

import soc.board.BoardConfiguration
import soc.core.Roll
import soc.dice.Dice
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.{CatanSet, InventoryHelper, PerfectInfoInventory, Resource}
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.moves2.build.{CitySOCState, SettlementSOCState}

case class RollDiceMove(player: Int) extends SOCMove[RollDiceMove]
case class RollDiceResult(player: Int, roll: Roll) extends SOCMoveResult[RollDiceMove] {
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[RollDiceMove]] =  playerIds.map(id => id -> this).toMap
  override def move: RollDiceMove = RollDiceMove(player)
}
case class RollDiceAction[BOARD <: BoardConfiguration, STATE[P] <: RollDiceSOCState[BOARD, P, STATE[P]]](dice: Dice)(implicit val moveResultProvider: MoveResultProvider[BOARD, Resource, STATE, RollDiceMove, RollDiceResult]) extends GameAction[BOARD, Resource, STATE, RollDiceMove] {
  override type R = RollDiceResult
  override def canDoAction[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = true
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[RollDiceMove] = List(RollDiceMove(position))
}

trait RollDiceSOCState[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: RollDiceSOCState[BOARD, PERSPECTIVE, STATE]] extends SOCState[BOARD, Resource, PERSPECTIVE, STATE] {
  this: SettlementSOCState[BOARD, Resource, PERSPECTIVE, STATE] with CitySOCState[BOARD, Resource, PERSPECTIVE, STATE] with RobberSOCState[BOARD, Resource, PERSPECTIVE, STATE] =>

  def getResourcesGainedOnRoll(roll: Int): Map[Int, Resources] = {
    board.hexesWithNodes.filter { boardHex =>
      boardHex.hex.getNumber.fold(false)(_.number == roll) && boardHex.node != robberLocation
    }.flatMap { node =>
      node.vertices.flatMap { vertex =>
        settlements.get(vertex).fold {
          cities.get(vertex).fold(Seq.empty[(Int, Resource)])(c => Seq(c.playerId -> node.hex.getResource.get, c.playerId -> node.hex.getResource.get))
        }(c => Seq(c.playerId -> node.hex.getResource.get))
      }.groupBy(_._1).view.mapValues(_.map(_._2).foldLeft(ResourceSet.empty[Int]){ case (set: Resources, r: Resource) => set.add(1, r) })
    }.toMap
  }

  def applyRoll(rollResult: RollDiceResult): STATE = {
    val diceRoll = rollResult.roll
    val resForPlayers: Map[Int, Resources] = getResourcesGainedOnRoll(diceRoll.number)
    val totalResourcesCollected: Resources = resForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
    val actualResForPlayers = {
      val resTypes: Seq[Resource] = totalResourcesCollected.getTypes
      val overflowTypes = resTypes.filter(item => !bank.contains(totalResourcesCollected.getAmount(item), item))
      resForPlayers.map[Int, Resources] { case (player, resourceSet) =>
        player -> overflowTypes.foldLeft(resourceSet) { case (set, res) => set.subtract(set.getAmount(res), res) }
      }
    }
    val trueTotalCollected = actualResForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
    val gainedResources: List[Gain[Resource]] = playerInventories.keys.toSeq.map { player =>
      Gain(player, actualResForPlayers.getOrElse(player, ResourceSet.empty[Int]))
    }.filterNot(_.resourceSet.isEmpty).toList
    updateTransactions(Lose(SOCState.BANK_PLAYER_ID, trueTotalCollected) :: gainedResources)
  }
}

//case class RollDiceAction[BOARD <: BoardConfiguration, STATE[P] <: RollDiceSOCState[BOARD, P, STATE]](dice: Dice) extends GameAction[BOARD, Resource, STATE, RollDiceMove] {
//  override def applyMoveToState[PERSPECTIVE <: Inventory[PERSPECTIVE]](moveResult: RollDiceResult, state: GameState[PERSPECTIVE, BOARD]): GameState[PERSPECTIVE, BOARD] = {
//    val diceRoll = moveResult.roll
//    if (diceRoll.number == state.boardRules.robberRoll) {
//      val newExpectingDiscard = state.players.players.filter { case (_, state) => state.numCards > state.rules.discardLimit}.keys.toList
//      return state.transition(
//        phase = if (newExpectingDiscard.isEmpty) GamePhase.MoveRobber else GamePhase.Discard,
//        expectingDiscard = newExpectingDiscard
//      ).state
//    }
//
//    val resForPlayers: Map[Int, Resources] = state.board.getResourcesGainedOnRoll(diceRoll.number)
//    val totalResourcesCollected: Resources = resForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
//    val actualResForPlayers = if (!state.resourceBank.contains(totalResourcesCollected)) {
//      val overflowTypes = {
//        val total = state.resourceBank.subtract(totalResourcesCollected)
//        Resource.list.filter(res => total.contains(res))
//      }
//      resForPlayers.map[Int, Resources] { case (player, resourceSet) =>
//        player -> overflowTypes.foldLeft(resourceSet) { case (set, res) =>
//          set.subtract(set.getAmount(res), res)
//        }
//      }
//    } else resForPlayers
//    val trueTotalCollected = actualResForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
//
//    val newTransactions: List[Gain] = state.players.getPlayers.map { player =>
//      Gain(player.position, actualResForPlayers.getOrElse(player.position, ResourceSet()))
//    }.filterNot(_.resourceSet.isEmpty).toList
//
//    state.transition(
//      resourceBank = state.resourceBank.subtract(trueTotalCollected),
//      phase = GamePhase.BuyTradeOrEnd,
//      transactions = newTransactions
//    ).state
//  }
//  override def canDoAction[PERSPECTIVE <: Inventory[PERSPECTIVE]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = true
//
//  override def getAllMoves[PERSPECTIVE <: Inventory[PERSPECTIVE]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[RollDiceMove] = List(RollDiceMove(position))
//}