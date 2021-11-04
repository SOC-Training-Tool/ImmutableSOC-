package soc.moves2

import shapeless.{::, HList}
import soc.board.{BoardConfiguration, BoardHex}
import soc.core.Roll
import soc.inventory._
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.moves2.SOCState.SOCState
import soc.moves2.build.BoardOps

case class RollDiceMove(player: Int) extends SOCMove

case class RollDiceResult(player: Int, roll: Roll) extends SOCMoveResult {
  override type A = RollDiceMove

  override def move: RollDiceMove = RollDiceMove(player)

  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, RollDiceResult] = playerIds.map(id => id -> this).toMap
}

object RollDiceMove {

  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList]: MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove] = {
    (_: STATE, _: PerfectInfo, pos: Int) => Seq(RollDiceMove(pos))
  }

  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove] = {
    (state, _, player) =>
      import RollDiceSOCState._
      import SOCState._

      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      !state.rolledDice && state.currentPlayer == player
  }

  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove] = {
    (state, inv, move) => canDoAction(state, inv, move.player)
  }
}


trait RollDiceOps[B, I, P, S] {

  def getVertexBuildingValue: PartialFunction[VertexBuilding, Int]

  def onRoll(rollDiceResult: RollDiceResult, s: S)(f: S => S): S

}

case class SOCCanRollDice(b: Boolean)

object RollDiceSOCState {

  import RobberSOCState.RobberSOCStateOps
  import SOCState.SOCStateOps

  implicit class RollDiceSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]) {
    val rolledDice = dep.get[SOCCanRollDice](state).b

    def setRollDice(b: Boolean): STATE = dep.update(SOCCanRollDice(b), state)
  }

  implicit class DistributeResourcesRollDiceSOCStateOps[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](state: STATE)(implicit boardOps: BoardOps[BOARD, Resource, PERSPECTIVE, STATE], rollOps: RollDiceOps[BOARD, Resource, PERSPECTIVE, STATE], dep: DependsOn[STATE, SOCCanRollDice :: SOCRobberLocation :: SOCState[BOARD, Resource, PERSPECTIVE]]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, Resource, PERSPECTIVE]]
    implicit val rollDep = dep.innerDependency[SOCCanRollDice :: SOCState[BOARD, Resource, PERSPECTIVE]]
    implicit val robberDep = dep.innerDependency[SOCRobberLocation :: SOCState[BOARD, Resource, PERSPECTIVE]]

    def rollDice(rollDiceResult: RollDiceResult): STATE = rollOps.onRoll(rollDiceResult, state)(identity).setRollDice(true)

    def getResourcesGainedOnRoll(roll: Int): Map[Int, Resources] =
      state.board.numberHexes
        .get(roll)
        .fold[Seq[BoardHex]](Nil)(_.filterNot(_.node == state.robberLocation))
        .flatMap { node =>
          node.vertices.flatMap { vertex =>
            boardOps.vertexBuildingMap(state).get(vertex).fold(Seq.empty[(Int, Resource)]) { vb =>
              (0 until rollOps.getVertexBuildingValue(vb)).map(_ => vb.playerId -> node.hex.getResource.get)
            }
          }.groupBy(_._1).view.mapValues(_.map(_._2).foldLeft(ResourceSet.empty[Int]) { case (set: Resources, r: Resource) => set.add(1, r) })
        }.toMap

    def distributeResources(resForPlayers: Map[Int, Resources]): STATE = {
      val totalResourcesCollected: Resources = resForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
      val actualResForPlayers = {
        val resTypes: Seq[Resource] = totalResourcesCollected.getTypes
        val overflowTypes = resTypes.filter(item => !state.bank.contains(totalResourcesCollected.getAmount(item), item))
        resForPlayers.map[Int, Resources] { case (player, resourceSet) =>
          player -> overflowTypes.foldLeft(resourceSet) { case (set, res) => set.subtract(set.getAmount(res), res) }
        }
      }
      val trueTotalCollected = actualResForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
      val gainedResources: List[Gain[Resource]] = state.playerInventories.keys.toSeq.map { player =>
        Gain(player, actualResForPlayers.getOrElse(player, ResourceSet.empty[Int]))
      }.filterNot(_.resourceSet.isEmpty).toList
      state.updateTransactions(Lose(SOCState.BANK_PLAYER_ID, trueTotalCollected) :: gainedResources)
    }
  }
}