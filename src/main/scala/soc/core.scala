package soc

import game.InventorySet
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil}
import soc.board.{Edge, Vertex}
import soc.inventory.ResourceInventories
import util.DependsOn

package object core {

  case object Wood

  case object Brick

  case object Sheep

  case object Wheat

  case object Ore

  type Resource = Wood.type :+: Brick.type :+: Sheep.type :+: Wheat.type :+: Ore.type :+: CNil

  object Resources {
    val WOOD: Resource = Coproduct[Resource](Wood)
    val BRICK: Resource = Coproduct[Resource](Brick)
    val SHEEP: Resource = Coproduct[Resource](Sheep)
    val WHEAT: Resource = Coproduct[Resource](Wheat)
    val ORE: Resource = Coproduct[Resource](Ore)

    val all: Seq[Resource] = WOOD :: BRICK :: SHEEP :: WHEAT :: ORE :: Nil

  }

  object ResourceSet {
    type ResourceSet[T] = InventorySet[Resource, T]
    type Resources = ResourceSet[Int]

    def apply[T: Numeric](br: T = 0, or: T = 0, sh: T = 0, wh: T = 0, wo: T = 0): ResourceSet[T] = ResourceSet[T](Map[Resource, T](Brick -> br, Wood -> wo, Ore -> or, Sheep -> sh, Wheat -> wh))

    def apply[T: Numeric](resMap: Map[Resource, T]): ResourceSet[T] = InventorySet.fromMap(resMap)

    def apply(resources: Resource*): Resources = InventorySet.fromList(resources.toSeq)

    def empty[T: Numeric]: ResourceSet[T] = InventorySet.empty[Resource, T]
  }

  case object Settlement

  case object City

  case object Road

  type VertexBuilding = Settlement.type :+: City.type :+: CNil

  type EdgeBuilding = Road.type :+: CNil

  object VertexBuilding {

    val SETTLEMENT = Coproduct[VertexBuilding](Settlement)
    val CITY = Coproduct[VertexBuilding](City)
  }

  object EdgeBuilding {
    val ROAD = Coproduct[EdgeBuilding](Road)
  }

  case class PlayerBuilding[BB <: Coproduct](building: BB, player: Int)

  type BoardBuildingState[BB <: Coproduct, T] = Map[T, PlayerBuilding[BB]]
  type VertexBuildingState[BB <: Coproduct] = BoardBuildingState[BB, Vertex]
  type EdgeBuildingState[BB <: Coproduct] = BoardBuildingState[BB, Edge]

  case class Bank[II <: Coproduct](b: InventorySet[II, Int])

  case class Turn(t: Int)

  case class PlayerPoints(points: Map[Int, Int])

  type CORE_STATE[II <: Coproduct, INV[_]] = Bank[II] :: Turn :: PlayerPoints :: INV[II] :: HNil

  implicit class CoreOps[II <: Coproduct, INV[_], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, CORE_STATE[II, INV]], inv: ResourceInventories[II, INV[II]]) {

    val turn: Int = dep.get[Turn](state).t

    def incrementTurn: STATE = dep.update(Turn(turn + 1), state)

    val playerPoints: Map[Int, Int] = dep.get[PlayerPoints](state).points

    def incrementPointForPlayer(player: Int): STATE =
      dep.update(PlayerPoints(playerPoints + (player -> (playerPoints.getOrElse(player, 0) + 1))), state)
    def decrementPointForPlayer(player: Int): STATE =
      dep.update(PlayerPoints(playerPoints + (player -> (playerPoints.getOrElse(player, 0) - 1))), state)

    val bank: InventorySet[II, Int] = dep.get[Bank[II]](state).b

    def addToBank(set: InventorySet[II, Int]): STATE = dep.update(Bank(bank.add(set)), state)

    def subtractFromBank(set: InventorySet[II, Int]): STATE = dep.update(Bank(bank.subtract(set)), state)

    val inventories: INV[II] = dep.get[INV[II]](state)

    val playerIds: List[Int] = inv.players(inventories).toList.sorted
    val numPlayers: Int = playerIds.length
    val currentPlayer: Int = playerIds.toIndexedSeq.apply(turn % numPlayers)

    def nextPlayer(playerId: Int): Int = {
      val indexOf = playerIds.indexOf(playerId)
      playerIds.drop(indexOf + 1).headOption.getOrElse(playerIds.min)
    }

    def previousPlayer(playerId: Int): Int = {
      val indexOf = playerIds.indexOf(playerId)
      playerIds.dropRight(numPlayers - indexOf).lastOption.getOrElse(playerIds.max)
    }

    def updateResourceInventories(transactions: List[SOCTransactions[II]]): STATE = dep.update(inv.update(inventories, transactions), state)

    def updateResourceInventories(transactions: SOCTransactions[II]*): STATE = updateResourceInventories(transactions.toList)


  }


  sealed trait SOCTransactions[II <: Coproduct]

  case class Gain[II <: Coproduct](playerId: Int, resourceSet: InventorySet[II, Int]) extends SOCTransactions[II]

  case class Lose[II <: Coproduct](playerId: Int, resourceSet: InventorySet[II, Int]) extends SOCTransactions[II]

  case class Steal[II <: Coproduct](robber: Int, victim: Int, resourceSet: Option[InventorySet[II, Int]]) extends SOCTransactions[II]


}