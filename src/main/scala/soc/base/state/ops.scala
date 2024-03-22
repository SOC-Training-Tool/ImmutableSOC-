package soc.base.state

import game.InventorySet
import shapeless.ops.coproduct
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Poly1}
import soc.base.actions.{BoardHex, SOCBoard}
import soc.base.{BuildCityMove, BuildRoadMove, BuildSettlementMove}
import soc.board.{Edge, Vertex}
import soc.core.{City, Road, Settlement, VertexBuildingValue}
import soc.inventory.ResourceInventories
import soc.inventory.Transactions.{Gain, Lose, PerfectInfo}
import util.DependsOn
import util.opext.Embedder
import ResourceInventories.ResourceInventoriesOp
import SOCBoard.SOCBoardOps

object ops {

  implicit class TurnOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, Turn :: HNil]) {
    val turn: Int = dep.get[Turn](state).t

    def incrementTurn: STATE = dep.update(Turn(turn + 1), state)
  }

  implicit class PointsOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, PlayerPoints :: HNil]) {
    val playerPoints: Map[Int, Int] = dep.get[PlayerPoints](state).points

    def incrementPointForPlayer(player: Int): STATE =
      dep.update(PlayerPoints(playerPoints + (player -> (playerPoints.getOrElse(player, 0) + 1))), state)

    def decrementPointForPlayer(player: Int): STATE =
      dep.update(PlayerPoints(playerPoints + (player -> (playerPoints.getOrElse(player, 0) - 1))), state)
  }

  implicit class BankOps[Res, STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, Bank[Res] :: HNil]) {
    val bank: InventorySet[Res, Int] = dep.get[Bank[Res]](state).b

    def addToBank(set: InventorySet[Res, Int]): STATE = dep.update(Bank(bank.add(set)), state)

    def subtractFromBank(set: InventorySet[Res, Int]): STATE = dep.update(Bank(bank.subtract(set)), state)
  }

  implicit class BankInvOps[Res, Inv[_], STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, Bank[Res] :: Inv[Res] :: HNil],
   inv: ResourceInventories[Res, PerfectInfo[Res], Inv]) {

    implicit val bankDep = dep.innerDependency[Bank[Res] :: HNil]

    def payToBank(player: Int, set: InventorySet[Res, Int]): STATE = {
      dep.updateWith[Inv[Res]](state)(_.update(Coproduct[PerfectInfo[Res]](Lose(player, set))))
        .addToBank(set)
    }

    def getFromBank(player: Int, set: InventorySet[Res, Int]): STATE = {
      dep.updateWith[Inv[Res]](state)(_.update(Coproduct[PerfectInfo[Res]](Gain(player, set))))
        .subtractFromBank(set)
    }
  }

  implicit class BuildSettlementStateOps[VB <: Coproduct, STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, VertexBuildingState[VB] :: PlayerPoints :: HNil],
   settleEmbedder: Embedder[VB, Settlement.type :+: CNil]) {

    implicit val pointsDep = dep.innerDependency[PlayerPoints :: HNil]

    val settlements: Map[Vertex, Int] = dep.get[VertexBuildingState[VB]](state).map { case (v, PlayerBuilding(vb, p)) =>
      settleEmbedder.select[Settlement.type](vb).map(_ => (v, p))
    }.flatten.toMap

    def addSettlement(vertex: Vertex, player: Int): STATE = {
      val map = dep.get[VertexBuildingState[VB]](state)
      dep.update(map + (vertex -> PlayerBuilding(settleEmbedder.inject(Settlement), player)), state)
    }

    def subtractSettlement(vertex: Vertex): STATE = {
      val map = dep.get[VertexBuildingState[VB]](state)
      dep.update(map - vertex, state)
    }

    def settlementVerticesForPlayer(player: Int): List[Vertex] = settlements.filter(_._2 == player).keys.toList

    def numSettlementsForPlayer(player: Int): Int = settlementVerticesForPlayer(player).size

    def placeSettlement(vertex: Vertex, player: Int): STATE = {
      addSettlement(vertex, player).incrementPointForPlayer(player)
    }

    def removeSettlement(vertex: Vertex): STATE = {
      settlements.get(vertex).fold(state)(subtractSettlement(vertex).decrementPointForPlayer)
    }
  }

  implicit class BuildCityStateOps[VB <: Coproduct, STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, VertexBuildingState[VB] :: PlayerPoints :: HNil],
   cityEmbedder: Embedder[VB, City.type :+: Settlement.type :+: CNil]) {

    implicit val pointsDep = dep.innerDependency[PlayerPoints :: HNil]
    implicit val innerEmbed = cityEmbedder.innerEmbed[Settlement.type :+: CNil]

    val cities: Map[Vertex, Int] = dep.get[VertexBuildingState[VB]](state).map { case (v, PlayerBuilding(vb, p)) =>
      cityEmbedder.select[City.type](vb).map(_ => (v, p))
    }.flatten.toMap

    def addCity(vertex: Vertex, player: Int): STATE = {
      val map = dep.get[VertexBuildingState[VB]](state)
      dep.update(map + (vertex -> PlayerBuilding(cityEmbedder.inject(City), player)), state)
    }

    def cityVerticesForPlayer(player: Int): List[Vertex] = cities.filter(_._2 == player).keys.toList

    def numCitiesForPlayer(player: Int): Int = cityVerticesForPlayer(player).size

    def buildCity(vertex: Vertex, player: Int): STATE = {
      state.removeSettlement(vertex)
        .addCity(vertex, player)
        .incrementPointForPlayer(player)
        .incrementPointForPlayer(player)
    }
  }

  implicit class BuildRoadStateOps[EB <: Coproduct, STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, EdgeBuildingState[EB] :: HNil],
   roadEmbedder: Embedder[EB, Road.type :+: CNil]) {

    val roads = dep.get[EdgeBuildingState[EB]](state).map { case (v, PlayerBuilding(eb, p)) =>
      roadEmbedder.select[Road.type](eb).map(_ => (v, p))
    }.flatten.toMap

    def addRoad(edge: Edge, player: Int): STATE = {
      val map = dep.get[EdgeBuildingState[EB]](state)
      dep.update(map + (edge -> PlayerBuilding(roadEmbedder.inject(Road), player)), state)
    }

    def roadEdgesForPlayer(player: Int): List[Edge] = roads.filter(_._2 == player).keys.toList

    def numRoadsForPlayer(player: Int): Int = roadEdgesForPlayer(player).size
  }

  implicit class RobberStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, RobberLocation :: HNil]) {
    val robberHexId: Int = dep.get[RobberLocation](state).robberHexId

    def updateRobberHexId(location: Int): STATE = dep.update(RobberLocation(location), state)
  }

  object ResourcesForBuildingPoly extends Poly1 {
    implicit def vertexBuilding[A](implicit vbValue: VertexBuildingValue[A]): Case.Aux[A, Int] = at(_ => vbValue.apply)
  }

  implicit class RollDiceOps[II, Transaction <: Coproduct, VB <: Coproduct, BOARD, INV[_], STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, RobberLocation :: VertexBuildingState[VB] :: BOARD :: Bank[II] :: INV[II] :: HNil],
   inv: ResourceInventories[II, Transaction, INV],
   socBoard: SOCBoard[II, BOARD],
   transaction: Embedder[Transaction, PerfectInfo[II]],
   vertexFolder: coproduct.Folder.Aux[ResourcesForBuildingPoly.type, VB, Int]) {

    implicit val robberLocationDep = dep.innerDependency[RobberLocation :: HNil]
    implicit val bankDep = dep.innerDependency[Bank[II] :: HNil]

    private val board: BOARD = dep.get[BOARD](state)
    private val vertexBuildingMap = dep.get[VertexBuildingState[VB]](state)

    def getResourcesGainedOnRoll(roll: Int): Map[Int, InventorySet[II, Int]] =
      board.numberHexes
        .get(roll)
        .fold[Seq[BoardHex[II]]](Nil)(_.filterNot(_.node == state.robberHexId))
        .flatMap { node =>
          node.hex.getResource.fold[Map[Int, InventorySet[II, Int]]](Map.empty) { res =>
            node.vertices.flatMap { vertex =>
              vertexBuildingMap.get(vertex).fold(Seq.empty[(Int, II)]) { vb =>
                (0 until vb.building.fold(ResourcesForBuildingPoly)).map(_ => vb.player -> res)
              }
            }.groupBy(_._1).view.mapValues(_.map(_._2).foldLeft(InventorySet.empty[II, Int]) { case (set, r) => set.add(1, r) }).toMap
          }
        }.toMap

    def distributeResources(resForPlayers: Map[Int, InventorySet[II, Int]]): STATE = {
      val totalResourcesCollected: InventorySet[II, Int] = resForPlayers.values.foldLeft(InventorySet.empty[II, Int])(_.add(_))
      val actualResForPlayers = {
        val resTypes: Seq[II] = totalResourcesCollected.getTypes
        val overflowTypes = resTypes.filter(item => !state.bank.contains(totalResourcesCollected.getAmount(item), item))
        resForPlayers.map[Int, InventorySet[II, Int]] { case (player, resourceSet) =>
          player -> overflowTypes.foldLeft(resourceSet) { case (set, res) => set.subtract(set.getAmount(res), res) }
        }
      }
      val trueTotalCollected = actualResForPlayers.values.foldLeft(InventorySet.empty[II, Int])(_.add(_))
      dep.updateWith[INV[II]](state)(_.update(actualResForPlayers.map { case (player, res) =>
        transaction.inject(Gain(player, res))
      }.toList))
        .subtractFromBank(trueTotalCollected)
    }
  }

  implicit class RoadLengthOps[STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, SOCLongestRoadPlayer :: SOCRoadLengths :: HNil]) {
    val longestRoadPlayer: Option[Int] = dep.get[SOCLongestRoadPlayer](state).player
    val roadLengths: Map[Int, Int] = dep.get[SOCRoadLengths](state).m

    def updateLongestRoadPlayer(player: Option[Int]) = dep.update[SOCLongestRoadPlayer](SOCLongestRoadPlayer(player), state)

    def updateRoadLengths(lengths: Map[Int, Int]) = dep.update[SOCRoadLengths](SOCRoadLengths(lengths), state)
  }

  implicit class ArmySizeOps[STATE <: HList]
  (state: STATE)
  (implicit dep: DependsOn[STATE, LargestArmyPlayer :: PlayerArmyCount :: HNil]) {
    val largestArmyPlayer: Option[Int] = dep.get[LargestArmyPlayer](state).player
    val armyCount: Map[Int, Int] = dep.get[PlayerArmyCount](state).m

    def updateLargestArmyPlayer(player: Option[Int]) = dep.update[LargestArmyPlayer](LargestArmyPlayer(player), state)

    def updateArmyCount(count: Map[Int, Int]) = dep.update[PlayerArmyCount](PlayerArmyCount(count), state)
  }
}
