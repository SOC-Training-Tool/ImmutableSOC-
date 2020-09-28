package soc.board

import soc.core.Roll
import soc.inventory._
import soc.inventory.resources.ResourceSet
import soc.inventory.resources.ResourceSet.Resources
import util.MapReverse

import scala.util.Random

sealed trait Hex {
  val getResource: Option[Resource]
  val getNumber: Option[Roll]
  lazy val getResourceAndNumber: Option[(Resource, Roll)] = getResource zip getNumber
}
case class ResourceHex(resource: Resource, number: Roll) extends Hex {
  override val getResource: Option[Resource] = Some(resource)
  override val getNumber: Option[Roll] = Some(number)
}
case object Desert extends Hex {
  override val getResource: Option[Resource] = None
  override val getNumber: Option[Roll] = None
}


case class BoardHex(
  node: Int,
  hex: Hex,
  vertices: List[Vertex]) {
}

sealed trait BuildingLocation
case class Vertex(node: Int) extends BuildingLocation
case class Edge(v1: Vertex, v2: Vertex) extends BuildingLocation {

  def contains(v: Vertex): Boolean = v == v1 || v == v2

  override def canEqual(a: Any): Boolean = a.isInstanceOf[Edge]

  override def equals(that: Any): Boolean = that match {
    case e@Edge(ev1, ev2) =>
      e.canEqual(this) &&
        (ev1 == v1 && ev2 == v2) || (ev2 == v1 && ev1 == v2)
    case _ => false
  }

  override def hashCode: Int = (v1.node * v1.node) + (v2.node * v2.node)
}

case class CatanBoard[T <: BoardConfiguration] private(
  val hexesWithNodes: Seq[BoardHex],
  val vertices: Seq[Vertex],
  val edges: Seq[Edge],
  val edgesFromVertex: Map[Vertex, Seq[Edge]],
  val neighboringVertices: Map[Vertex, Seq[Vertex]],
  val adjacentHexes: Map[Vertex, Seq[BoardHex]],
  val portMap: Map[Edge, Port],
  robberHex: Int,
  buildingMap: Map[BuildingLocation, CatanBuilding] = Map.empty,
  roadLengths: Map[Int, Int] = Map.empty
) {

  val verticesBuildingMap: Map[Vertex, VertexBuilding] = buildingMap.collect {
    case(v: Vertex, b: VertexBuilding) => v -> b
  }
  val edgesBuildingMap: Map[Edge, EdgeBuilding] = buildingMap.collect {
    case (e: Edge, b: EdgeBuilding) => e -> b
  }

  def canBuildSettlement(vertex: Vertex, playerId: Int, initialPlacement: Boolean = false): Boolean = {
    vertices.contains(vertex) &&
      !verticesBuildingMap.contains(vertex) &&
      neighboringVertices(vertex).forall { v => !verticesBuildingMap.contains(v) } &&
      (if (!initialPlacement) {
        edgesFromVertex(vertex).exists { edge =>
          edgesBuildingMap.get(edge).fold(false)(_.playerId == playerId)
        }
      } else true)
  }

  def buildSettlement(vertex: Vertex, playerId: Int): CatanBoard[T] = {
    val settlement = Settlement(playerId)
    val vertexMap = buildingMap + (vertex -> settlement)
    copy(buildingMap = vertexMap)
  }

  def canBuildCity(vertex: Vertex, playerId: Int): Boolean = {
    verticesBuildingMap.contains(vertex) &&
      verticesBuildingMap(vertex).isInstanceOf[Settlement] && {
      val settlement = verticesBuildingMap(vertex)
      settlement.playerId == playerId
    }
  }

  def buildCity(vertex: Vertex, playerId: Int): CatanBoard[T] = {
    val city = City(playerId)
    val vertexMap = (buildingMap - vertex) + (vertex -> city)
    copy(buildingMap = vertexMap)
  }

  def canBuildRoad(edge: Edge, playerId: Int): Boolean = {
    def canBuildRoadOffVertex(v: Vertex): Boolean = {
      if (verticesBuildingMap.get(v).fold(false)(_.playerId == playerId)) true
      else if (verticesBuildingMap.get(v).fold(false)(_.playerId != playerId)) false
      else {
        edgesFromVertex(v).filterNot(_ == edge).exists { e =>
          edgesBuildingMap.contains(e) && edgesBuildingMap(e).playerId == playerId
        }
      }
    }

    edges.contains(edge) && !edgesBuildingMap.contains(edge) && (canBuildRoadOffVertex(edge.v1) || canBuildRoadOffVertex(edge.v2))
  }

  def getSettlementVerticesForPlayer(id: Int): Seq[Vertex] = verticesBuildingMap.toSeq.filter {
    case (_, Settlement(`id`)) => true
    case _ => false
  }.map(_._1)
  def getNumSettlementsForPlayer(id: Int): Int = getSettlementVerticesForPlayer(id).length

  def getNumCityVerticesForPlayer(id: Int): Seq[Vertex] = verticesBuildingMap.toSeq.filter {
    case (_, City(`id`)) => true
    case _ => false
  }.map(_._1)
  def getNumCitiesForPlayer(id: Int): Int = getNumCityVerticesForPlayer(id).length

  def getRoadEdgesForPlayer(id: Int): Seq[Edge] = edgesBuildingMap.toSeq.filter {
    case (_, Road(`id`)) => true
    case _ => false
  }.map(_._1)
  def getNumRoadsForPlayer(id: Int): Int = getRoadEdgesForPlayer(id).length

  def getPortsForPlayer(id: Int): Set[Port] = getSettlementVerticesForPlayer(id).flatMap(getPort).toSet

  def getPort(vertex: Vertex): Option[Port] = {
    portMap.find { case (edge, _) => edge.contains(vertex) }.map(_._2)
  }

  def getPossibleSettlements(playerId: Int, initial: Boolean): Seq[Vertex] = {
    vertices.filter(canBuildSettlement(_, playerId, initial))
  }

  def getPossibleCities(playerId: Int): Seq[Vertex] = {
    vertices.filter(canBuildCity(_, playerId))
  }

  def getPossibleRoads(playerId: Int): Seq[Edge] = {
    edges.filter(canBuildRoad(_, playerId))
  }

  def playersOnHex(node: Int): Seq[Int] = {
    hexesWithNodes.find(_.node == node).fold(List.empty[Int])(_.vertices.flatMap { v =>
      verticesBuildingMap.get(v).map(_.playerId)
    }.distinct)
  }

  def longestRoadLength(playerId: Int): Int = roadLengths.getOrElse(playerId, 0)

  def getResourcesGainedOnRoll(roll: Int): Map[Int, Resources] = {
    hexesWithNodes.filter { boardHex =>
      boardHex.hex.getNumber.fold(false)(_.number == roll) && boardHex.node != robberHex
    }.flatMap { node =>
      node.vertices.flatMap { vertex =>
        verticesBuildingMap.get(vertex) match {
          case Some(Settlement(playerId)) => Seq(playerId -> node.hex.getResource.get)
          case Some(City(playerId)) => Seq(playerId -> node.hex.getResource.get, playerId -> node.hex.getResource.get)
          case _ => Nil
        }
      }
    }.groupBy(_._1).view.mapValues(_.map(_._2).foldLeft(ResourceSet.empty[Int])(_.add(1, _)))
  }.toMap


}

object CatanBoard {

  def apply[T <: BoardConfiguration](
    vertexMap: Map[Int, List[Vertex]],
    portMap: Map[Edge, Port],
    hexes: List[Hex]
  ): CatanBoard[T] = {
    val hexesWithNodes: Seq[BoardHex] = hexes.zipWithIndex.map { case (hex: Hex, node: Int) =>
      BoardHex(node, hex, vertexMap(node))
    }
    CatanBoard(hexesWithNodes, portMap)
  }

  def apply[T <: BoardConfiguration](
    hexes: Seq[BoardHex],
    portMap: Map[Edge, Port],
    robber: Int,
    buildingMap: Map[BuildingLocation, CatanBuilding]
  ): CatanBoard[T] = {
    CatanBoard(hexes, portMap).copy(
      buildingMap = buildingMap,
      robberHex = robber
    )
  }

  def apply[T <: BoardConfiguration](
    hexesWithNodes: Seq[BoardHex],
    portMap: Map[Edge, Port]
  ): CatanBoard[T] = {

    val vertices: Seq[Vertex] = hexesWithNodes.flatMap(_.vertices).distinct
    val edges: Seq[Edge] = hexesWithNodes.flatMap { hex =>
      val vertices = hex.vertices
      vertices.zip(vertices.tail ::: List(vertices.head)).map { case (v1, v2) => Edge(v1, v2) }
    }.distinct

    val edgesFromVertex: Map[Vertex, Seq[Edge]] = vertices.map { vertex =>
      vertex -> edges.flatMap {
        case Edge(`vertex`, v) => Seq(Edge(`vertex`, v))
        case Edge(v, `vertex`) => Seq(Edge(v, `vertex`))
        case _ => Nil
      }
    }.toMap

    val neighboringVertices: Map[Vertex, Seq[Vertex]] = vertices.map { vertex =>
      vertex -> edgesFromVertex(vertex).flatMap {
        case Edge(`vertex`, v) => Seq(v)
        case Edge(v, `vertex`) => Seq(v)
        case _ => Nil
      }
    }.toMap.view.mapValues(_.distinct).toMap

    val adjacentHexes: Map[Vertex, Seq[BoardHex]] = vertices.map { vertex =>
      vertex -> hexesWithNodes.filter(_.vertices.contains(vertex))
    }.toMap

    val robber = hexesWithNodes.find(_.hex.getNumber.isEmpty).fold(0)(_.node)
    CatanBoard(hexesWithNodes, vertices, edges, edgesFromVertex, neighboringVertices, adjacentHexes, portMap, robber)
  }

  def checkValid[T <: BoardConfiguration](board: CatanBoard[T])(implicit boardRules: BoardRules[T]): Boolean = {
    board.adjacentHexes.forall { case (_, hexes) =>
      val rolls = hexes.map(_.hex.getNumber).flatten
      rolls.filterNot(boardRules.cannotNeighbor.contains).length < 2
    }
  }
}

trait BoardConfiguration

trait BoardGenerator[T <: BoardConfiguration] {
  def apply(config: T): CatanBoard[T]
  def unapply(board: CatanBoard[T]): T
  def randomBoard(implicit rand: Random): T
}

trait BoardMapping[T <: BoardConfiguration] {
  val hexMapping: Map[Int, String]
  val vertexMapping: Map[Int, String]
  lazy val reverseHexMapping = MapReverse.reverseMap(hexMapping)
  lazy val reverseVertexMapping = MapReverse.reverseMap(vertexMapping)
}

trait BoardRules[T <: BoardConfiguration] {
  val numHex: Int
  val numPorts: Int
  val resourceCounts: Map[Resource, Int]
  val portCounts: Map[Port, Int]
  val validRolls: Seq[Roll]
  val cannotNeighbor: Seq[Roll]

  val robberRoll: Int
  val diceProbability: Int

}