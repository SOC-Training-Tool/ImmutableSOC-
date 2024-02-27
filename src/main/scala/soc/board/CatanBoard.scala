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

case class CatanBoard[T <: BoardConfiguration] (
  hexesWithNodes: Seq[BoardHex],
  portMap: Map[Edge, Port]) {

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

  lazy val numberHexes: Map[Int, Seq[BoardHex]] = hexesWithNodes
    .flatMap(h => h.hex.getNumber.map(n => (n.number, h)))
    .groupBy(_._1)
    .view.mapValues(_.map(_._2))
    .toMap
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