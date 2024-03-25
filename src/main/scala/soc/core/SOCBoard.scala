package soc.core

import shapeless.Coproduct

case class Vertex(node: Int)

case class Edge(v1: Vertex, v2: Vertex) {

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

sealed trait Hex[+Res] {
  val getResourceAndNumber: Option[(Res, Int)]
  val getResource: Option[Res]
  val getNumber: Option[Int]
}
case class ResourceHex[Res <: Coproduct](resource: Res, number: Int) extends Hex[Res] {
  override val getResourceAndNumber: Option[(Res, Int)] = Some((resource, number))
  override val getResource: Option[Res] = Some(resource)
  override val getNumber: Option[Int] = Some(number)
}
case object Desert extends Hex[Nothing] {
  override val getResourceAndNumber: Option[(Nothing, Int)] = None
  override val getResource: Option[Nothing] = None
  override val getNumber: Option[Int] = None
}

case class BoardHex[Res](
                     node: Int,
                     hex: Hex[Res],
                     vertices: List[Vertex]) {
}

trait SOCBoard[Res, T] {
  def hexesWithNodes(t: T): Seq[BoardHex[Res]]
}

object SOCBoard {

  implicit class SOCBoardOps[Res, T](t: T)(implicit board: SOCBoard[Res, T]) {

    lazy val hexesWithNodes: Seq[BoardHex[Res]] = board.hexesWithNodes(t)

    lazy val vertices: Seq[Vertex] = hexesWithNodes.flatMap(_.vertices).distinct
    lazy val edges: Seq[Edge] = hexesWithNodes.flatMap { hex =>
      val vertices = hex.vertices
      vertices.zip(vertices.tail ::: List(vertices.head)).map { case (v1, v2) => Edge(v1, v2) }
    }.distinct

    lazy val edgesFromVertex: Map[Vertex, Seq[Edge]] = vertices.map { vertex =>
      vertex -> edges.flatMap {
        case e @ Edge(`vertex`, _) => Seq(e)
        case e @ Edge(_, `vertex`) => Seq(e)
        case _ => Nil
      }
    }.toMap

    lazy val neighboringVertices: Map[Vertex, Seq[Vertex]] = vertices.map { vertex =>
      vertex -> edgesFromVertex(vertex).flatMap {
        case Edge(`vertex`, v) => Seq(v)
        case Edge(v, `vertex`) => Seq(v)
        case _ => Nil
      }
    }.toMap.view.mapValues(_.distinct).toMap

    lazy val numberHexes: Map[Int, Seq[BoardHex[Res]]] = hexesWithNodes
      .flatMap(h => h.hex.getNumber.map(_ -> h))
      .groupBy(_._1)
      .view.mapValues(_.map(_._2))
      .toMap

    lazy val hexesForVertex: Map[Vertex, Seq[BoardHex[Res]]] = vertices.map { v =>
      v -> hexesWithNodes.filter(_.vertices.contains(v))
    }.toMap
  }


}
