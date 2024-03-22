package soc.base.actions

import shapeless.Coproduct
import soc.board.{Edge, Vertex}

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
