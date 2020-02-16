package soc.board

import soc.inventory.Port

import scala.annotation.tailrec
import scala.util.Random

trait SimpleCatanBoardConfiguration extends BoardConfiguration {
  val hexes: List[Hex]
  val ports: List[Port]
}

trait SimpleBoardLayout[B <: SimpleCatanBoardConfiguration] extends BoardGenerator[B] with BoardRules[B] {

  implicit val boardRules: BoardRules[B] = this
  implicit val generator: BoardGenerator[B] = this

  def apply(hexes: List[Hex], ports: List[Port]): B
  val vertexMap: Map[Int, List[Int]]
  val portEdges: Seq[(Int, Int)]
  val numDesert: Int

  lazy val numHex: Int = vertexMap.size
  lazy val numPorts: Int = portEdges.size

  def apply(config: B): CatanBoard[B] = createBoard(config.hexes, config.ports)
  protected def createBoard(hexes: List[Hex], ports: List[Port]): CatanBoard[B] = {

    require(hexes.length == this.numHex, s"There should be $numHex hexes")
    require(ports.length == this.numPorts, s"There should be $numPorts ports")

    require(hexes.filter {
      case Desert => false
      case _ => true
    }.groupBy(_.getResource.get).forall { case (resource, list) =>
      list.length == resourceCounts(resource)
    }, "There are not enough hexes of each resource type")

    val transformedVertexMap: Map[Int, List[Vertex]] = vertexMap.view.mapValues(_.map(Vertex)).toMap
    val transformedPortMap: Map[Edge, Port] = portEdges.map { case (v1, v2) => Edge(Vertex(v1), Vertex(v2)) }.zip(ports).toMap
    CatanBoard(transformedVertexMap, transformedPortMap, hexes)
  }

  def unapply(board: CatanBoard[B]): B = {
    val hexes = board.hexesWithNodes.map(_.hex).toList
    val ports = board.portMap.values.toList
    apply(hexes, ports)
  }

  @tailrec
  override final def randomBoard(implicit rand: Random): B = {
    val resources = resourceCounts.toSeq.flatMap {
      case (resource, amt) => (1 to amt).map(_ => resource)
    }

    val hexes = rand.shuffle {
      (0 to numDesert).map(_ => Desert).toList :::
        (rand.shuffle(resources) zip rand.shuffle(validRolls)).map {
          case (resource, roll) => ResourceHex(resource, roll)
        }.toList
    }
    val ports = rand.shuffle {
      portCounts.toSeq.flatMap {
        case (port: Port, amt) => (1 to amt).map(_ => port)
      }.toList
    }

    val config = apply(hexes, ports)
    if (CatanBoard.checkValid(apply(config))) config
    else randomBoard
  }
}
