package soc.moves2.build

import soc.board.{BoardConfiguration, CatanBoard, Edge, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource, Road, VertexBuilding}
import soc.moves2.{PerfectInformationMoveGameAction, PerfectInformationSOCMove, SOCPlayerPointsMap, SOCState}
import util.MapWrapper


case class BuildRoadMove(player: Int, edge: Edge) extends PerfectInformationSOCMove[BuildRoadMove]
case class BuildRoadAction[BOARD <: BoardConfiguration, II <: Resource, STATE[P] <: RoadSOCState[BOARD, P, II, STATE[P]]](limit: Int, cost: CatanSet[II, Int]) extends PerfectInformationMoveGameAction[BOARD, II, STATE, BuildRoadMove] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    state.roadsForPlayer(position) < limit && inv.itemSet.contains(cost)
  }
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[BuildRoadMove] = {
    state.board.edges.map(BuildRoadMove(position, _)).filter(state.canBuildRoad).toSeq
  }
}

case class SOCRoadMap(m: Map[Edge, Road]) extends MapWrapper[Edge, Road]
trait RoadSOCState[BOARD <: BoardConfiguration, I <: InventoryItem, PERSPECTIVE <: InventoryHelper[I, PERSPECTIVE], STATE <: RoadSOCState[BOARD, I, PERSPECTIVE, STATE]] extends SOCState[BOARD, I, PERSPECTIVE, STATE]{
  this: CitySOCState[BOARD, I, PERSPECTIVE, STATE] with SettlementSOCState[BOARD, I, PERSPECTIVE, STATE] with LongestRoadSOCState[BOARD, I, PERSPECTIVE, STATE] =>
  def self = this

  def roads: SOCRoadMap

  def roadsForPlayer(player: Int): Int = roads.values.count(_.playerId == player)

  def updateRoads(roads: SOCRoadMap): STATE

  def canBuildRoad(buildRoadMove: BuildRoadMove): Boolean = {
    val loc = buildRoadMove.edge
    val playerId = buildRoadMove.player
    def canBuildRoadOffVertex(v: Vertex): Boolean = {
      if (!(v == loc.v1 || v == loc.v2)) false
      else if (settlements.get(v).fold(false)(_.playerId == playerId) || cities.get(v).fold(false)(_.playerId == playerId)) true
      else if (settlements.get(v).fold(false)(_.playerId != playerId) || cities.get(v).fold(false)(_.playerId != playerId)) false
      else board.edgesFromVertex(v).filterNot(_ == loc).exists { e => roads.get(e).fold(false)(_.playerId == playerId) }
    }
    board.edges.contains(loc) && !roads.contains(loc) && (canBuildRoadOffVertex(loc.v1) || canBuildRoadOffVertex(loc.v2))
  }

  def buildRoad(buildRoadMove: BuildRoadMove, buy: Option[CatanSet[I, Int]]): STATE = {
    onRoadBuild(buildRoadMove.player) {
      val ur = updateRoads(SOCRoadMap(roads + (buildRoadMove.edge -> Road(buildRoadMove.player)))).self.updateRoadLengths(buildRoadMove.player)
      buy.fold(ur)(cost => ur.updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildRoadMove.player, cost))))
    }
  }
}

case class SOCLongestRoadPlayer(player: Option[Int])
case class SOCRoadLengths(m: Map[Int, Int]) extends MapWrapper[Int, Int]

trait LongestRoadSOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: LongestRoadSOCState[BOARD, II, PERSPECTIVE, STATE]] extends SOCState[BOARD, II, PERSPECTIVE, STATE] {
  this: CitySOCState[BOARD, II, PERSPECTIVE, STATE] with SettlementSOCState[BOARD, II, PERSPECTIVE, STATE] with RoadSOCState[BOARD, II, PERSPECTIVE, STATE] =>
  def self = this

  def longestRoadPlayer: SOCLongestRoadPlayer
  def roadLengths: SOCRoadLengths

  def updateLongestRoadPlayer(player: SOCLongestRoadPlayer): STATE
  def updateRoadLengths(roadLengths: SOCRoadLengths): STATE

  def onRoadBuild(player: Int)(onRoad: => STATE): STATE = {
    val originalLongestRoadPlayer = self.longestRoadPlayer.player.flatMap(p => roadLengths.get(p).map(p -> _))
    val ur = onRoad.updateRoadLengths(player)
    (ur.roadLengths(player), originalLongestRoadPlayer) match {
      case (currPlayerRoads, Some((p, longestRoadLength))) if currPlayerRoads > longestRoadLength =>
        ur.updateLongestRoadPlayer(SOCLongestRoadPlayer(Some(player))).updatePoints(SOCPlayerPointsMap(((playerPoints - player) - p) + (player -> (playerPoints(player) + 2)) + (p -> (playerPoints(p) - 2))))
      case (currPlayerRoads, None) if currPlayerRoads >= 3 =>
        ur.updateLongestRoadPlayer(SOCLongestRoadPlayer(Some(player))).updatePoints(SOCPlayerPointsMap((playerPoints - player) + (player -> (playerPoints(player) + 2))))
      case _ => ur
    }
  }

  def updateRoadLengths(playerId: Int): STATE = {
    updateRoadLengths(roadLengths = SOCRoadLengths((roadLengths - playerId) + (playerId -> calcLongestRoadLength(playerId))))
  }
  def updateRoadLengths: STATE = {
    val players = (settlements ++ cities).map(_._2.playerId).toSeq.distinct
    updateRoadLengths(roadLengths = SOCRoadLengths(players.map(p => p -> calcLongestRoadLength(p)).toMap))
  }

  def calcLongestRoadLength(playerId: Int): Int = {
    val edges = roads.m.toSeq.flatMap {
      case (edge, building) if building.playerId == playerId => Seq(edge)
      case _ => Nil
    }
    calcLongestRoadLength(playerId, edges: _*)
  }

  def calcLongestRoadLength(playerId: Int, roads: Edge*): Int = {
    roads.map(r => calcLongestRoadLengthRecur(playerId, List((r.v1, r.v2)), List(r))).max
  }

  private def calcLongestRoadLengthRecur(playerId: Int, stack: List[(Vertex, Vertex)], visited: List[Edge] = Nil): Int = {
    if (stack.isEmpty) visited.length
    else {
      val (v1, v2): (Vertex, Vertex) = stack.head
      if (v1 == v2) {
        return Math.max(visited.length, calcLongestRoadLengthRecur(playerId, stack.tail, visited))
      }

      def fromVertex(vertex: Vertex): Seq[Vertex] = if ((settlements ++ cities).get(vertex).fold(true)(_.playerId == playerId)) {
        board.neighboringVertices(vertex).filterNot(v => visited.contains(Edge(vertex, v))).filter(v => roads.get(Edge(vertex, v)).fold(false)(_.playerId == playerId)).toList
      } else Nil

      (fromVertex(v1), fromVertex(v2)) match {
        // see road scenario1
        case (Nil, Nil) =>
          Math.max(visited.length, calcLongestRoadLengthRecur(playerId, stack.tail, visited))

        // see road scenario2
        case (Nil, r :: Nil) =>
          calcLongestRoadLengthRecur(playerId, (v1, r) :: stack.tail, Edge(v2, r) :: visited)

        case (Nil, r1 :: r2 :: Nil) =>
          Seq(
            calcLongestRoadLengthRecur(playerId, (v1, r1) :: stack.tail, Edge(v2, r1) :: visited),
            calcLongestRoadLengthRecur(playerId, (v1, r2) :: stack.tail, Edge(v2, r2) :: visited)
          ).max

        case (l :: Nil, Nil) =>
          calcLongestRoadLengthRecur(playerId, (l, v2) :: stack.tail, Edge(l, v1) :: visited)

        case (l :: Nil, r :: Nil) =>
          calcLongestRoadLengthRecur(playerId, (l, r) :: stack.tail, Edge(r, v2) :: Edge(l, v1) :: visited)

        case (l :: Nil, r1 :: r2 :: Nil) =>
          Seq(
            calcLongestRoadLengthRecur(playerId, (l, r1) :: stack.tail, Edge(r1, v2) :: Edge(l, v1) :: visited),
            calcLongestRoadLengthRecur(playerId, (l, r2) :: stack.tail, Edge(r2, v2) :: Edge(l, v1) :: visited)
          ).max
        // see road scenario4

        case (l1 :: l2 :: Nil, Nil) =>
          Seq(
            calcLongestRoadLengthRecur(playerId, (l1, v2) :: stack.tail, Edge(l1, v1) :: visited),
            calcLongestRoadLengthRecur(playerId, (l2, v2) :: stack.tail, Edge(l2, v1) :: visited)
          ).max

        case (l1 :: l2 :: Nil, r :: Nil) =>
          Seq(
            calcLongestRoadLengthRecur(playerId, (l1, r) :: stack.tail, Edge(l1, v1) :: Edge(r, v2) :: visited),
            calcLongestRoadLengthRecur(playerId, (l2, r) :: stack.tail, Edge(l2, v1) :: Edge(r, v2) :: visited)
          ).max

        case (l1 :: l2 :: Nil, r1 :: r2 :: Nil) =>
          Seq(
            calcLongestRoadLengthRecur(playerId, (l1, r1) :: stack.tail, Edge(l1, v1) :: Edge(r1, v2) :: visited),
            calcLongestRoadLengthRecur(playerId, (l2, r2) :: stack.tail, Edge(l2, v1) :: Edge(r2, v2) :: visited),
            calcLongestRoadLengthRecur(playerId, (l1, r2) :: stack.tail, Edge(l1, v1) :: Edge(r2, v2) :: visited),
            calcLongestRoadLengthRecur(playerId, (l2, r1) :: stack.tail, Edge(l2, v1) :: Edge(r1, v2) :: visited)
          ).max
        case _ => throw new Exception("")
      }
    }
  }
}
