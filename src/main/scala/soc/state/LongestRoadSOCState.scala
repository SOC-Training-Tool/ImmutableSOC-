package soc.state

import shapeless.{HList, :: => :::}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.{InventoryHelper, InventoryItem}
import soc.state.SOCState.{SOCState, SOCStateOps}
import soc.state.build.BoardOps
import util.{DependsOn, MapWrapper}

case class SOCLongestRoadPlayer(player: Option[Int])
case class SOCRoadLengths(m: Map[Int, Int]) extends MapWrapper[Int, Int]

object LongestRoadSOCState {

  implicit class LongestRoadSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCLongestRoadPlayer ::: SOCRoadLengths ::: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val longestRoadPlayer: SOCLongestRoadPlayer = dep.get(state)
    val roadLengths: SOCRoadLengths = dep.get(state)
    def updateLongestRoadPlayer(player: SOCLongestRoadPlayer): STATE = {
      val playerPoints = state.playerPoints
      val s = (longestRoadPlayer.player, player.player) match {
        case (None, None) => state
        case (None, Some(p)) => state.updatePoints(SOCPlayerPointsMap(playerPoints + (p -> (playerPoints(p) + 2))))
        case (Some(p), None) => state.updatePoints(SOCPlayerPointsMap(playerPoints + (p -> (playerPoints(p) - 2))))
        case (Some(o), Some(n)) => state.updatePoints(SOCPlayerPointsMap(playerPoints + (n -> (playerPoints(n) + 2)) + (o -> (playerPoints(o) - 2))))
      }
      dep.update(player, s)
    }
    def updateRoadLengths: STATE = updateRoadLengths(SOCRoadLengths(state.playerIds.foldLeft(Map.empty[Int, Int]) { case (m, p) => m + (p -> calcLongestRoadLength(p))}))
    def updateRoadLengths(player: Int): STATE = updateRoadLengths(SOCRoadLengths((roadLengths - player) + (player -> calcLongestRoadLength(player))))
    def updateRoadLengths(roadLengths: SOCRoadLengths): STATE = dep.update(roadLengths, state)

    def redistributeLongestRoad(player: Int): STATE = {
      val ur = state.updateRoadLengths(player)
      val (longestRoadLength, longRoadPlayers) = ur.playerIds
        .map(p => (p, ur.roadLengths(p)))
        .groupBy(_._2)
        .maxBy(_._1)
      val longestRoadPlayer = ur.longestRoadPlayer.player.flatMap(p => ur.roadLengths.get(p).map(p -> _))
      (longestRoadPlayer.map(_._2), longRoadPlayers.map(_._1)) match {
        case (_, _) if longestRoadLength < 5 => ur.updateLongestRoadPlayer(SOCLongestRoadPlayer(None))
        case (None, p :: Nil) => ur.updateLongestRoadPlayer(SOCLongestRoadPlayer(Some(p)))
        case (Some(lr), _) if lr == longestRoadLength => ur
        case (Some(_), p :: Nil) => ur.updateLongestRoadPlayer(SOCLongestRoadPlayer(Some(p)))
        case (Some(_), _ :: _ :: _) => ur.updateLongestRoadPlayer(SOCLongestRoadPlayer(None))
      }
    }

    def calcLongestRoadLength(playerId: Int): Int = {
      val edges = boardOps.edgeBuildingMap(state).toSeq.flatMap {
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

        def fromVertex(vertex: Vertex): Seq[Vertex] = if (boardOps.vertexBuildingMap(state).get(vertex).fold(true)(_.playerId == playerId)) {
          state.board.neighboringVertices(vertex).filterNot(v => visited.contains(Edge(vertex, v))).filter(v => boardOps.edgeBuildingMap(state).get(Edge(vertex, v)).fold(false)(_.playerId == playerId)).toList
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
}
