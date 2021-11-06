package soc.moves2.build

import shapeless.ops.hlist.{SelectAll, Selector}
import shapeless.{::, HList, HNil, :: => :::}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory._
import soc.moves2.SOCState.{SOCState, SOCStateOps}
import soc.moves2._
import util.MapWrapper

case class BuildRoadMove(player: Int, edge: Edge) extends PerfectInformationSOCMove[BuildRoadMove]

case class SOCRoadMap(m: Map[Edge, Road]) extends MapWrapper[Edge, Road]

trait RoadBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
  def onBuildRoad(buildRoadMove: BuildRoadMove, s: S)(f: S => S): S
}

object RoadSOCState {

  implicit class RoadSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val roads: SOCRoadMap = dep.get(state)
    def updateRoads(roads: SOCRoadMap): STATE = dep.update(roads, state)

    def buildRoad(buildRoadMove: BuildRoadMove, buy: Option[CatanSet[II, Int]]): STATE = roadBoardOps.onBuildRoad(buildRoadMove, state) { state =>
      val ur = updateRoads(SOCRoadMap(roads + (buildRoadMove.edge -> Road(buildRoadMove.player))))
      buy.fold(ur)(cost => ur.updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildRoadMove.player, cost))))
    }

    def roadsForPlayer(player: Int): Int = roads.values.count(_.playerId == player)

    def canBuildRoad(buildRoadMove: BuildRoadMove): Boolean = {
      val loc = buildRoadMove.edge
      val playerId = buildRoadMove.player

      def canBuildRoadOffVertex(v: Vertex): Boolean = (v == loc.v1 || v == loc.v2) &&
        (roadBoardOps.vertexBuildingMap(state).get(v).fold(false)(_.playerId == playerId) ||
          (!roadBoardOps.vertexBuildingMap(state).get(v).fold(false)(_.playerId != playerId) &&
            state.board.edgesFromVertex(v).filterNot(_ == loc).exists { e => roads.get(e).fold(false)(_.playerId == playerId) }))

      state.board.edges.contains(loc) && !roadBoardOps.edgeBuildingMap(state).contains(loc) && (canBuildRoadOffVertex(loc.v1) || canBuildRoadOffVertex(loc.v2))
    }
  }

  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove] = {
    (state: STATE, _: PerfectInfo, pos: Int) =>
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      state.board.edges.map(BuildRoadMove(pos, _)).filter(state.canBuildRoad)
  }

  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCCanRollDice ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildRoadMove]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove] = {
    (state, inv, player) =>
      import soc.moves2.RollDiceSOCState.RollDiceSOCStateOps
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      implicit val cityDep = dep.innerDependency[SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]]
      implicit val rollDep = dep.innerDependency[SOCCanRollDice ::: SOCState[BOARD, II, PERSPECTIVE]]
      state.rolledDice && state.currentPlayer == player && state.roadsForPlayer(player) < 4 && inv.canSpend(cost.getCost)
  }

  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove] = {
    (state, inv, move) =>
      canDoAction(state, inv, move.player) && state.canBuildRoad(move)
  }

  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap :: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildRoadMove]) = new UpdateState[BOARD, II, PERSPECTIVE, BuildRoadMove, STATE] {
    override def apply(t: STATE, u: BuildRoadMove): STATE =  t.buildRoad(u, Some(cost.getCost))
  }


//  type BASE_NEXT_MOVES[W[_ <: SOCMoveResult]] = W[BuildSettlementMove] :: W[BuildRoadMove] :: W[BuildCityMove] :: W[EndTurnMove] :: HNil // TODO add full list
//  implicit def baseNextMoves[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], W[_ <: SOCMoveResult], A <: HList, STATE <: HList](implicit ws: Selector[A, W[BuildRoadMove]], sa: SelectAll[A, BASE_NEXT_MOVES[W]]): NextMove[BOARD, II, PERSPECTIVE, W, A, STATE, BuildRoadMove] = (a: A) => {
//    val func = (_: STATE, r: BuildRoadMove) => Map(r.player -> sa.apply(a).toList)
//    ws.apply(a) -> func
//  }

  //  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[B, I, P] <: HList](implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[STATE[BOARD, II, PERSPECTIVE]], cost: Cost[II, BuildRoadMove]): ApplyMoveResult[BuildRoadMove, STATE[BOARD, II, PERSPECTIVE]] = (s, m) =>
  //    s.buildRoad(m, Some(cost.getCost))
}

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

