//package soc.state.vectorize
//
//import soc.board.{BaseBoardConfiguration, BoardConfiguration, CatanBoard}
//import soc.inventory._
//import soc.inventory.developmentCard.DevelopmentCardSet._
//import soc.inventory.Inventory.{PerfectInfo, ProbableInfo}
//import soc.inventory.developmentCard.DevelopmentCardSet
//import soc.inventory.resources.ResourceSet
//import soc.state.GameState
//import soc.state.player.PlayerState
//
//import scala.annotation.tailrec
//
//
//trait Vectorize[T] {
//  def toVector(state: T, perspective: Option[Int] = None): List[SOCVectorize.VectorizeType]
//}
//
//object SOCVectorize {
//
//  type VectorizeType = Any
//
//  def apply[T](implicit vectorizer: Vectorize[T]): Vectorize[T] = vectorizer
//
//  object ops {
//    def toVector[T](v: T, perspective: Option[Int] = None)(implicit vectorizer: Vectorize[T]): List[VectorizeType] = SOCVectorize[T].toVector(v, perspective)
//
//    implicit class VectorizeOps[T](v: T)(implicit vectorizer: Vectorize[T]) {
//      def toVector(perspective: Option[Int] = None): List[VectorizeType] = SOCVectorize[T].toVector(v, perspective)
//    }
//  }
//
//
//  implicit def gameStateVectorize[T <: Inventory[T], B <: BoardConfiguration](implicit boardV: Vectorize[CatanBoard[B]], playerV: Vectorize[PlayerState[T]]): Vectorize[GameState[T, B]] = {
//    case (gs, Some(perspective)) =>
//
//    @tailrec
//    def arrangePlayers(pos: Int, collector: List[PlayerState[T]] = Nil): List[PlayerState[T]] = {
//      if (collector.contains(gs.players.getPlayer(pos))) collector
//      else {
//        arrangePlayers(gs.players.nextPlayer(pos), collector ::: List(gs.players.getPlayer(pos)))
//      }
//    }
//
//    gs.turn.toDouble ::
//    gs.currentPlayer.toDouble ::
//      gs.players.getPlayers.map(_.points).sum ::
//      boardV.toVector(gs.board, Some(perspective)) ::: {
//      val playedDCards = gs.players.getPlayers
//        .map(_.inventory.playedDevCards)
//        .foldLeft(DevelopmentCardSet.empty[Int]) { case (total, set) => total.add(set) }
//      (if (gs.canRollDice) 1.0 else 0.0) ::
//        (if (gs.canPlayCard) 1.0 else 0.0) ::
//        gs.resourceBank.getAmount(Brick).toDouble ::
//        gs.resourceBank.getAmount(Wood).toDouble ::
//        gs.resourceBank.getAmount(Sheep).toDouble ::
//        gs.resourceBank.getAmount(Wheat).toDouble ::
//        gs.resourceBank.getAmount(Ore).toDouble ::
//        gs.developmentCardsLeft.toDouble ::
//        playedDCards.getAmount(Knight).toDouble ::
//        playedDCards.getAmount(Monopoly).toDouble ::
//        playedDCards.getAmount(RoadBuilder).toDouble ::
//        playedDCards.getAmount(YearOfPlenty).toDouble ::
//        playedDCards.getAmount(CatanPoint).toDouble :: Nil
//    } :::
//      arrangePlayers(gs.currentPlayer).flatMap(playerV.toVector(_))
//  }
//
//  implicit def playerStateVectorize[T <: Inventory[T]](implicit inventoryVector: Vectorize[T]): Vectorize[PlayerState[T]] = { case (ps, _) =>
//      ps.position.toDouble ::
//        ps.numCards.toDouble ::
//        ps.dots.getAmount(Brick).toDouble ::
//        ps.dots.getAmount(Wood).toDouble ::
//        ps.dots.getAmount(Sheep).toDouble ::
//        ps.dots.getAmount(Wheat).toDouble ::
//        ps.dots.getAmount(Ore).toDouble ::
//        ps.points.toDouble ::
//        ps.boardPoints.toDouble ::
//        ps.armyPoints.toDouble ::
//        ps.roadPoints.toDouble ::
//        ps.dCardPoints.toDouble ::
//        inventoryVector.toVector(ps.inventory) ::: {
//        (if (ps.ports.contains(Brick)) 1.0 else 0.0) ::
//          (if (ps.ports.contains(Ore)) 1.0 else 0.0) ::
//          (if (ps.ports.contains(Sheep)) 1.0 else 0.0) ::
//          (if (ps.ports.contains(Wheat)) 1.0 else 0.0) ::
//          (if (ps.ports.contains(Wood)) 1.0 else 0.0) ::
//          (if (ps.ports.contains(Misc)) 1.0 else 0.0) ::
//          (ps.settlements.map(_.node.toDouble) ::: (1 to (5 - ps.settlements.size)).map(_ => 0.0).toList) :::
//          (ps.cities.map(_.node.toDouble) ::: (1 to (4 - ps.cities.size)).map(_ => 0.0).toList)
//      }
//       // (ps.roads.map(_.toDouble) ::: (1 to (15 - ps.roads.size)).map(_ => 0.0).toList)
//    }
//
//    implicit val baseBoardVectorize: Vectorize[CatanBoard[BaseBoardConfiguration]] = { case (board, Some(perspective)) => //53
//      val hexSetup = board.hexesWithNodes.sortBy(_.node).flatMap { node =>
//        node.hex.getResourceAndNumber.fold(List(0.0, 0.0)) {case (res, roll) =>
//          List(res.value.toDouble, roll.number.toDouble, if (node.node == board.robberHex) 1.0 else 0.0, roll.dots)
//        }
//      }.toList
//
//      val portSetup = board.portMap.values.map(_.value.toDouble).toList
//
//      val dots = {
//        val dotSet = board.hexesWithNodes
//          .filter(_.hex.getNumber.isDefined)
//          .groupBy(_.hex.getResource.get)
//          .foldLeft(ResourceSet.empty[Int]){
//            case (dots, (res, hexes)) => dots.add(hexes.map(_.hex.getNumber.get.dots).sum, res)
//          }
//        dotSet.getAmount(Brick).toDouble ::
//          dotSet.getAmount(Wood).toDouble ::
//          dotSet.getAmount(Sheep).toDouble ::
//          dotSet.getAmount(Wheat).toDouble ::
//          dotSet.getAmount(Ore).toDouble :: Nil
//      }
//
//      val nodes = board.vertices.map { vertex =>
//          board.verticesBuildingMap.get(vertex).fold(0.0) {
//            case Settlement(playerId) => 1 + (2 * playerId)
//            case City(playerId) => 2 * (1 + playerId)
//            case _ => 0
//          }.toDouble
//      }.toList
//      val edges = board.edges.map { edge =>
//        board.edgesBuildingMap.get(edge).fold(0.0) {
//          case Road(playerId) => 1 + playerId
//        }.toDouble
//      }.toList
//
//      hexSetup ::: portSetup ::: nodes ::: edges ::: dots
//    }
//
//  implicit val probableInventoryVector: Vectorize[ProbableInfo] = { inventory: ProbableInfo =>
//    inventory.probableResourceSet.getAmount(Brick) ::
//      inventory.probableResourceSet.getAmount(Wood) ::
//      inventory.probableResourceSet.getAmount(Sheep) ::
//      inventory.probableResourceSet.getAmount(Wheat) ::
//      inventory.probableResourceSet.getAmount(Ore) ::
//      inventory.playedDevCards.getTotal.toDouble ::
//      inventory.playedDevCards.getAmount(Knight).toDouble ::
//      inventory.playedDevCards.getAmount(Monopoly).toDouble ::
//      inventory.playedDevCards.getAmount(RoadBuilder).toDouble ::
//      inventory.playedDevCards.getAmount(YearOfPlenty).toDouble ::
//      inventory.playedDevCards.getAmount(CatanPoint).toDouble ::
//      inventory.knownDevCards.filterUnPlayed.getTotal.toDouble ::
//      inventory.knownDevCards.filterUnPlayed.getAmount(Knight).toDouble ::
//      inventory.knownDevCards.filterUnPlayed.getAmount(Monopoly).toDouble ::
//      inventory.knownDevCards.filterUnPlayed.getAmount(RoadBuilder).toDouble ::
//      inventory.knownDevCards.filterUnPlayed.getAmount(YearOfPlenty).toDouble ::
//      inventory.knownDevCards.filterUnPlayed.getAmount(CatanPoint).toDouble ::
//      inventory.probableDevCards.getTotal.toDouble ::
//      inventory.probableDevCards.getAmount(Knight).toDouble ::
//      inventory.probableDevCards.getAmount(Monopoly).toDouble ::
//      inventory.probableDevCards.getAmount(RoadBuilder).toDouble ::
//      inventory.probableDevCards.getAmount(YearOfPlenty).toDouble ::
//      inventory.probableDevCards.getAmount(CatanPoint).toDouble :: Nil
//  }
//
//  implicit val perfectInventoryVector: Vectorize[PerfectInfo] = { inventory =>
//    inventory.resourceSet.getAmount(Brick) ::
//      inventory.resourceSet.getAmount(Wood) ::
//      inventory.resourceSet.getAmount(Sheep) ::
//      inventory.resourceSet.getAmount(Wheat) ::
//      inventory.resourceSet.getAmount(Ore) ::
//      inventory.playedDevCards.getTotal.toDouble ::
//      inventory.playedDevCards.getAmount(Knight).toDouble ::
//      inventory.playedDevCards.getAmount(Monopoly).toDouble ::
//      inventory.playedDevCards.getAmount(RoadBuilder).toDouble ::
//      inventory.playedDevCards.getAmount(YearOfPlenty).toDouble ::
//      inventory.playedDevCards.getAmount(CatanPoint).toDouble ::
//      inventory.developmentCards.filterUnPlayed.getTotal.toDouble ::
//      inventory.developmentCards.filterUnPlayed.getAmount(Knight).toDouble ::
//      inventory.developmentCards.filterUnPlayed.getAmount(Monopoly).toDouble ::
//      inventory.developmentCards.filterUnPlayed.getAmount(RoadBuilder).toDouble ::
//      inventory.developmentCards.filterUnPlayed.getAmount(YearOfPlenty).toDouble ::
//      inventory.developmentCards.filterUnPlayed.getAmount(CatanPoint).toDouble ::  Nil
//  }
//
//}