//package soc.moves
//
//import soc.board._
//import soc.inventory.Inventory.{PerfectInfo, ProbableInfo, PublicInfo}
//import soc.inventory.resources.ResourceSet
//import soc.inventory._
//import soc.inventory.resources.ResourceSet._
//import soc.moves2
//import soc.moves2.{RollDiceMove, build}
//import soc.moves2.build.BuildSettlementMove
//import soc.state.{GamePhase, GameState}
//
//object PossibleMoves {
//
//  trait ResourceSetExtractor[T <: Inventory[T]] {
//    def extractResources(inventory: T): Seq[Resources]
//  }
//
//  implicit val perfectInfoResourceSetExtractor: ResourceSetExtractor[PerfectInfo] = inv => Seq(inv.resourceSet)
//  implicit val probableInfoResourceSetExtractor: ResourceSetExtractor[ProbableInfo] = inv => inv.possibleResourceSets
//
//  def getMovesForState[T <: Inventory[T], B <: BoardConfiguration](state: GameState[PublicInfo, B], inventory: T, playerPosition: Int)(implicit resourceExtractor: ResourceSetExtractor[T]): IterableOnce[CatanMove] = {
//    val devCardMoves = if (!inventory.playedDevCards.playedCardOnTurn(state.turn)) {
//      getPossibleDevelopmentCard(state, inventory)
//    } else Nil
//
//    val playPoints = if (inventory.canPlayCard(CatanPoint, state.turn)) List(RevealPoint) else Nil
//
//    state.phase match {
//      case GamePhase.InitialPlacement if playerPosition == state.currentPlayer => getInitialPlacements(state).iterator
//      case GamePhase.Discard => getPossibleDiscards(inventory, playerPosition)()
//      case GamePhase.MoveRobber if playerPosition == state.currentPlayer => getPossibleRobberLocations(state).iterator
//      case GamePhase.Roll if playerPosition == state.currentPlayer => (RollDiceMove :: devCardMoves ::: playPoints).iterator
//      case GamePhase.BuyTradeOrEnd if playerPosition == state.currentPlayer => (EndTurnMove :: getPossibleBuilds(state, inventory) ::: getPossiblePortTrades(state, inventory) ::: devCardMoves ::: playPoints).iterator
//
//      case _ if playerPosition == state.currentPlayer && inventory.canPlayCard(CatanPoint, state.turn) => List(RevealPoint, EndTurnMove).iterator
//      case GamePhase.GameOver if playerPosition == state.currentPlayer => List(EndTurnMove).iterator
//
//      case _ => Nil
//    }
//  }
//
//  def getInitialPlacements(state: GameState[PublicInfo, _]): List[InitialPlacementMove] = {
//
//    val board: CatanBoard[_] = state.board
//    val currPlayer = state.currentPlayer
//
//    val first = board.getSettlementVerticesForPlayer(currPlayer).isEmpty
//    board.getPossibleSettlements(currPlayer, true).flatMap { v =>
//      val settlementBoard = board.buildSettlement(v, currPlayer)
//      settlementBoard.edgesFromVertex(v).filter(settlementBoard.canBuildRoad(_, currPlayer)).map { e =>
//        InitialPlacementMove(first, v, e)
//      }
//    }.toList
//  }
//
//  def getSettlements[T <: Inventory[T]](state: GameState[PublicInfo, _], inventory: T): List[BuildSettlementMove] = {
//    val board: CatanBoard[_] = state.board
//    val currPlayer = state.currentPlayer
//    val gameRules = state.rules
//
//    if (board.getNumSettlementsForPlayer(currPlayer) < gameRules.numSettlements && inventory.canSpend(Settlement.cost)) {
//      board.getPossibleSettlements(currPlayer, false).toList.map(v => build.BuildSettlementMove(v))
//    } else Nil
//  }
//
//  def getCities[T <: Inventory[T]](state: GameState[PublicInfo, _], inventory: T): List[BuildCityMove] = {
//    val board: CatanBoard[_] = state.board
//    val currPlayer = state.currentPlayer
//    val gameRules = state.rules
//
//    if (board.getNumCitiesForPlayer(currPlayer) < gameRules.numCities && inventory.canSpend(City.cost)) {
//      board.getPossibleCities(currPlayer).toList.map(BuildCityMove)
//    } else Nil
//  }
//
//  def getRoads[T <: Inventory[T]](state: GameState[PublicInfo, _], inventory: T): List[BuildRoadMove] = {
//    val board: CatanBoard[_] = state.board
//    val currPlayer = state.currentPlayer
//    val gameRules = state.rules
//
//    if (board.getNumRoadsForPlayer(currPlayer) < gameRules.numRoads && inventory.canSpend(Road.cost)) {
//      board.getPossibleRoads(currPlayer).toList.map(BuildRoadMove)
//    } else Nil
//  }
//
//  def getDevelopmentCards[T <: Inventory[T]](state: GameState[PublicInfo, _], inventory: T): List[BuyDevelopmentCardMove.type] = {
//    val devCardsDeck = state.developmentCardsLeft
//
//    if(inventory.canSpend(DevelopmentCard.cost) && devCardsDeck > 0) {
//      List(BuyDevelopmentCardMove)
//    } else Nil
//  }
//
//  def getPossibleBuilds[T <: Inventory[T]](state: GameState[PublicInfo, _], inventory: T): List[CatanMove] = {
//    getSettlements(state, inventory) ::: getCities(state, inventory) ::: getRoads(state, inventory) ::: getDevelopmentCards(state, inventory)
//  }
//
//  def getPossiblePortTrades[T <: Inventory[T]](state: GameState[PublicInfo, _], inventory: T): List[PortTradeMove] = {
//    val board = state.board
//    val currPlayer = state.currentPlayer
//
//    def canSpend(res: Resource, amount: Int): Boolean = inventory.canSpend(ResourceSet(Map(res -> 2)))
//
//    val ports = board.getPortsForPlayer(currPlayer)
//
//    val _3to1 = ports.contains(Misc)
//    Resource.list.flatMap { res =>
//      val otherRes: Seq[Resource with Port] = Resource.list.filterNot(_ == res)
//      if ( ports.contains(res) && canSpend(res, 2)) {
//        val give = ResourceSet().add(2, res)
//        otherRes.map{ r =>
//          val get = ResourceSet().add(1, r)
//          PortTradeMove(give,  get)
//        }
//      }
//      else if (_3to1 && canSpend(res, 3)) {
//        val give =  ResourceSet().add(3, res)
//        otherRes.map{ r =>
//          val get = ResourceSet().add(1, r)
//          PortTradeMove(give,  get)
//        }
//      }
//      else if(canSpend(res, 4)) {
//        val give =  ResourceSet().add(4, res)
//        otherRes.map{ r =>
//          val get = ResourceSet().add(1, r)
//          PortTradeMove(give,  get)
//        }
//      }
//      else Nil
//    }
//  }
//
//  def getPossibleRobberLocations(state: GameState[PublicInfo, _]): List[MoveRobberAndStealMove] = {
//    val board = state.board
//    val currPlayer = state.currentPlayer
//
//    board.hexesWithNodes
//      .filterNot(_.node == board.robberHex)
//      .flatMap { hex =>
//        board.playersOnHex(hex.node).filterNot(p => p == currPlayer || state.players.players.get(p).fold(false)(_.numCards <= 0)) match {
//          case Nil => List(MoveRobberAndStealMove(hex.node, None))
//          case list => list.map(n => MoveRobberAndStealMove(hex.node, Some(n)))
//        }
//      }
//  }.toList
//
//  def getPossibleDiscards[T <: Inventory[T]](inventory: T, playerPosition: Int)(numToDiscard: Int = inventory.numCards / 2)(implicit resourceExtractor: ResourceSetExtractor[T]): Iterator[DiscardResourcesMove] = {
//    resourceExtractor.extractResources(inventory).iterator.flatMap { resourceSet =>
//      CatanSet.toList(resourceSet).combinations(numToDiscard).map { resList =>
//        DiscardResourcesMove(ResourceSet(resList.toList:_*))
//      }
//    }
//  }
//
//  def getPossibleDevelopmentCard[T <: Inventory[T], B <: BoardConfiguration](state: GameState[PublicInfo, B], inventory: T): List[CatanMove] = if (state.canPlayCard) {
//
//    val board = state.board
//    val currPlayer = state.currentPlayer
//    val gameRules = state.rules
//
//    def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = inventory.canPlayCard(card, turn)
//
//    val knight: List[KnightMove] = if (canPlayCard(Knight, state.turn)) {
//      getPossibleRobberLocations(state).map(KnightMove)
//    } else Nil
//
//    val monopoly: List[MonopolyMove] = if (canPlayCard(Monopoly, state.turn)) {
//      Resource.list.map(MonopolyMove)
//    } else Nil
//
//    val yearOfPlenty: List[YearOfPlentyMove] = if (canPlayCard(YearOfPlenty, state.turn)) {
//      Resource.list.flatMap { res1 =>
//        Resource.list.map { res2 =>
//          YearOfPlentyMove(res1, res2)
//        }
//      }
//    } else Nil
//
//    val roads: List[RoadBuilderMove] = if (canPlayCard(RoadBuilder, state.turn) && board.getNumRoadsForPlayer(currPlayer) < gameRules.numRoads) {
//      val firsRoadsAndBoards = board.getPossibleRoads(currPlayer).map { road1 =>
//        val newBoard = board.buildRoad(road1, currPlayer)
//        (road1, newBoard)
//      }
//      if (board.getNumRoadsForPlayer(currPlayer) < gameRules.numRoads - 1) {
//        firsRoadsAndBoards.flatMap {case (road1, newBoard) =>
//          newBoard.getPossibleRoads(currPlayer).map { road2 =>
//            RoadBuilderMove(road1, Some(road2))
//          }
//        }
//      } else firsRoadsAndBoards.map { case (edge, _) => RoadBuilderMove(edge, None)}
//    }.toList
//    else Nil
//
//    knight ::: {
//      if (state.phase == GamePhase.Roll) Nil
//      else monopoly ::: yearOfPlenty ::: roads
//    }
//  } else Nil
//}
