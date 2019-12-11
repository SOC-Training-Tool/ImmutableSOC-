package soc.moves

import soc.board._
import soc.inventory.Inventory.{PerfectInfo, PublicInfo}
import soc.inventory.resources.CatanResourceSet
import soc.inventory._
import soc.inventory.resources.CatanResourceSet._
import soc.state.{GamePhase, GameState}

object PossibleMoves {

  trait ResourceSetExtractor[T <: Inventory[T]] {
    def extractResources(inventory: T): Seq[Resources]
  }

  implicit val perfectInfoResourceSetExtractor: ResourceSetExtractor[PerfectInfo] = inv => Seq(inv.resourceSet)

  def getMovesForState[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T, playerPosition: Int)(implicit resourceExtractor: ResourceSetExtractor[T]) = {
    val devCardMoves = if (!inventory.playedDevCards.playedCardOnTurn(state.turn)) {
      getDevelopmentCards(state, inventory)
    } else Nil

    state.phase match {
      case GamePhase.InitialPlacement => getInitialPlacements(state).iterator
      case GamePhase.Roll => (RollDiceMove :: devCardMoves).iterator
      case GamePhase.BuyTradeOrEnd => (EndTurnMove :: getPossibleBuilds(state, inventory) ::: getPossiblePortTrades(state, inventory) ::: devCardMoves).iterator
      case GamePhase.Discard => getPossibleDiscards(inventory, playerPosition)()
      case GamePhase.MoveRobber => getPossibleRobberLocations(state).iterator
    }
  }

  def getInitialPlacements(state: GameState[PublicInfo]): List[InitialPlacementMove] = {

    val board: CatanBoard = state.board
    val currPlayer = state.currentPlayer

    val first = board.getSettlementVerticesForPlayer(currPlayer).isEmpty
    board.getPossibleSettlements(currPlayer, true).flatMap { v =>
      val settlementBoard = board.buildSettlement(v, currPlayer)
      settlementBoard.edgesFromVertex(v).filter(settlementBoard.canBuildRoad(_, currPlayer)).map { e =>
        InitialPlacementMove(first, v, e)
      }
    }.toList
  }

  def getSettlements[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[BuildSettlementMove] = {
    val board: CatanBoard = state.board
    val currPlayer = state.currentPlayer
    val gameRules = state.rules

    if (board.getNumSettlementsForPlayer(currPlayer) < gameRules.numSettlements && inventory.canSpend(Settlement.cost)) {
      board.getPossibleSettlements(currPlayer, false).toList.map(v => BuildSettlementMove(v))
    } else Nil
  }

  def getCities[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[BuildCityMove] = {
    val board: CatanBoard = state.board
    val currPlayer = state.currentPlayer
    val gameRules = state.rules

    if (board.getNumCitiesForPlayer(currPlayer) < gameRules.numCities && inventory.canSpend(City.cost)) {
      board.getPossibleCities(currPlayer).toList.map(BuildCityMove)
    } else Nil
  }

  def getRoads[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[BuildRoadMove] = {
    val board: CatanBoard = state.board
    val currPlayer = state.currentPlayer
    val gameRules = state.rules

    if (board.getNumRoadsForPlayer(currPlayer) < gameRules.numRoads && inventory.canSpend(Road.cost)) {
      board.getPossibleRoads(currPlayer).toList.map(BuildRoadMove)
    } else Nil
  }

  def getDevelopmentCards[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[BuyDevelopmentCardMove.type] = {
    val devCardsDeck = state.developmentCardsLeft

    if(inventory.canSpend(DevelopmentCard.cost) && devCardsDeck > 0) {
      List(BuyDevelopmentCardMove)
    } else Nil
  }

  def getPossibleBuilds[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[CatanMove] = {
    getSettlements(state, inventory) ::: getCities(state, inventory) ::: getRoads(state, inventory) ::: getDevelopmentCards(state, inventory)
  }

  def getPossiblePortTrades[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[PortTradeMove] = {
    val board = state.board
    val currPlayer = state.currentPlayer

    def canSpend(res: Resource, amount: Int): Boolean = inventory.canSpend(CatanResourceSet.fromMap(Map(res -> 2)))

    val ports = board.getPortsForPlayer(currPlayer)

    val _3to1 = ports.contains(Misc)
    Resource.list.flatMap { res =>
      val otherRes: Seq[Resource with Port] = Resource.list.filterNot(_ == res)
      if ( ports.contains(res) && canSpend(res, 2)) {
        val give = CatanResourceSet().add(2, res)
        otherRes.map{ r =>
          val get = CatanResourceSet().add(1, r)
          PortTradeMove(give,  get)
        }
      }
      else if (_3to1 && canSpend(res, 3)) {
        val give =  CatanResourceSet().add(3, res)
        otherRes.map{ r =>
          val get = CatanResourceSet().add(1, r)
          PortTradeMove(give,  get)
        }
      }
      else if(canSpend(res, 4)) {
        val give =  CatanResourceSet().add(4, res)
        otherRes.map{ r =>
          val get = CatanResourceSet().add(1, r)
          PortTradeMove(give,  get)
        }
      }
      else Nil
    }
  }

  def getPossibleRobberLocations(state: GameState[PublicInfo]): List[MoveRobberAndStealMove] = {
    val board = state.board
    val currPlayer = state.currentPlayer

    board.hexesWithNodes
      .filterNot(_.node == board.robberHex)
      .flatMap { hex =>
        board.playersOnHex(hex.node).filterNot(p => p == currPlayer || state.players.players.get(p).fold(false)(_.numCards <= 0)) match {
          case Nil => List(MoveRobberAndStealMove(hex.node, None))
          case list => list.map(n => MoveRobberAndStealMove(hex.node, Some(n)))
        }
      }
  }.toList

  def getPossibleDiscards[T <: Inventory[T]](inventory: T, playerPosition: Int)(numToDiscard: Int = inventory.numCards / 2)(implicit resourceExtractor: ResourceSetExtractor[T]): Iterator[DiscardResourcesMove] = {
    resourceExtractor.extractResources(inventory).iterator.flatMap { resourceSet =>
      CatanSet.toList[Resource, Resources](resourceSet).combinations(numToDiscard).map { resList =>
        DiscardResourcesMove(CatanSet.fromList[Resource, CatanResourceSet[Int]](resList.toList))
      }
    }
  }

  def getPossibleDevelopmentCard[T <: Inventory[T]](state: GameState[PublicInfo], inventory: T): List[CatanMove] = if (state.canPlayCard) {

    val board = state.board
    val currPlayer = state.currentPlayer
    val gameRules = state.rules

    def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = inventory.canPlayCard(card, turn)

    val knight: List[KnightMove] = if (canPlayCard(Knight, state.turn)) {
      getPossibleRobberLocations(state).map(KnightMove)
    } else Nil

    val monopoly: List[MonopolyMove] = if (canPlayCard(Monopoly, state.turn)) {
      Resource.list.map(MonopolyMove)
    } else Nil

    val yearOfPlenty: List[YearOfPlentyMove] = if (canPlayCard(YearOfPlenty, state.turn)) {
      Resource.list.flatMap { res1 =>
        Resource.list.map { res2 =>
          YearOfPlentyMove(res1, res2)
        }
      }
    } else Nil

    val roads: List[RoadBuilderMove] = if (canPlayCard(RoadBuilder, state.turn) && board.getNumRoadsForPlayer(currPlayer) < gameRules.numRoads) {
      val firsRoadsAndBoards = board.getPossibleRoads(currPlayer).map { road1 =>
        (road1, board.buildRoad(road1, currPlayer))
      }
      if (board.getNumRoadsForPlayer(currPlayer) < gameRules.numRoads - 1) {
        firsRoadsAndBoards.flatMap {case (road1, newBoard) =>
          newBoard.getPossibleRoads(currPlayer).map { road2 =>
            RoadBuilderMove(road1, Some(road2))
          }
        }
      } else firsRoadsAndBoards.map { case (edge, _) => RoadBuilderMove(edge, None)}
    }.toList
    else Nil

    knight ::: {
      if (state.phase == GamePhase.Roll) Nil
      else monopoly ::: yearOfPlenty ::: roads
    }
  } else Nil
}
