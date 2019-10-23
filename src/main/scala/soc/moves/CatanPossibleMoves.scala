package soc.moves

import soc.board._
import soc.core.GameRules
import soc.inventory.Inventory.{PublicInfo, PerfectInfo}
import soc.inventory.resources.CatanResourceSet
import soc.inventory._
import soc.inventory.developmentCard.DevelopmentCardSet._
import soc.inventory.resources.CatanResourceSet._
import soc.state.{GameState, GamePhase}

case class CatanPossibleMoves (state: GameState[PublicInfo], inventory: PerfectInfo, playerPosition: Int)(implicit gameRules: GameRules) {

  val board: CatanBoard = state.board
  val currPlayer = state.currentPlayer
  val devCardsDeck = state.developmentCardsLeft

  def getPossibleMovesForState: Iterator[CatanMove] = {

    val devCardMoves = if (!inventory.playedDevCards.playedCardOnTurn(state.turn)) {
      getPossibleDevelopmentCard
    } else Nil

    state.phase match {
      case GamePhase.InitialPlacement => getPossibleInitialPlacements.iterator
      case GamePhase.Roll => (RollDiceMove :: devCardMoves).iterator
      case GamePhase.BuyTradeOrEnd => (EndTurnMove :: getPossibleBuilds ::: getPossiblePortTrades ::: devCardMoves).iterator
      case GamePhase.Discard => getPossibleDiscards()
      case GamePhase.MoveRobber => getPossibleRobberLocations.iterator
    }
  }

  def getPossibleInitialPlacements: List[InitialPlacementMove] = {
    val first = board.getSettlementVerticesForPlayer(currPlayer).isEmpty
    board.getPossibleSettlements(currPlayer, true).flatMap { v =>
      val settlementBoard = board.buildSettlement(v, currPlayer)
      settlementBoard.edgesFromVertex(v).filter(settlementBoard.canBuildRoad(_, currPlayer)).map { e =>
        InitialPlacementMove(first, v, e)
      }
    }.toList
  }

  lazy val getPossibleSettlements: List[BuildSettlementMove] = if (board.getNumSettlementsForPlayer(playerPosition) < gameRules.numSettlements && inventory.canBuild(Settlement.cost)) {
    board.getPossibleSettlements(currPlayer, false).toList.map(v => BuildSettlementMove(v))
  } else Nil

  lazy val getPossibleCities: List[BuildCityMove] = if (board.getNumCitiesForPlayer(playerPosition) < gameRules.numCities && inventory.canBuild(City.cost)) {
    board.getPossibleCities(currPlayer).toList.map(BuildCityMove)
  } else Nil

  lazy val getPossibleRoads: List[BuildRoadMove] = if (board.getNumRoadsForPlayer(playerPosition) < gameRules.numRoads && inventory.canBuild(Road.cost)) {
    board.getPossibleRoads(currPlayer).toList.map(BuildRoadMove)
  } else Nil

  lazy val getPossibleDevelopmentCards: List[BuyDevelopmentCardMove.type] = if(inventory.canBuild(DevelopmentCard.cost) && devCardsDeck > 0) {
    List(BuyDevelopmentCardMove)
  } else Nil


  def getPossibleBuilds: List[CatanBuildMove] = {
   getPossibleSettlements ::: getPossibleCities ::: getPossibleRoads ::: getPossibleDevelopmentCards
  }

  def getPossiblePortTrades: List[PortTradeMove] = {
    val resourceSet = inventory.resourceSet

    val ports = board.getPortsForPlayer(playerPosition)

    val _3to1 = ports.contains(Misc)
    Resource.list.flatMap { res =>
      val otherRes = Resource.list.filterNot(_ == res)
      val num = resourceSet.getAmount(res)
      if ( ports.contains(res.asInstanceOf[Port]) && num >= 2) {
        val give = CatanResourceSet().add(2, res)
        otherRes.map{ r =>
          val get = CatanResourceSet().add(1, r)
          PortTradeMove(give,  get)
        }
      }
      else if (_3to1 && num >= 3) {
        val give =  CatanResourceSet().add(3, res)
        otherRes.map{ r =>
          val get = CatanResourceSet().add(1, r)
          PortTradeMove(give,  get)
        }
      }
      else if(num >= 4) {
        val give =  CatanResourceSet().add(4, res)
        otherRes.map{ r =>
          val get = CatanResourceSet().add(1, r)
          PortTradeMove(give,  get)
        }
      }
      else Nil
    }
  }

  def getPossibleRobberLocations: List[MoveRobberAndStealMove] = {
    board.hexesWithNodes
      .filterNot(_.node == board.robberHex)
      .flatMap { hex =>
        board.playersOnHex(hex.node).filterNot(p => p == currPlayer || state.players.players.get(p).fold(false)(_.numCards <= 0)) match {
          case Nil => List(MoveRobberAndStealMove(hex.node, None))
          case list => list.map(n => MoveRobberAndStealMove(hex.node, Some(n)))
        }
      }
  }.toList

  def getPossibleDiscards(numToDiscard: Int = state.players.players.get(playerPosition).fold(0)(_.numCards / 2)) = {
    CatanSet.toList[Resource, Resources](inventory.resourceSet).combinations(numToDiscard).map { resList =>
      DiscardResourcesMove(CatanSet.fromList[Resource, CatanResourceSet[Int]](resList.toList))
    }
  }

  def getPossibleDevelopmentCard: List[CatanPlayCardMove] = {

    val knight: List[KnightMove] = if (!inventory.playedDevCards.playedCardOnTurn(state.turn) && inventory.developmentCards.filterUnPlayed.contains(Knight)) {
      getPossibleRobberLocations.map(KnightMove)
    } else Nil

    val monopoly: List[MonopolyMove] = if (!inventory.playedDevCards.playedCardOnTurn(state.turn) && inventory.developmentCards.filterUnPlayed.contains(Monopoly)) {
      Resource.list.map(MonopolyMove)
    } else Nil

    val yearOfPlenty: List[YearOfPlentyMove] = if (!inventory.playedDevCards.playedCardOnTurn(state.turn) && inventory.developmentCards.filterUnPlayed.contains(YearOfPlenty)) {
      Resource.list.flatMap { res1 =>
        Resource.list.map { res2 =>
          YearOfPlentyMove(res1, res2)
        }
      }
    } else Nil

    val roads: List[RoadBuilderMove] = if (!inventory.playedDevCards.playedCardOnTurn(state.turn) && inventory.developmentCards.filterUnPlayed.contains(RoadBuilder) && board.getNumRoadsForPlayer(playerPosition) < gameRules.numRoads) {
      val firsRoadsAndBoards = board.getPossibleRoads(currPlayer).map { road1 =>
        (road1, board.buildRoad(road1, currPlayer))
      }
      if (board.getNumRoadsForPlayer(playerPosition) < gameRules.numRoads - 1) {
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
  }
}
