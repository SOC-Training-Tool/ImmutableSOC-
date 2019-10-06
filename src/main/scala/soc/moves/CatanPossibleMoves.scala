package soc.moves

import soc.proto.ProtoCoder.ops._
import soc.board._
import BaseCatanBoard.baseBoardMapping
import soc.board.ProtoImplicits._

import protos.soc.gameState.PublicGameState
import soc.core.GameRules
import soc.inventory.Inventory.PerfectInfo
import soc.inventory.resources.CatanResourceSet
import soc.inventory._
import soc.inventory.developmentCard.DevelopmentCardSet._
import soc.inventory.resources.CatanResourceSet.Resources
import soc.state.GameState

/**
  * Copyright nDimensional, Inc. 2015. All rights reserved.
  */
case class CatanPossibleMoves (state: PublicGameState, inventory: PerfectInfo, playerPosition: Int)(implicit gameRules: GameRules) {

  val board: CatanBoard = state.board.proto
  val currPlayer = state.currentTurnPlayer
  val devCardsDeck = state.developmentCardsLeft

  def getPossibleMovesForState: List[CatanMove] = {

    val devCardMoves = if (state.canPlayCard) {
      getPossibleDevelopmentCard
    } else Nil

    val beforeOrAfterDiceMoves = if (state.canRollDice) List(RollDiceMove)
    else {
      EndTurnMove ::
        getPossibleBuilds :::
        getPossiblePortTrades
      //getPossibleTrades(soc.state.soc.state.player, soc.state.game)
    }

    devCardMoves ::: beforeOrAfterDiceMoves
  }

  def getPossibleInitialPlacements(first: Boolean): Seq[InitialPlacementMove] = {
    board.getPossibleSettlements(currPlayer.position, true).flatMap { v =>
      val settlementBoard = board.buildSettlement(v, currPlayer.position)
      settlementBoard.edgesFromVertex(v).filter(settlementBoard.canBuildRoad(_, currPlayer.position)).map { e =>
        InitialPlacementMove(first, v, e)
      }
    }
  }

  lazy val getPossibleSettlements: List[BuildSettlementMove] = if (board.getNumSettlementsForPlayer(playerPosition) < gameRules.numSettlements && inventory.canBuild(Settlement.cost)) {
    board.getPossibleSettlements(currPlayer.position, false).toList.map(v => BuildSettlementMove(v))
  } else Nil

  lazy val getPossibleCities: List[BuildCityMove] = if (board.getNumCitiesForPlayer(playerPosition) < gameRules.numCities && inventory.canBuild(City.cost)) {
    board.getPossibleCities(currPlayer.position).toList.map(BuildCityMove)
  } else Nil

  lazy val getPossibleRoads: List[BuildRoadMove] = if (board.getNumRoadsForPlayer(playerPosition) < gameRules.numRoads && inventory.canBuild(Road.cost)) {
    board.getPossibleRoads(currPlayer.position).toList.map(BuildRoadMove)
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
        board.playersOnHex(hex.node).filterNot(p => p == currPlayer.position || state.playerStates.get(p).fold(false)(_.publicInventory.numCards <= 0)) match {
          case Nil => List(MoveRobberAndStealMove(hex.node, None))
          case list => list.map(n => MoveRobberAndStealMove(hex.node, Some(n)))
        }
      }
  }.toList
    def getPossibleDiscards(numToDiscard: Int = state.playerStates.get(playerPosition).fold(0)(_.publicInventory.numCards / 2)) = {
    CatanSet.toList[Resource, Resources](inventory.resourceSet).combinations(numToDiscard).map { resList =>
      DiscardResourcesMove(Map(playerPosition -> CatanResourceSet.fromList(resList: _*)))
    }
  }

  def getPossibleDevelopmentCard: List[CatanPlayCardMove] = {

    val knight: List[KnightMove] = if (!inventory.playedDevCards.containsCardOnTurn(state.turn) && inventory.notPlayedDevCards.contains(Knight)) {
      getPossibleRobberLocations.map(KnightMove)
    } else Nil

    val monopoly: List[MonopolyMove] = if (!inventory.playedDevCards.containsCardOnTurn(state.turn) && inventory.notPlayedDevCards.contains(Monopoly)) {
      Resource.list.map(MonopolyMove)
    } else Nil

    val yearOfPlenty: List[YearOfPlentyMove] = if (!inventory.playedDevCards.containsCardOnTurn(state.turn) && inventory.notPlayedDevCards.contains(YearOfPlenty)) {
      Resource.list.flatMap { res1 =>
        Resource.list.map { res2 =>
          YearOfPlentyMove(res1, res2)
        }
      }
    } else Nil

    val roads: List[RoadBuilderMove] = if (!inventory.playedDevCards.containsCardOnTurn(state.turn) && inventory.notPlayedDevCards.contains(RoadBuilder) && board.getNumRoadsForPlayer(playerPosition) < gameRules.numRoads) {
      val firsRoadsAndBoards = board.getPossibleRoads(currPlayer.position).map { road1 =>
        (road1, board.buildRoad(road1, currPlayer.position))
      }
      if (board.getNumRoadsForPlayer(playerPosition) < gameRules.numRoads - 1) {
        firsRoadsAndBoards.flatMap {case (road1, newBoard) =>
          newBoard.getPossibleRoads(currPlayer.position).map { road2 =>
            RoadBuilderMove(road1, Some(road2))
          }
        }
      } else firsRoadsAndBoards.map { case (edge, _) => RoadBuilderMove(edge, None)}
    }.toList
    else Nil

    knight ::: {
      if (state.canRollDice) Nil
      else monopoly ::: yearOfPlenty ::: roads
    }
  }
}
