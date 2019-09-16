package state

import board._
import core._
import inventory._
import inventory.resources.CatanResourceSet.Resources
import inventory.resources._
import moves.{BuildCityMove, BuildRoadMove, BuildSettlementMove, BuyDevelopmentCardResult, DiscardResourcesMove, EndTurnMove, InitialPlacementMove, KnightResult, MonopolyResult, MoveResult, MoveRobberAndStealResult, PlayerTradeMove, PortTradeMove, RoadBuilderMove, RollResult, YearOfPlentyMove}
import state.player._

case class GameState[T <: Inventory[T]](
  board: CatanBoard,
  players: PlayerStateHelper[T],
  currTurn: Int,
  turnState: TurnState = TurnState(),
  bank: Resources,
  devCardsDeck: Int,
  transactions: List[SOCTransactions] = Nil,
  diceRolls: Int = 0
) {

  val turn: Int = diceRolls / 4

  val firstPlayerId: Int = players.firstPlayerId
  val lastPlayerId: Int = players.lastPlayerId

  def initialPlacement(result: InitialPlacementMove): GameState[T] = initialPlacement(result.first, result.settlement, result.road)
  def initialPlacement(first: Boolean, vertex: Vertex, edge: Edge): GameState[T] = {

    val newState = buildSettlement(vertex, buy = false).buildRoad(edge, buy = false)
    val resourcesFromSettlement = if (!first) {
      val resList = newState.board.adjacentHexes(vertex).flatMap { node =>
        node.hex.getResourceAndNumber.map {
          case (resource, _) => resource
        }
      }
      CatanResourceSet(resList:_*)
    } else CatanResourceSet.empty[Int]
    val newTransactions = if (!resourcesFromSettlement.isEmpty) List(Gain(currTurn, resourcesFromSettlement))
    else Nil
    val nextTurn = (first, currTurn) match {
      case (true, `lastPlayerId`) => currTurn
      case (true, _) => players.nextPlayer(currTurn)
      case (false, `firstPlayerId`) => currTurn
      case (false, _) => players.previousPlayer(currTurn)
    }

    newState.copy(
      players = newState.players.updateResources(transactions),
      bank = bank.subtract(resourcesFromSettlement),
      transactions = transactions ::: newTransactions,
      currTurn = nextTurn
    )
  }

  def rollDice(rollResult: RollResult): GameState[T] = rollDice(rollResult.roll)
  def rollDice(diceRoll: Roll): GameState[T] = {

    if (diceRoll.number == 7) return copy(diceRolls = diceRolls + 1, turnState = turnState.copy(canRollDice = false))

    val resForPlayers: Map[Int, Resources] = board.getResourcesGainedOnRoll(diceRoll.number)
    val totalResourcesCollected: Resources = resForPlayers.values.foldLeft(CatanResourceSet.empty[Int])(_.add(_))
    val actualResForPlayers = if (!bank.contains(totalResourcesCollected)) {
      val overflowTypes = {
        val total = bank.subtract(totalResourcesCollected)
        Resource.list.filter(res => total.contains(res))
      }
      resForPlayers.map[Int, Resources] { case (player, resourceSet) =>
        player -> overflowTypes.foldLeft(resourceSet) { case (set, res) =>
          set.subtract(set.getAmount(res), res)
        }
      }
    } else resForPlayers
    val trueTotalCollected = actualResForPlayers.values.foldLeft(CatanResourceSet.empty[Int])(_.add(_))

    val newTransactions: List[Gain] = players.getPlayers.map { player =>
      Gain(player.position, actualResForPlayers.getOrElse(player.position, CatanResourceSet()))
    }.filterNot(_.resourceSet.isEmpty).toList

    copy(
      players = players.updateResources(newTransactions),
      bank = bank.subtract(trueTotalCollected),
      // possibleHands = newPossHands,
      turnState = turnState.copy(canRollDice = false),
      transactions = transactions ::: newTransactions,
      diceRolls = diceRolls + 1
    )
  }

  def playersDiscardFromSeven(discard: DiscardResourcesMove): GameState[T] = playersDiscardFromSeven(discard.resourceSet)
  def playersDiscardFromSeven(cardsLostMap: Map[Int, Resources]): GameState[T] = {

    val newTransactions = cardsLostMap.toList.map { case (player, discard) => Lose(player, discard) }
    val totalLost: Resources = cardsLostMap.values.foldLeft(CatanResourceSet.empty[Int])(_.add(_))
    copy(
      bank = bank.add(totalLost),
      transactions = transactions ::: newTransactions,
      players = players.updateResources(newTransactions)
    )
  }

  def moveRobberAndSteal(result: MoveRobberAndStealResult): GameState[T] = moveRobberAndSteal(result.robberLocation, result.steal)
  def moveRobberAndSteal(robberLocation: Int, steal: Option[Steal]): GameState[T] = {

    val newTransactions = steal.fold(List.empty[SOCTransactions])(s => List(s))
    copy(
      board = board.copy(robberHex = robberLocation),
      // possibleHands = newPossHands,
      transactions = transactions ::: newTransactions,
      players = players.updateResources(newTransactions)
    )
  }

  def buildSettlement(result: BuildSettlementMove): GameState[T] = buildSettlement(result.vertex)
  def buildSettlement(vertex: Vertex, buy: Boolean = true): GameState[T] = {

    val newBoard = board.buildSettlement(vertex, currTurn)
    val newTransactions = if (buy) List(Lose(currTurn, Settlement.cost)) else Nil
    copy(
      players = players.buildSettlement(currTurn, vertex, newBoard).updateResources(newTransactions),
      board = newBoard,
      bank = if (buy) bank.add(Settlement.cost) else bank,
      transactions = transactions ::: newTransactions
    )
  }

  def buildCity(result: BuildCityMove): GameState[T] = buildCity(result.vertex)
  def buildCity(vertex: Vertex): GameState[T]  = {

    val newBoard = board.buildCity(vertex, currTurn)
    val newTransactions = List(Lose(currTurn, City.cost))
    copy(
      players = players.buildCity(currTurn, vertex, newBoard).updateResources(newTransactions),
      board = newBoard,
      bank = bank.add(City.cost),
      transactions = transactions ::: newTransactions
    )
  }

  def buildRoad(result: BuildRoadMove): GameState[T] = buildRoad(result.edge)
  def buildRoad(edge: Edge, buy: Boolean = true): GameState[T] = {

    val newBoard = board.buildRoad(edge, currTurn)
    val newTransactions = if (buy) List(Lose(currTurn, Road.cost)) else Nil
    copy(
      players = players.buildRoad(currTurn, edge, newBoard).updateResources(newTransactions),
      board = newBoard,
      bank = if (buy) bank.add(Road.cost) else bank,
      transactions = transactions ::: newTransactions
    )
  }

  def buyDevelopmentCard(result: BuyDevelopmentCardResult): GameState[T] = buyDevelopmentCard(result.card)
  def buyDevelopmentCard(card: Option[DevelopmentCard]): GameState[T] = {

    val newTransactions = List(Lose(currTurn, DevelopmentCard.cost))
    copy(
      players = players.buyDevelopmentCard(currTurn, card).updateResources(newTransactions),
      bank = bank.add(DevelopmentCard.cost),
      transactions = transactions ::: newTransactions,
      devCardsDeck = devCardsDeck - 1
    )
  }

  def playKnight(result: KnightResult): GameState[T] = playKnight(result.robber.robberLocation, result.robber.steal)
  def playKnight(robberLocation: Int, steal: Option[Steal]): GameState[T]  = {

    copy(
      players = players.playKnight(currTurn),
      turnState = turnState.copy(canPlayDevCard = false)
    ).moveRobberAndSteal(robberLocation, steal)
  }

  def playMonopoly(result: MonopolyResult): GameState[T] = playMonopoly(result.cardsLost)
  def playMonopoly(cardsLost: Map[Int, Resources]): GameState[T] = {

    val newTransactions = Gain(currTurn, cardsLost.values.fold(CatanResourceSet.empty[Int])(_.add(_))) ::
      cardsLost.map { case (player, cards) => Lose(player, cards) }.toList
    copy(
      players = players.playMonopoly(currTurn).updateResources(newTransactions),
      turnState = turnState.copy(canPlayDevCard = false),
      transactions = transactions ::: newTransactions
    )
  }

  def playYearOfPlenty(result: YearOfPlentyMove): GameState[T] = playYearOfPlenty(result.res1, result.res2)
  def playYearOfPlenty(card1: Resource, card2: Resource): GameState[T] = {

    val set = CatanResourceSet(card1, card2)
    val newTransactions = List(Gain(currTurn, set))
    copy(
      players = players.playYearOfPlenty(currTurn).updateResources(newTransactions),
      bank = bank.subtract(set),
      transactions = transactions ::: newTransactions,
      turnState = turnState.copy(canPlayDevCard = false)
    )
  }

  def playRoadBuilder(result: RoadBuilderMove): GameState[T] = playRoadBuilder(result.road1, result.road2)
  def playRoadBuilder(road1: Edge, road2: Option[Edge]): GameState[T] = {

    val firstRoadBoard = buildRoad(road1, buy = false)
    val newBoard = road2.fold(firstRoadBoard)(r => firstRoadBoard.buildRoad(r, buy = false))
    newBoard.copy(
      players = players.playRoadBuilder(currTurn),
      turnState = turnState.copy(canPlayDevCard = false)
    )
  }

    def trade(result: PlayerTradeMove): GameState[T] =  trade(result.to, result.give, result.get)
    def trade(to: Int, give: Resources, get: Resources): GameState[T] = {

      val newTransactions = List(
        Lose(currTurn, give),
        Lose(to, get),
        Gain(currTurn, get),
        Gain(to, give)
      )
      copy(
        transactions = transactions ::: newTransactions,
        players = players.updateResources(newTransactions)
      )

    }

  def portTrade(result: PortTradeMove): GameState[T] = portTrade(result.give, result.get)
  def portTrade(give: Resources, get: Resources): GameState[T] = {

    val newTransactions = List(
      Lose(currTurn, give),
      Gain(currTurn, get)
    )
    copy(
      bank = bank.subtract(get).add(give),
      transactions = transactions ::: newTransactions,
      players = players.updateResources(newTransactions)
    )
  }

  def endTurn: GameState[T] = copy(
    players = players.endTurn(currTurn),
    turnState = TurnState(),
    currTurn = players.nextPlayer(currTurn)
  )

  def apply(moveResult: MoveResult): GameState[T] = moveResult match {
    case r: RollResult => rollDice(r)
    case EndTurnMove => endTurn
    case r: InitialPlacementMove => initialPlacement(r)
    case r: MoveRobberAndStealResult => moveRobberAndSteal(r)
    case r: BuyDevelopmentCardResult => buyDevelopmentCard(r)
    case r: BuildRoadMove => buildRoad(r)
    case r: BuildSettlementMove => buildSettlement(r)
    case r: BuildCityMove => buildCity(r)
    case r: PortTradeMove => portTrade(r)
    case r: KnightResult => playKnight(r)
    case r: YearOfPlentyMove => playYearOfPlenty(r)
    case r: MonopolyResult => playMonopoly(r)
    case r: RoadBuilderMove => playRoadBuilder(r)
    case _ => this
  }
}

object GameState {

  def apply[T <: Inventory[T]](
    board: CatanBoard,
    players: PlayerStateHelper[T],
    rules: GameRules
  ): GameState[T] = GameState[T](
    board,
    players,
    players.firstPlayerId,
    TurnState(),
    rules.initBank,
    rules.initDevCardAmounts.getTotal
  )
}