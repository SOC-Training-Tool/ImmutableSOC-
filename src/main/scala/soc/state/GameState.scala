package soc.state

import soc.board._
import soc.core._
import soc.inventory.Inventory.PublicInfo
import soc.inventory._
import soc.inventory.resources.ResourceSet._
import soc.inventory.resources._
import soc.moves.PossibleMoves.ResourceSetExtractor
import soc.moves._
import soc.moves2
import soc.moves2.build
import soc.state.GamePhase.GameOver
import soc.state.player._

case class GameState[T <: Inventory[T], B <: BoardConfiguration](
  board: CatanBoard[B],
  players: PlayerStateHelper[T],
  resourceBank: ResourceSet[Int],
  developmentCardsLeft: Int,
  currentPlayer: Int,
  phase: GamePhase.Value,
  turn: Int,
  implicit val rules: GameRules[B],
  expectingDiscard: List[Int])
  (implicit boardRules: BoardRules[B]) {

  lazy val toPublicGameState = GameState[PublicInfo, B](
    board,
    players.toPublicInfoHelper,
    resourceBank,
    developmentCardsLeft,
    currentPlayer,
    phase,
    turn,
    rules,
    expectingDiscard
  )

   def transition (
    board: CatanBoard[B] = board,
    players: PlayerStateHelper[T] = players,
    resourceBank: ResourceSet[Int] = resourceBank,
    developmentCardsLeft: Int = developmentCardsLeft,
    currentPlayer: Int = currentPlayer,
    phase: GamePhase.Value = phase,
    turn: Int = turn,
    rules: GameRules[B] = rules,
    expectingDiscard: List[Int] = expectingDiscard,
    transactions: List[SOCTransactions] = Nil
  ): StateTransition[T, B] = {
    val state = copy(board, players.updateResources(transactions), resourceBank, developmentCardsLeft, currentPlayer, phase, turn, rules, expectingDiscard)
    val nextState = if (state.isOver) state.copy(phase = GameOver) else state
    StateTransition( nextState, transactions)
  }

  val winner = players.players.values.toSeq.find(_.points >= rules.pointsToWin)
  val isOver = winner.isDefined

  val canRollDice = phase == GamePhase.Roll
  val canPlayCard = !players.getPlayer(currentPlayer).playedDevCards.playedCardOnTurn(turn)

  val firstPlayerId: Int = players.firstPlayerId
  val lastPlayerId: Int = players.lastPlayerId

  def getPlayerInfo[A](f: PlayerState[T] => A): Map[Int, A] = players.players.view.mapValues(f(_)).toMap

  def initialPlacement(result: InitialPlacementMove): StateTransition[T, B] = initialPlacement(result.first, result.settlement, result.road)
  def initialPlacement(first: Boolean, vertex: Vertex, edge: Edge): StateTransition[T, B] = {

    val newState = buildSettlement(vertex, buy = false).state.buildRoad(edge, buy = false).state
    val resourcesFromSettlement = if (!first) {
      val resList = newState.board.adjacentHexes(vertex).flatMap { node =>
        node.hex.getResourceAndNumber.map {
          case (resource, _) => resource
        }
      }
      ResourceSet(resList:_*)
    } else ResourceSet.empty[Int]
    val newTransactions = if (!resourcesFromSettlement.isEmpty) List(Gain(currentPlayer, resourcesFromSettlement))
    else Nil
    val nextTurn = (first, currentPlayer) match {
      case (true, `lastPlayerId`) => currentPlayer
      case (true, _) => players.nextPlayer(currentPlayer)
      case (false, `firstPlayerId`) => currentPlayer
      case (false, _) => players.previousPlayer(currentPlayer)
    }

    newState.transition(
      resourceBank = resourceBank.subtract(resourcesFromSettlement),
      currentPlayer = nextTurn,
      transactions = newTransactions,
      phase = if (!first && currentPlayer == firstPlayerId) GamePhase.Roll else GamePhase.InitialPlacement
    )
  }

  def rollDice(rollResult: RollResult): StateTransition[T, B] = rollDice(rollResult.roll)
  def rollDice(diceRoll: Roll): StateTransition[T, B] = {

    if (diceRoll.number == boardRules.robberRoll) {
      val newExpectingDiscard = players.players.filter { case (_, state) => state.numCards > rules.discardLimit}.keys.toList
      return transition(
        phase = if (newExpectingDiscard.isEmpty) GamePhase.MoveRobber else GamePhase.Discard,
        expectingDiscard = newExpectingDiscard
      )
    }

    val resForPlayers: Map[Int, Resources] = board.getResourcesGainedOnRoll(diceRoll.number)
    val totalResourcesCollected: Resources = resForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))
    val actualResForPlayers = if (!resourceBank.contains(totalResourcesCollected)) {
      val overflowTypes = {
        val total = resourceBank.subtract(totalResourcesCollected)
        Resource.list.filter(res => total.contains(res))
      }
      resForPlayers.map[Int, Resources] { case (player, resourceSet) =>
        player -> overflowTypes.foldLeft(resourceSet) { case (set, res) =>
          set.subtract(set.getAmount(res), res)
        }
      }
    } else resForPlayers
    val trueTotalCollected = actualResForPlayers.values.foldLeft(ResourceSet.empty[Int])(_.add(_))

    val newTransactions: List[Gain] = players.getPlayers.map { player =>
      Gain(player.position, actualResForPlayers.getOrElse(player.position, ResourceSet()))
    }.filterNot(_.resourceSet.isEmpty).toList

    transition(
      resourceBank = resourceBank.subtract(trueTotalCollected),
      phase = GamePhase.BuyTradeOrEnd,
      transactions = newTransactions
    )
  }

  def playersDiscardFromRobber(discard: DiscardResourcesResult): StateTransition[T, B] = playersDiscardFromRobber(discard.resourceLost)
  def playersDiscardFromRobber(cardsLost: Map[Int, Resources]): StateTransition[T, B] = {
    val newTransactions: List[SOCTransactions] = cardsLost.toSeq.map { case (pos, res) => Lose(pos, res)}.toList
    val totalLost: Resources = CatanSet.sum(cardsLost.values.toSeq)
    val newExpectingDiscard = expectingDiscard.filterNot(cardsLost.keys.toList.contains)
    transition (
      resourceBank = resourceBank.add(totalLost),
      transactions = newTransactions,
      phase = if (newExpectingDiscard.isEmpty) GamePhase.MoveRobber else GamePhase.Discard,
      expectingDiscard = newExpectingDiscard
    )
  }

  def moveRobberAndSteal(result: MoveRobberAndStealResult): StateTransition[T, B] = moveRobberAndSteal(result.robberLocation, result.steal)
  def moveRobberAndSteal(robberLocation: Int, steal: Option[RobPlayer]): StateTransition[T, B] = {
    val newTransactions = steal.fold(List.empty[SOCTransactions])(s => List(Steal(currentPlayer, s.player, s.res.map(ResourceSet(_)))))
    transition (
      board = board.copy(robberHex = robberLocation),
      transactions = newTransactions,
      phase = GamePhase.BuyTradeOrEnd
    )
  }

  def buildSettlement(result: build.BuildSettlementMove): StateTransition[T, B] = buildSettlement(result.vertex)
  def buildSettlement(vertex: Vertex, buy: Boolean = true): StateTransition[T, B] = {

    val newBoard = board.buildSettlement(vertex, currentPlayer)
    val newTransactions = if (buy) List(Lose(currentPlayer, Settlement.cost)) else Nil
    transition (
      board = newBoard,
      resourceBank = (if (buy) resourceBank.add(Settlement.cost) else resourceBank),
      players = players.buildSettlement(currentPlayer, vertex, newBoard),
      transactions = newTransactions
    )
  }

  def buildCity(result: BuildCityMove): StateTransition[T, B] = buildCity(result.vertex)
  def buildCity(vertex: Vertex): StateTransition[T, B] = {
    val newBoard = board.buildCity(vertex, currentPlayer)
    val newTransactions = List(Lose(currentPlayer, City.cost))
    transition (
      board = newBoard,
      resourceBank = resourceBank.add(City.cost),
      players = players.buildCity(currentPlayer, vertex, newBoard),
      transactions = newTransactions
    )
  }

  def buildRoad(result: BuildRoadMove): StateTransition[T, B] = buildRoad(result.edge)
  def buildRoad(edge: Edge, buy: Boolean = true): StateTransition[T, B] = {
    val newBoard = board.buildRoad(edge, currentPlayer)
    val newTransactions = if (buy) List(Lose(currentPlayer, Road.cost)) else Nil
    transition (
      board = newBoard,
      resourceBank = (if (buy) resourceBank.add(Road.cost) else resourceBank),
      players = players.buildRoad(currentPlayer, edge, newBoard),
      transactions = newTransactions
    )
  }

  def buyDevelopmentCard(result: BuyDevelopmentCardResult): StateTransition[T, B] = buyDevelopmentCard(result.card)
  def buyDevelopmentCard(card: Option[DevelopmentCard]): StateTransition[T, B] = {
    val newTransactions = List(Lose(currentPlayer, DevelopmentCard.cost))
    transition(
      resourceBank =  resourceBank.add(DevelopmentCard.cost),
      developmentCardsLeft = developmentCardsLeft - 1,
      players = players.buyDevelopmentCard(turn, currentPlayer, card),
      transactions = newTransactions
    )
  }

  def playKnight(result: KnightResult): StateTransition[T, B] = playKnight(result.robber.robberLocation, result.robber.steal)
  def playKnight(robberLocation: Int, steal: Option[RobPlayer]): StateTransition[T, B] = {
    transition(
      players = players.playKnight(currentPlayer, turn)
    ).state.moveRobberAndSteal(robberLocation, steal).state.transition(phase = phase)
  }

  def playMonopoly(result: MonopolyResult): StateTransition[T, B] = playMonopoly(result.cardsLost)
  def playMonopoly(cardsLost: Map[Int, Resources]): StateTransition[T, B] = {
    val newTransactions = Gain(currentPlayer, cardsLost.values.fold(ResourceSet.empty[Int])(_.add(_))) ::
      cardsLost.map { case (player, cards) => Lose(player, cards) }.toList
    transition (
      players = players.playMonopoly(currentPlayer, turn),
      transactions = newTransactions
    )
  }

  def playYearOfPlenty(result: YearOfPlentyMove): StateTransition[T, B] = playYearOfPlenty(result.res1, result.res2)
  def playYearOfPlenty(card1: Resource, card2: Resource): StateTransition[T, B] = {
    val set = ResourceSet(card1, card2)
    val newTransactions = List(Gain(currentPlayer, set))
    transition(
      resourceBank = resourceBank.subtract(set),
      players = players.playYearOfPlenty(currentPlayer, turn),
      transactions = newTransactions
    )
  }

  def playRoadBuilder(result: RoadBuilderMove): StateTransition[T, B] = playRoadBuilder(result.road1, result.road2)
  def playRoadBuilder(road1: Edge, road2: Option[Edge]): StateTransition[T, B] = {
    val firstRoadBoard = buildRoad(road1, buy = false).state
    val newBoard = road2.fold(firstRoadBoard)(r => firstRoadBoard.buildRoad(r, buy = false).state)
    newBoard.transition(
      players = players.playRoadBuilder(currentPlayer, turn)
    )
  }

  def revealPoint: StateTransition[T, B] = transition(players = players.playPointCard(currentPlayer, turn))

  def trade(result: PlayerTradeMove): StateTransition[T, B] = trade(result.to, result.give, result.get)
  def trade(to: Int, give: Resources, get: Resources): StateTransition[T, B] = {

    val newTransactions = List(
      Lose(currentPlayer, give),
      Lose(to, get),
      Gain(currentPlayer, get),
      Gain(to, give)
    )

    transition(
      transactions = newTransactions
    )
  }

  def portTrade(result: PortTradeMove): StateTransition[T, B] = portTrade(result.give, result.get)
  def portTrade(give: Resources, get: Resources): StateTransition[T, B] = {
    val newTransactions = List(
      Lose(currentPlayer, give),
      Gain(currentPlayer, get)
    )
    transition(
      resourceBank =  resourceBank.subtract(get).add(give),
      transactions = newTransactions
    )
  }


  def canDoMove(playerId: Int, move: CatanMove)(implicit resourceExtractor: ResourceSetExtractor[T]): Boolean = {
    val inventory = players.getPlayer(playerId).inventory
    val possibleMoves = PossibleMoves.getMovesForState(toPublicGameState, inventory, playerId)
    possibleMoves.iterator.contains(move)
  }

  def nextState(moveResult: MoveResult): GameState[T, B] = apply(moveResult).state

  def apply(moveResult: MoveResult): StateTransition[T, B] = moveResult match {
    case r: RollResult => rollDice(r)
    case EndTurnMove => endTurn
    case r: InitialPlacementMove => initialPlacement(r)
    case r: MoveRobberAndStealResult => moveRobberAndSteal(r)
    case r: BuyDevelopmentCardResult => buyDevelopmentCard(r)
    case r: BuildRoadMove => buildRoad(r)
    case r: build.BuildSettlementMove => buildSettlement(r)
    case r: BuildCityMove => buildCity(r)
    case r: PortTradeMove => portTrade(r)
    case r: KnightResult => playKnight(r)
    case r: YearOfPlentyMove => playYearOfPlenty(r)
    case r: MonopolyResult => playMonopoly(r)
    case r: RoadBuilderMove => playRoadBuilder(r)
    case RevealPoint => revealPoint
    case r: DiscardResourcesResult => playersDiscardFromRobber(r)
    case _ => transition()
  }

  def doMoveIfCan(moveResult: MoveResult)(implicit resourceExtractor: ResourceSetExtractor[T]): StateTransition[T, B] = {
    val playerId = moveResult match {
      case DiscardResourcesResult(res) => res.head._1
      case _ => currentPlayer
    }
    if (!canDoMove(playerId, moveResult.move)) {
      throw new Exception(s"Cannot do move ${moveResult.move}\n$this")
    }
    apply(moveResult)
  }
}

object GameState {

  def apply[T <: Inventory[T], B <: BoardConfiguration](board: CatanBoard[B], playerPos: Seq[Int], rules: GameRules[B])(implicit factory: InventoryHelperFactory[T], boardRules: BoardRules[B]): GameState[T, B] = {
    implicit val gameRules = rules
    val psHelper: PlayerStateHelper[T] = PlayerStateHelper[T](playerPos)
    GameState(board, psHelper, rules)
  }

  def apply[T <: Inventory[T], B <: BoardConfiguration](board: CatanBoard[B], players: PlayerStateHelper[T], rules: GameRules[B])(implicit boardRules: BoardRules[B]): GameState[T, B] = {
    implicit val gameRules = rules
    GameState[T, B](
      board,
      players,
      rules.initBank,
      rules.initDevCardAmounts.getTotal,
      players.firstPlayerId,
      GamePhase.InitialPlacement,
      0,
      rules,
      Nil)
  }
}

case class StateTransition[T <: Inventory[T], B <: BoardConfiguration](state: GameState[T, B], transactions: List[SOCTransactions])