package soc.state

import soc.proto.ProtoCoder.ops._
import soc.board._
import BaseCatanBoard.baseBoardMapping
import protos.soc.state.{Player, PublicGameState, TurnPhase}
import soc.board.ProtoImplicits._
import soc.inventory.ProtoImplicits._
import soc.core._
import soc.inventory._
import soc.inventory.resources.CatanResourceSet.Resources
import soc.inventory.resources._
import soc.moves._
import soc.state.player._
import soc.state.ProtoImplicits._

case class GameState[T <: Inventory[T]](
  publicGameState: PublicGameState,
  players: PlayerStateHelper[T]) {

  private def update(gameStateUpdate: PublicGameState => PublicGameState, playerUpdate: PlayerStateHelper[T] => PlayerStateHelper[T]): GameState[T] = {
    val newPlayers = playerUpdate(players)
    val newState = gameStateUpdate(publicGameState).copy(
      playerStates = newPlayers.proto
    )
    copy(newState, newPlayers)
  }

  val currentPlayer = publicGameState.currentTurnPlayer.position
  val board: CatanBoard = publicGameState.board.proto
  val bank = publicGameState.resourceBank.proto
  val developmentCardsInDeck = publicGameState.developmentCardsLeft
  val canRollDice = publicGameState.phase == TurnPhase.ROLL
  val canPlayCard = players.getPlayer(currentPlayer).playedDevCards.playedCardOnTurn(publicGameState.turn)
  val turnNumber = publicGameState.turn

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
      CatanResourceSet.fromList(resList: _*)
    } else CatanResourceSet.empty[Int]
    val newTransactions = if (!resourcesFromSettlement.isEmpty) List(Gain(currentPlayer, resourcesFromSettlement))
    else Nil
    val nextTurn = (first, currentPlayer) match {
      case (true, `lastPlayerId`) => currentPlayer
      case (true, _) => players.nextPlayer(currentPlayer)
      case (false, `firstPlayerId`) => currentPlayer
      case (false, _) => players.previousPlayer(currentPlayer)
    }

    newState.update (
      _.copy(resourceBank = bank.subtract(resourcesFromSettlement).proto, turn = nextTurn),
      _.updateResources(newTransactions)
    )
  }

  def rollDice(rollResult: RollResult): GameState[T] = rollDice(rollResult.roll)
  def rollDice(diceRoll: Roll): GameState[T] = {

    if (diceRoll.number == 7) return copy(publicGameState.copy(
      phase = TurnPhase.BUY_TRADE_OR_END
    ))

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

    update (
      _.copy(resourceBank = bank.subtract(trueTotalCollected).proto, phase = TurnPhase.BUY_TRADE_OR_END),
      _.updateResources(newTransactions)
    )
  }

  def playersDiscardFromSeven(discard: DiscardResourcesMove): GameState[T] = playersDiscardFromSeven(discard.player.position, discard.resourceSet)
  def playersDiscardFromSeven(player: Int, cardsLost: Resources): GameState[T] = {
    val newTransactions = List(Lose(player, cardsLost))
    val totalLost: Resources = cardsLost
    update (
      _.copy(resourceBank = bank.add(totalLost).proto),
      _.updateResources(newTransactions)
    )
  }

  def moveRobberAndSteal(result: MoveRobberAndStealResult): GameState[T] = moveRobberAndSteal(result.robberLocation, result.steal)
  def moveRobberAndSteal(robberLocation: Int, steal: Option[RobPlayer]): GameState[T] = {
    val newTransactions = steal.fold(List.empty[SOCTransactions])(s => List(Steal(currentPlayer, s.player.position, s.res.map(CatanResourceSet.fromList(_)))))
    update (
      _.copy(board = board.copy(robberHex = robberLocation).proto),
      _.updateResources(newTransactions)
    )
  }

  def buildSettlement(result: BuildSettlementMove): GameState[T] = buildSettlement(result.vertex)
  def buildSettlement(vertex: Vertex, buy: Boolean = true): GameState[T] = {
    val newBoard = board.buildSettlement(vertex, currentPlayer)
    val newTransactions = if (buy) List(Lose(currentPlayer, Settlement.cost)) else Nil
    update(
      _.copy(board = newBoard.proto, resourceBank = (if (buy) bank.add(Settlement.cost) else bank).proto),
      _.buildSettlement(currentPlayer, vertex, newBoard).updateResources(newTransactions),
    )
  }

  def buildCity(result: BuildCityMove): GameState[T] = buildCity(result.vertex)
  def buildCity(vertex: Vertex): GameState[T] = {
    val newBoard = board.buildCity(vertex, currentPlayer)
    val newTransactions = List(Lose(currentPlayer, City.cost))
    update(
      _.copy(board = newBoard.proto, resourceBank = bank.add(City.cost).proto),
      _.buildCity(currentPlayer, vertex, newBoard).updateResources(newTransactions)
    )
  }

  def buildRoad(result: BuildRoadMove): GameState[T] = buildRoad(result.edge)
  def buildRoad(edge: Edge, buy: Boolean = true): GameState[T] = {
    val newBoard = board.buildRoad(edge, currentPlayer)
    val newTransactions = if (buy) List(Lose(currentPlayer, Road.cost)) else Nil
    update(
      _.copy(board = newBoard.proto, resourceBank = (if (buy) bank.add(Road.cost) else bank).proto),
      _.buildRoad(currentPlayer, edge, newBoard).updateResources(newTransactions)
    )
  }

  def buyDevelopmentCard(result: BuyDevelopmentCardResult): GameState[T] = buyDevelopmentCard(result.card)
  def buyDevelopmentCard(card: Option[DevelopmentCard]): GameState[T] = {
    val newTransactions = List(Lose(currentPlayer, DevelopmentCard.cost))
    update(
      _.copy(resourceBank =  bank.add(DevelopmentCard.cost).proto, developmentCardsLeft = publicGameState.developmentCardsLeft - 1),
      _.buyDevelopmentCard(publicGameState.turn, currentPlayer, card).updateResources(newTransactions)
    )
  }

  def playKnight(result: KnightResult): GameState[T] = playKnight(result.robber.robberLocation, result.robber.steal)
  def playKnight(robberLocation: Int, steal: Option[RobPlayer]): GameState[T] = {
    update(
      _.copy(),
      _.playKnight(currentPlayer, turnNumber)
    ).moveRobberAndSteal(robberLocation, steal)
  }

  def playMonopoly(result: MonopolyResult): GameState[T] = playMonopoly(result.cardsLost)
  def playMonopoly(cardsLost: Map[Int, Resources]): GameState[T] = {
    val newTransactions = Gain(currentPlayer, cardsLost.values.fold(CatanResourceSet.empty[Int])(_.add(_))) ::
      cardsLost.map { case (player, cards) => Lose(player, cards) }.toList
    update(
      _.copy(),
      _.playMonopoly(currentPlayer, turnNumber).updateResources(newTransactions)
    )
  }

  def playYearOfPlenty(result: YearOfPlentyMove): GameState[T] = playYearOfPlenty(result.res1, result.res2)
  def playYearOfPlenty(card1: Resource, card2: Resource): GameState[T] = {
    val set = CatanResourceSet.fromList(card1, card2)
    val newTransactions = List(Gain(currentPlayer, set))
    update(
      _.copy(resourceBank = bank.subtract(set).proto),
      _.playYearOfPlenty(currentPlayer, turnNumber).updateResources(newTransactions)
    )
  }

  def playRoadBuilder(result: RoadBuilderMove): GameState[T] = playRoadBuilder(result.road1, result.road2)
  def playRoadBuilder(road1: Edge, road2: Option[Edge]): GameState[T] = {
    val firstRoadBoard = buildRoad(road1, buy = false)
    val newBoard = road2.fold(firstRoadBoard)(r => firstRoadBoard.buildRoad(r, buy = false))
    newBoard.update(
      _.copy(),
      _.playRoadBuilder(currentPlayer, turnNumber)
    )
  }

  def trade(result: PlayerTradeMove): GameState[T] = trade(result.to, result.give, result.get)
  def trade(to: Int, give: Resources, get: Resources): GameState[T] = {

    val newTransactions = List(
      Lose(currentPlayer, give),
      Lose(to, get),
      Gain(currentPlayer, get),
      Gain(to, give)
    )

    update(
      _ => publicGameState,
      _.updateResources(newTransactions)
    )
  }

  def portTrade(result: PortTradeMove): GameState[T] = portTrade(result.give, result.get)
  def portTrade(give: Resources, get: Resources): GameState[T] = {
    val newTransactions = List(
      Lose(currentPlayer, give),
      Gain(currentPlayer, get)
    )
    update(
      _.copy(resourceBank =  bank.subtract(get).add(give).proto),
      _.updateResources(newTransactions)
    )
  }

  def endTurn: GameState[T] = {
    val nextPlayerPosition = players.nextPlayer(currentPlayer)
    val nextPlayer = Player(
      players.getPlayer(nextPlayerPosition).name,
      players.getPlayer(nextPlayerPosition).position
    )
    update(
      _.copy(phase = TurnPhase.ROLL, currentTurnPlayer = nextPlayer ),
      _.endTurn(currentPlayer)
    )
  }

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
    playerNameIds: Seq[(String, Int)],
    rules: GameRules
  )(implicit factory: InventoryHelperFactory[T]): GameState[T] = {

    implicit val gameRules = rules
    implicit val helper = factory.createInventoryHelper
    val psHelper = PlayerStateHelper(playerNameIds.map { case (name, position) =>
      position -> PlayerState(name, position, helper.createInventory)
    }.toMap)
    GameState(board, psHelper, rules)
  }

  def apply[T <: Inventory[T]](
    board: CatanBoard,
    players: PlayerStateHelper[T],
    rules: GameRules
  ): GameState[T] = {
    val firstPlayer = Player(
      players.getPlayer(players.firstPlayerId).name,
      players.getPlayer(players.firstPlayerId).position
    )
    GameState[T](
      PublicGameState(
        board.proto,
        players.proto,
        rules.initBank.proto,
        rules.initDevCardAmounts.getTotal,
        firstPlayer,
        TurnPhase.ROLL,
        0),
      players)
  }
}