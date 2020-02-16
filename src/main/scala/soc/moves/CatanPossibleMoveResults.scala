package soc.moves

import soc.board.{BoardConfiguration, BoardRules}
import soc.core.Roll
import soc.core.Roll._
import soc.inventory.{CatanSet, Inventory, InventoryHelper, ProbableInfoInventoryHelper, Resource}
import soc.inventory.Inventory.{PerfectInfo, ProbableInfo}
import soc.inventory.developmentCard.DevelopmentCardSet
import soc.inventory.developmentCard.DevelopmentCardSet.DevelopmentCardSet
import soc.inventory.resources.ResourceSet
import soc.inventory.resources.ResourceSet.{ResourceSet, Resources}
import soc.moves.PossibleMoves.ResourceSetExtractor
import soc.moves2.RollDiceMove
import soc.state.GameState
import util.CombinationMapIterator

object CatanPossibleMoveResults {

  private object helpers {

    def diceRobber[I <: Inventory[I], B <: BoardConfiguration](state: GameState[I, B], moveResults: List[MoveResult], playerPosition: Int, move: MoveRobberAndStealMove)(implicit calculator: MoveResultCalculator[I]): ProbState[I, B] = robber(state, moveResults, playerPosition, move, MoveRobberAndStealResult.tupled)
    def knightRobber[I <: Inventory[I], B <: BoardConfiguration](state: GameState[I, B], moveResults: List[MoveResult], playerPosition: Int, move: MoveRobberAndStealMove)(implicit calculator: MoveResultCalculator[I]): ProbState[I, B] = robber(state, moveResults, playerPosition, move, a => KnightResult(MoveRobberAndStealResult.tupled(a)))

    def robber[I <: Inventory[I], B <: BoardConfiguration](state: GameState[I, B], moveResults: List[MoveResult], playerPosition: Int, move: MoveRobberAndStealMove, f: ((Seq[Int], Int, Option[RobPlayer])) => MoveResult)(implicit calculator: MoveResultCalculator[I]): ProbState[I, B] = {
      move.playerStole.fold[ProbState[I, B]] {
        val moveResult = f(Nil, move.node, None)
        SimpleProbState(state(moveResult).state, moveResults ::: List(moveResult))
      } { p =>
        WeightedProbState {
          calculator.getSteal(state, playerPosition, move.node, p).map[StateProbState[I, B]] { case (probability, result) =>
            val moveResult = f(result.viewableBy, result.robberLocation, result.steal)
            StateProbState(probability, SimpleProbState(state(moveResult).state, moveResults ::: List(moveResult)))
          }.iterator
        }
      }
    }

    def discardResources[I <: Inventory[I], B <: BoardConfiguration](state: GameState[I, B], moveResults: List[MoveResult], possibleDiscards: Map[Int, Iterator[DiscardResourcesMove]], playerPosition: Int)(implicit calculator: MoveResultCalculator[I]): ProbState[I, B] = UniformProbState {
      val allPossibleDiscards: Iterator[DiscardResourcesResult] = CombinationMapIterator.getIterator(possibleDiscards).map(discard => DiscardResourcesResult(discard.view.mapValues(_.resourceSet).toMap))
      allPossibleDiscards.map { discardResult =>
        UniformProbState {
          val discardResultState = state.apply(discardResult).state
          PossibleMoves.getPossibleRobberLocations(discardResultState.toPublicGameState).map {
            diceRobber(discardResultState, moveResults ::: List(discardResult), playerPosition, _)
          }.iterator
        }
      }
    }
  }

    def apply[I <: Inventory[I], B <: BoardConfiguration](state: GameState[I, B], move: CatanMove, playerPosition: Int)(implicit calculator: MoveResultCalculator[I], boardRules: BoardRules[B]): ProbState[I, B] = {
      val robberRoll = boardRules.robberRoll
      val minRoll: Int = boardRules.validRolls.map(_.number).min
      val maxRoll: Int = boardRules.validRolls.map(_.number).max
      move match {
        case perfectInfoMove: PerfectInfoMove => SimpleProbState(state(perfectInfoMove).state, Seq(perfectInfoMove))
        case RollDiceMove => WeightedProbState {
          (minRoll to maxRoll).iterator.map[StateProbState[I, B]] {
            case `robberRoll` =>
              val roll = Roll(robberRoll)
              StateProbState(roll.prob, {
                val rollResult = RollResult(roll)
                val rollResultState = state(rollResult).state
                val playersToDiscard: Seq[Int] = rollResultState.expectingDiscard
                val possibleDiscards: Map[Int, Iterator[DiscardResourcesMove]] = calculator.getAllDiscards(rollResultState, playersToDiscard)
                helpers.discardResources(rollResultState, List(rollResult), possibleDiscards, playerPosition)
              })
            case r =>
              val roll = Roll(r)
              val rollResult = RollResult(roll)
              StateProbState(roll.prob, SimpleProbState(state(rollResult).state, Seq(rollResult)))
          }
        }
        case discard: DiscardResourcesMove =>
          val playersToDiscard = state.expectingDiscard.filterNot(_ == playerPosition)
          val possibleDiscards = calculator.getAllDiscards(state, playersToDiscard.filterNot(_ == playerPosition))
          helpers.discardResources(state, Nil, possibleDiscards ++ Map(playerPosition -> Iterator(discard)), playerPosition)
        case robberMove: MoveRobberAndStealMove => helpers.diceRobber(state, Nil, playerPosition, robberMove)
        case BuyDevelopmentCardMove =>
          val totalDevCardsBought = calculator.boughtDevCardsProbabilities(state)
          val leftInDeck = DevelopmentCardSet(state.rules.initDevCardAmounts.amountMap.view.mapValues(_.toDouble).toMap).subtract(totalDevCardsBought)
          WeightedProbState {
            leftInDeck.getTypes.map[StateProbState[I, B]] { card =>
              val prob = leftInDeck.getAmount(card) / leftInDeck.getTotal
              val result = BuyDevelopmentCardResult(Seq(playerPosition), Some(card))
              StateProbState(prob, SimpleProbState(state(result).state, Seq(result)))
            }.iterator
          }
        case knight: KnightMove => helpers.knightRobber(state, Nil, playerPosition, knight.robber)
        case MonopolyMove(res) => WeightedProbState {
          val monopolyResults = calculator.monopolyResults(state, res, playerPosition)
          monopolyResults.iterator.map[StateProbState[I, B]] { case ProbabilityOfStateIfMove(probability, moveResults, newState) =>
            StateProbState(probability, SimpleProbState(newState, moveResults))
          }
        }
      }
    }

  implicit val perfectInfoResultCalculator: MoveResultCalculator[PerfectInfo] = new MoveResultCalculator[PerfectInfo] {

    override implicit val resourceSetExtractor: ResourceSetExtractor[PerfectInfo] = PossibleMoves.perfectInfoResourceSetExtractor

    override def boughtDevCardsProbabilities[B <: BoardConfiguration](state: GameState[PerfectInfo, B]): DevelopmentCardSet[Double] = {
      val totalDevCardsBought = CatanSet.fromList(state.players.players.values.flatMap { _.inventory.developmentCards.toList}.toSeq)
      val leftInDeck = state.rules.initDevCardAmounts.subtract(totalDevCardsBought)
      CatanSet.fromMap(leftInDeck.getTypes.map { card =>
        card -> leftInDeck.getAmount(card).toDouble / leftInDeck.getTotal
      }.toMap)
    }

    override def monopolyResults[B <: BoardConfiguration](state: GameState[PerfectInfo, B], res: Resource, playerPosition: Int): Seq[ProbabilityOfStateIfMove[PerfectInfo, B]] = {
      val resMap: Map[Int, ResourceSet[Int]] = state.players.players.filterNot(_._1 == playerPosition).view.mapValues(ps => ResourceSet[Int](Map(res -> ps.inventory.resourceSet.getAmount(res)))).toMap
      val result = MonopolyResult(resMap)
      Seq(ProbabilityOfStateIfMove(1.0, Seq(result), state.apply(result).state))
    }

    override def getSteal[B <: BoardConfiguration](state: GameState[PerfectInfo, B], playerPosition: Int, loc: Int, playerStole: Int): Seq[(Double, MoveRobberAndStealResult)] = {
      val resources: Resources = state.players.getPlayer(playerStole).inventory.resourceSet
      resources.getTypes.map { res =>
        val prob = resources.getAmount(res).toDouble / resources.getTotal
        (prob, MoveRobberAndStealResult(Seq(playerPosition, playerStole), loc, Some(RobPlayer(playerStole, Some(res)))))
      }
    }
  }

  implicit val probableInfoResultCalculator: MoveResultCalculator[ProbableInfo] = new MoveResultCalculator[ProbableInfo] {
    override implicit val resourceSetExtractor: ResourceSetExtractor[ProbableInfo] = PossibleMoves.probableInfoResourceSetExtractor

    override def getSteal[B <: BoardConfiguration](state: GameState[ProbableInfo, B], playerPosition: Int, loc: Int, playerStole: Int): Seq[(Double, MoveRobberAndStealResult)] = {
      val probableResSet = state.players.getPlayer(playerStole).inventory.probableResourceSet
      probableResSet.getTypes.map { res =>
        val prob = probableResSet.getProbabilityOfResourceInHand(res)
        (prob, MoveRobberAndStealResult(Seq(playerPosition, playerStole), loc, Some(RobPlayer(playerStole, Some(res)))))
      }
    }

    override def boughtDevCardsProbabilities[B <: BoardConfiguration](state: GameState[ProbableInfo, B]): DevelopmentCardSet[Double] = {
      val probableHelper: ProbableInfoInventoryHelper = InventoryHelper.get[ProbableInfo, ProbableInfoInventoryHelper](state.players.inventoryHelper)
      probableHelper.possibleDevCards.prob
    }

    override def monopolyResults[B <: BoardConfiguration](state: GameState[ProbableInfo, B], res: Resource, playerPosition: Int): Seq[ProbabilityOfStateIfMove[ProbableInfo, B]] = {
      val probableHelper: ProbableInfoInventoryHelper = InventoryHelper.get[ProbableInfo, ProbableInfoInventoryHelper](state.players.inventoryHelper)
      val possibleHands = probableHelper.possibleHands
      val total = possibleHands.hands.map(_._1).sum
      possibleHands.hands.map { case (mult, handMap) =>
        val resMap: Map[Int, ResourceSet[Int]] = handMap.filterNot(_._1 == playerPosition).view.mapValues(ps => ResourceSet[Int](Map(res -> ps.getAmount(res)))).toMap
        val result = MonopolyResult(resMap)
        (ProbabilityOfStateIfMove(mult.toDouble / total, Seq(result), state.apply(result).state))
      }
    }
  }
}

trait MoveResultCalculator[T <: Inventory[T]] {

  implicit val resourceSetExtractor: ResourceSetExtractor[T]

  def getAllDiscards[B <: BoardConfiguration](state: GameState[T, B], playersToDiscard: Seq[Int]): Map[Int, Iterator[DiscardResourcesMove]] = playersToDiscard.map { p =>
    val inventory = state.players.getPlayer(p).inventory
    p -> PossibleMoves.getPossibleDiscards(inventory, p)()
  }.toMap

  def getSteal[B <: BoardConfiguration](state: GameState[T, B], playerPosition: Int, loc: Int, playerStole: Int): Seq[(Double, MoveRobberAndStealResult)]

  def boughtDevCardsProbabilities[B <: BoardConfiguration](state: GameState[T, B]): DevelopmentCardSet[Double]

  def monopolyResults[B <: BoardConfiguration](state: GameState[T, B], resource: Resource, playerPosition: Int): Seq[ProbabilityOfStateIfMove[T, B]]

}

case class ProbabilityOfStateIfMove[T <: Inventory[T], B <: BoardConfiguration](probability: Double, results: Seq[MoveResult], state: GameState[T, B])

object ProbState {

  def eval[I <: Inventory[I], B <: BoardConfiguration](probState: ProbState[I, B], e: GameState[I, B] => Double): Double = probState match {
    case SimpleProbState(state, _) => e(state)
    case StateProbState(probability, probState) => probability * eval(probState, e)
    case WeightedProbState(probStates) => probStates.map(eval(_, e)).sum
    case UniformProbState(probStates) =>
      val (a, b) = probStates.duplicate
      a.map(eval(_, e)).sum / b.length.toDouble
  }

  def statesWithProb[I <: Inventory[I], B <: BoardConfiguration](probState: ProbState[I, B]): Iterator[ProbabilityOfStateIfMove[I, B]] = probState match {
    case SimpleProbState(state, moveResults) => Iterator(ProbabilityOfStateIfMove(1.0, moveResults, state))
    case StateProbState(probability, probState) => statesWithProb(probState).map(s => s.copy(probability = s.probability * probability))
    case WeightedProbState(probStates) => probStates.flatMap(s => statesWithProb(s))
    case UniformProbState(probStates) =>
      val (a, b) = probStates.duplicate
      a.flatMap(s => statesWithProb(s)).map(s => s.copy(probability = s.probability / b.length.toDouble))
  }
}

sealed trait ProbState[I <: Inventory[I], B <: BoardConfiguration]
case class UniformProbState[I <: Inventory[I], B <: BoardConfiguration](probStates: Iterator[ProbState[I, B]]) extends ProbState[I, B]
case class WeightedProbState[I <: Inventory[I], B <: BoardConfiguration](probStates: Iterator[StateProbState[I, B]]) extends ProbState[I, B]
case class StateProbState[I <: Inventory[I], B <: BoardConfiguration](probability: Double, probState: ProbState[I, B]) extends ProbState[I, B]
case class SimpleProbState[I <: Inventory[I], B <: BoardConfiguration](state: GameState[I, B], moveResults: Seq[MoveResult]) extends ProbState[I, B]