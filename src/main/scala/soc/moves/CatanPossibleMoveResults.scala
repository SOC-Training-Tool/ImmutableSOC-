package soc.moves

import soc.core.Roll
import soc.inventory.{CatanSet, DevelopmentCard, Inventory, Resource}
import soc.inventory.Inventory.PerfectInfo
import soc.inventory.developmentCard.DevelopmentCardSet
import soc.inventory.resources.CatanResourceSet
import soc.inventory.resources.CatanResourceSet.Resources
import soc.moves.PossibleMoves.ResourceSetExtractor
import soc.state.GameState
import util.CombinationMapIterator

object CatanPossibleMoveResults {

  def apply[T <: Inventory[T]](state: GameState[T], move: CatanMove, playerPosition: Int)(implicit calculator: MoveResultCalculator[T]): Seq[ProbabilityOfStateIfMove[T]] = {
    calculator.calculateResults(state, move, playerPosition)
  }

  implicit val perfectInfoResultCalculator: MoveResultCalculator[PerfectInfo] = new MoveResultCalculator[PerfectInfo] {

    override implicit val resourceSetExtractor: ResourceSetExtractor[PerfectInfo] = PossibleMoves.perfectInfoResourceSetExtractor

    override def robberMoveResult(state: GameState[PerfectInfo], playerPosition: Int, moveRobberAndStealMove: MoveRobberAndStealMove): Seq[(Double, MoveRobberAndStealResult)] = moveRobberAndStealMove match {
      case MoveRobberAndStealMove(loc, None) => Seq((1.0, MoveRobberAndStealResult(Nil, loc, None)))
      case MoveRobberAndStealMove(loc, Some(p)) =>
        val resources: Resources = state.players.getPlayer(p).inventory.resourceSet
        resources.getTypes.map { res =>
          val prob = resources.getAmount(res).toDouble / resources.getTotal
          (prob, MoveRobberAndStealResult(Seq(playerPosition, p), loc, Some(RobPlayer(p, Some(res)))))
        }
    }

    override def boughtDevCardsProbabilities(state: GameState[PerfectInfo]): DevelopmentCardSet[Double] = {
      import DevelopmentCardSet._
      val totalDevCardsBought = CatanSet.fromList(state.players.players.values.flatMap { p =>
        p.inventory.developmentCards.toList
      }.toSeq)
      val leftInDeck = state.rules.initDevCardAmounts.subtract(totalDevCardsBought)
      CatanSet.fromMap(leftInDeck.getTypes.map { card =>
        card -> leftInDeck.getAmount(card).toDouble / leftInDeck.getTotal
      }.toMap)
    }

    override def monopolyResults(state: GameState[PerfectInfo], res: Resource, playerPosition: Int): Seq[ProbabilityOfStateIfMove[PerfectInfo]] = {
      val result = MonopolyResult(state.players.players.filterNot(_._1 == playerPosition).view.mapValues(ps => CatanResourceSet.fromMap(Map(res -> ps.inventory.resourceSet.getAmount(res)))).toMap)
      Seq(ProbabilityOfStateIfMove(1.0, Seq(result), state.apply(result).state))
    }

  }
}

trait MoveResultCalculator[T <: Inventory[T]] {

  implicit val resourceSetExtractor: ResourceSetExtractor[T]

  def calculateResults(state: GameState[T], move: CatanMove, playerPosition: Int): Seq[ProbabilityOfStateIfMove[T]] = move match {
    case perfectInfoMove: PerfectInfoMove => Seq(ProbabilityOfStateIfMove(1.0, Seq(perfectInfoMove), state(perfectInfoMove).state))
    case RollDiceMove => {
      (2 to 12).flatMap {
        case 7 => {
          val roll = Roll(7)
          val rollResult = RollResult(roll)
          val rollResultState: GameState[T] = state(rollResult).state

          val playersToDiscard: Seq[Int] = rollResultState.expectingDiscard
          val possibleDiscards = getAllDiscards(rollResultState, playersToDiscard)
          discardMoveResult(rollResultState, playerPosition, possibleDiscards).map { probOfState =>
            ProbabilityOfStateIfMove(roll.prob * probOfState.probability, rollResult +: probOfState.results, probOfState.state)
          }
        }
        case r =>
          val roll = Roll(r)
          val rollResult = RollResult(roll)
          Seq(ProbabilityOfStateIfMove(roll.prob, Seq(rollResult), state(rollResult).state))
      }
    }
    case discard: DiscardResourcesMove =>
      val playersToDiscard: Seq[Int] = state.expectingDiscard.filterNot(_ == playerPosition)
      val possibleDiscards = getAllDiscards(state, playersToDiscard.filterNot(_ == playerPosition))
      discardMoveResult(state, playerPosition, possibleDiscards ++ Map(playerPosition -> Iterator(discard)))
    case robber: MoveRobberAndStealMove => robberMoveResult(state, playerPosition, robber).map { case (prob, robberResult) =>
      ProbabilityOfStateIfMove(prob, Seq(robberResult), state.apply(robberResult).state)
    }
    case BuyDevelopmentCardMove =>
      import DevelopmentCardSet._
      val totalDevCardsBought = boughtDevCardsProbabilities(state)
      val leftInDeck = state.rules.initDevCardAmounts.amountMap.view.mapValues(_.toDouble).toMap.subtract(totalDevCardsBought)
      leftInDeck.getTypes.map { card =>
        val prob = leftInDeck.getAmount(card) / leftInDeck.getTotal
        val result = BuyDevelopmentCardResult(Seq(playerPosition), Some(card))
        ProbabilityOfStateIfMove(prob, Seq(result), state.apply(result).state)
      }
    case knight: KnightMove => robberMoveResult(state, playerPosition, knight.robber).map { case (prob, robberResult) =>
      val knightResult = KnightResult(robberResult)
      ProbabilityOfStateIfMove(prob, Seq(knightResult), state.apply(knightResult).state)
    }
    case MonopolyMove(res) => monopolyResults(state, res, playerPosition)
  }

  def getAllDiscards(state: GameState[T], playersToDiscard: Seq[Int]): Map[Int, Iterator[DiscardResourcesMove]] = playersToDiscard.map { p =>
    val inventory = state.players.getPlayer(p).inventory
    p -> PossibleMoves.getPossibleDiscards(inventory, p)()
  }.toMap

  def discardMoveResult(state: GameState[T], playerPosition: Int, possibleDiscards: Map[Int, Iterator[DiscardResourcesMove]]): Seq[ProbabilityOfStateIfMove[T]] = {
    val (allPossibleDiscards, b) = CombinationMapIterator.getIterator(possibleDiscards).map(discard => DiscardResourcesResult(discard.view.mapValues(_.resourceSet).toMap)).duplicate
    val total = b.length
    allPossibleDiscards.flatMap { discardResult =>
      val discardResultState = state.apply(discardResult).state
      PossibleMoves.getPossibleRobberLocations(discardResultState.toPublicGameState).flatMap {
        robberMoveResult(discardResultState, playerPosition, _)
      }.map { case (prob, moveRobberResult: MoveRobberAndStealResult) =>
        val moveRobberState = discardResultState.apply(moveRobberResult).state
        val totalProb = (prob) / total
        ProbabilityOfStateIfMove(totalProb, Seq(discardResult, moveRobberResult), moveRobberState)
      }
    }
  }.toSeq

  def robberMoveResult(state: GameState[T], playerPosition: Int, moveRobberAndStealMove: MoveRobberAndStealMove): Seq[(Double, MoveRobberAndStealResult)]

  def boughtDevCardsProbabilities(state: GameState[T]): DevelopmentCardSet[Double]

  def monopolyResults(state: GameState[T], resource: Resource, playerPosition: Int): Seq[ProbabilityOfStateIfMove[T]]

}


case class ProbabilityOfStateIfMove[T <: Inventory[T]](probability: Double, results: Seq[MoveResult], state: GameState[T])