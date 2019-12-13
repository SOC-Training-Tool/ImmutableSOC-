package soc.inventory.resources

import ResourceSet._
import soc.inventory.Resource

import scala.annotation.tailrec

case class PossibleHands(hands: Seq[(Int, Map[Int, Resources])]) {

  lazy val handsForPlayers: Map[Int, Seq[(Resources, Int)]] = hands.flatMap(_._2.keys).distinct.map { playerId =>
    playerId -> hands.map { case(m, map) =>
      if (map.get(playerId).isEmpty) println(hands.mkString("\n"))
      (map(playerId), m)}
  }.toMap

  lazy val probableHands: Map[Int, ProbableResourceSet] = handsForPlayers.map { case (playerId, allResSets) =>
    val numHands = allResSets.map(_._2).sum
    val resMap: Map[Resource, (Int, Double)] = Resource.list.map { res =>
      val knownAmount = allResSets.map(_._1.getAmount(res)).min
      val unknownAmount = allResSets.map { case (set, mult) =>
        (set.getAmount(res) - knownAmount) * mult
      }.sum.toDouble / numHands
      res -> (knownAmount, unknownAmount)
    }.toMap
    val knownMap: Map[Resource, Int] = resMap.view.mapValues(_._1).toMap
    val unknownMap: Map[Resource, Double] = resMap.view.mapValues(_._2).toMap
    val knownSet: Resources = ResourceSet(knownMap)
    val unknownSet: ResourceSet[Double] = ResourceSet(unknownMap)
    playerId -> ProbableResourceSet(knownSet, unknownSet)
  }

  def playerGainCards(player: Int, set: Resources): PossibleHands = copy {
    val ifEmpty = copy(Seq((1, Map(player -> ResourceSet.empty))))
    hands.headOption.fold(ifEmpty) {
      case (_, hand) if !hand.contains(player) => copy(hands.map { case (mult, playerHands) => (mult, playerHands + (player -> ResourceSet.empty[Int])) })
      case _ => copy()
    }.hands.map { case (mult, hand) => (mult, hand.map {
      case (`player`, resources) => player -> (resources.add(set))
      case (p, rm) => p -> rm
    })}
  }

  def playerLoseCards(player: Int, set: Resources): PossibleHands = copy {
    hands.filter {case (_, pr) => pr.get(player).fold(false)(_.contains(set))}.map { case (mult, pr) => (mult, pr.map {
      case (`player`, resources) => player -> resources.subtract(set)
      case (p, rm) => p -> rm
    })}
  }

  def stealUnknownCards(robber: Int, victim: Int): PossibleHands = copy {
    hands.flatMap { case (mult, handSet) =>
      handSet.get(victim).fold(copy().hands) { resSet =>
        resSet.getTypes.flatMap { res =>
          val set = ResourceSet(res)
          val amount = resSet.getAmount(res)
          PossibleHands(Seq((mult, handSet))).playerLoseCards(victim, set).playerGainCards(robber, set).hands.map { case (m, pr) => (m * amount, pr)}
        }
      }
    }.groupBy { case (_, f) => f }.toSeq.map {
      case (playerResources, playerResourcesWithMult) => (playerResourcesWithMult.map(_._1).sum, playerResources)
    }
  }

  @tailrec
  final def calculateHands(transactions: List[SOCTransactions]): PossibleHands = {
    if (transactions.isEmpty) copy()
    else calculateHands(transactions.head).calculateHands(transactions.tail)
  }

  def calculateHands(transaction: SOCTransactions): PossibleHands = transaction match {
    case Gain(player, set) => playerGainCards(player, set)
    case Lose(player, set) => playerLoseCards(player, set)
    case Steal(robber, victim, Some(set)) => playerLoseCards(victim, set).playerGainCards(robber, set)
    case Steal(robber, victim, None) => stealUnknownCards(robber, victim)
  }
}

object SOCPossibleHands {

  def empty = PossibleHands(Nil)

}

sealed trait SOCTransactions

case class Gain(playerId: Int, resourceSet: Resources) extends SOCTransactions
case class Lose(playerId: Int, resourceSet: Resources) extends SOCTransactions
case class Steal(robber: Int, victim: Int, resourceSet: Option[Resources]) extends SOCTransactions
