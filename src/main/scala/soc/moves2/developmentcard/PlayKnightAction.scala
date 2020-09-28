package soc.moves2.developmentcard

import soc.board.BoardConfiguration
import soc.inventory.{DevelopmentCard, Inventory, InventoryHelper, InventoryItem, Knight, PerfectInfoInventory, Resource}
import soc.moves2.{MoveResultProvider, RobberAction, RobberMove, RobberMoveResult, SOCMove, SOCMoveResult, SOCPlayerPointsMap, SOCState}
import soc.moves2.MoveResultProvider.MoveResultProviderTransformer
import util.MapWrapper

case class PlayKnightMove(robberMove: RobberMove) extends PlayDevelopmentCardMove[PlayKnightMove] {
  override def player: Int = robberMove.player
  override def card: DevelopmentCard = Knight
}
case class PlayKnightMoveResult[II <: InventoryItem](robberMoveResult: RobberMoveResult[II]) extends SOCMoveResult[PlayKnightMove] {
  def player = robberMoveResult.player
  override def move: PlayKnightMove = PlayKnightMove(robberMoveResult.move.getMove)
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[PlayKnightMove]] = {
    robberMoveResult.getPerspectiveResults(playerIds).map {
      case (i, result: RobberMoveResult[II]) => i -> PlayKnightMoveResult(result)
    }
  }
}
case class PlayKnightAction[BOARD <: BoardConfiguration, STATE[P] <: SOCState[BOARD, Resource, P, STATE[P]]](cardsInDeck: Int, robberAction: RobberAction[BOARD, Resource, STATE]) extends PlayDevelopmentCardAction[BOARD, Resource, STATE, PlayKnightMove] {

  override val cardType: DevelopmentCard = Knight
  override type R = PlayKnightMoveResult[Resource]

  override val moveResultProvider: MoveResultProvider[BOARD, Resource, STATE, PlayKnightMove, PlayKnightMoveResult[Resource]] = robberAction.moveResultProvider.transform[PlayKnightMove, PlayKnightMoveResult[Resource]](_.robberMove, PlayKnightMoveResult.apply[Resource](_))
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PlayKnightMove] = {
    robberAction.getAllMoves(state, inv, position).map {PlayKnightMove(_) }
  }
}

case class SOCLargestArmyPlayer(p: Option[Int])
case class SOCNumKnights(m: Map[Int, Int]) extends MapWrapper[Int, Int]
trait LargestArmySOCState[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: LargestArmySOCState[BOARD, PERSPECTIVE, STATE]] extends SOCState[BOARD, Resource, PERSPECTIVE, STATE] {
  this: DevelopmentCardSOCState[BOARD, Resource, PERSPECTIVE, STATE] =>
  def self = this

  def largestArmyPlayer: SOCLargestArmyPlayer
  def numKnights: SOCNumKnights

  def updateLargestArmyPlayer(player: SOCLargestArmyPlayer): STATE
  def updateNumKnights(numKnights: SOCNumKnights): STATE

  def incrementKnightCount(playerId: Int): STATE = updateNumKnights(SOCNumKnights((numKnights - playerId) + (playerId -> numKnights.get(playerId).fold(1)(_ + 1))))

  def onKnightPlay(player: Int)(onKnight: => STATE): STATE = {
    val originalLargestArmyPlayer = largestArmyPlayer.p.flatMap(p => numKnights.get(p).map(p -> _))
    val uk = onKnight.incrementKnightCount(player)
    (uk.numKnights(player), originalLargestArmyPlayer) match {
      case (currPlayerKnights, Some((p, largestArmyCount))) if currPlayerKnights > largestArmyCount =>
        uk.updateLargestArmyPlayer(SOCLargestArmyPlayer(Some(player))).updatePoints(SOCPlayerPointsMap(((playerPoints - player) - p) + (player -> (playerPoints(player) + 2)) + (p -> (playerPoints(p) - 2))))
      case (currPlayerKnights, None) if currPlayerKnights >= 3 =>
        uk.updateLargestArmyPlayer(SOCLargestArmyPlayer(Some(player))).updatePoints(SOCPlayerPointsMap((playerPoints - player) + (player -> (playerPoints(player) + 2))))
      case _ => uk
    }
  }
}
