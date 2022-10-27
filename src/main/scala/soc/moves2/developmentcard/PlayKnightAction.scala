package soc.moves2.developmentcard

import shapeless.{::, HList}
import soc.board.BoardConfiguration
import soc.inventory._
import soc.moves2.MoveResultProvider.MoveResultProviderTransformer
import soc.state.SOCState.SOCState
import soc.moves2._
import soc.state.SOCPlayerPointsMap
import soc.state.build.BoardOps
import util.{DependsOn, MapWrapper}

case class PlayKnightMove(robberMove: RobberMove) extends PlayDevelopmentCardMove {
  override def player: Int = robberMove.player
  override def card: DevelopmentCard = Knight
}
case class PlayKnightMoveResult[II <: InventoryItem](robberMoveResult: RobberMoveResult[II]) extends PlayDevelopmentCardMoveResult[PlayKnightMove] {
  def player = robberMoveResult.player
  override def move: PlayKnightMove = PlayKnightMove(robberMoveResult.move)
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[PlayKnightMove]] = {
    robberMoveResult.getPerspectiveResults(playerIds).map {
      case (i, result: RobberMoveResult[II]) => i -> PlayKnightMoveResult(result)
    }
  }
}
case class PlayKnightAction[BOARD <: BoardConfiguration, STATE[P] <: SOCState[BOARD, Resource, P, STATE[P]]](cardsInDeck: Int, robberAction: RobberAction[BOARD, Resource, STATE]) extends PlayDevelopmentCardAction[BOARD, Resource, STATE, PlayKnightMove, PlayKnightAction[BOARD, STATE]] {

  override val cardType: DevelopmentCard = Knight
  override type R = PlayKnightMoveResult[Resource]

  override val moveResultProvider: MoveResultProvider[BOARD, Resource, STATE, PlayKnightMove, PlayKnightMoveResult[Resource]] = robberAction.moveResultProvider.transform[PlayKnightMove, PlayKnightMoveResult[Resource]](_.robberMove, PlayKnightMoveResult.apply[Resource](_))
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PlayKnightMove] = {
    robberAction.getAllMoves(state, inv, position).map {PlayKnightMove(_) }
  }
}

case class SOCLargestArmyPlayer(p: Option[Int])
case class SOCNumKnights(m: Map[Int, Int]) extends MapWrapper[Int, Int]

object LargestArmySOCState {
//  implicit def largestArmyPlayerFG[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: LargestArmySOCState[BOARD, PERSPECTIVE, STATE]]: SOCStateFieldGenerator[BOARD, Resource, PERSPECTIVE, STATE, SOCLargestArmyPlayer] = {case(_,_) => SOCLargestArmyPlayer(None)}
//  implicit def knightCountFG[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: LargestArmySOCState[BOARD, PERSPECTIVE, STATE]]: SOCStateFieldGenerator[BOARD, Resource, PERSPECTIVE, STATE, SOCNumKnights] = {case(_,p) => SOCNumKnights(p.map(i => i -> 0).toMap)}

  import soc.state.SOCState.SOCStateOps

  implicit class LargestArmySOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[B, I, P] <: HList](state: STATE[BOARD, II, PERSPECTIVE])(implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCLargestArmyPlayer :: SOCNumKnights :: SOCState[BOARD, II, PERSPECTIVE]]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val largestArmyPlayer: SOCLargestArmyPlayer = dep.get(state)
    def updateLargestArmyPlayer(player: SOCLargestArmyPlayer): STATE[BOARD, II, PERSPECTIVE] = dep.update(player, state)

    val numKnights: SOCNumKnights = dep.get(state)
    def updateNumKnights(numKnights: SOCNumKnights): STATE[BOARD, II, PERSPECTIVE] = dep.update(numKnights, state)

    def incrementKnightCount(playerId: Int): STATE[BOARD, II, PERSPECTIVE] =
      updateNumKnights(SOCNumKnights((numKnights - playerId) + (playerId -> numKnights.get(playerId).fold(1)(_ + 1))))

    def onKnightPlay(player: Int)(onKnight: => STATE[BOARD, II, PERSPECTIVE]): STATE[BOARD, II, PERSPECTIVE] = {
      val playerPoints = state.playerPoints
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

  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], STATE[_, _, _] <: HList](implicit dep: DependsOn[STATE[BOARD ,II, PERSPECTIVE], SOCNumKnights :: SOCLargestArmyPlayer :: SOCDevelopmentCardsInDeck :: SOCState[BOARD, II, PERSPECTIVE]], applyRobberMoveResult: ApplyMoveResult[RobberMoveResult[II], STATE[BOARD, II, PERSPECTIVE]]): ApplyMoveResult[PlayKnightMoveResult[II], STATE[BOARD, II, PERSPECTIVE]] = {
    case (s, m) =>
      import DevelopmentCardSOCState._
      implicit val playDevCardDep = dep.innerDependency[SOCDevelopmentCardsInDeck :: SOCState[BOARD, II, PERSPECTIVE]]
      implicit val playKnightDep = dep.innerDependency[SOCLargestArmyPlayer :: SOCNumKnights :: SOCState[BOARD, II, PERSPECTIVE]]
      s.onKnightPlay(m.player)(applyRobberMoveResult(s.playDevelopmentCard(m), m.robberMoveResult))
  }
}
