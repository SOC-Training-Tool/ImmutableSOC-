package soc.inventory.developmentCard

import soc.inventory._
import scala.util.Random

object DevelopmentCardSet {

  type DevelopmentCardSet[T] = CatanSet[DevelopmentCard, T]

  def apply[T: Numeric](kn: T = 0, pt: T = 0, rb: T = 0, mp: T = 0, yp: T = 0): DevelopmentCardSet[T] = DevelopmentCardSet[T](Map[DevelopmentCard, T](Knight -> kn, CatanPoint -> pt, RoadBuilder -> rb, Monopoly -> mp, YearOfPlenty -> yp))
  def apply[T: Numeric](devMap: Map[DevelopmentCard, T]): DevelopmentCardSet[T] = CatanSet.fromMap(devMap)
  def apply(cards: DevelopmentCard*): DevelopmentCardSet[Int] = CatanSet.fromList(cards.toSeq)

  val fullBank = DevelopmentCardSet[Int](14, 5, 2, 2, 2)
  def empty[T: Numeric]: DevelopmentCardSet[T] = CatanSet.empty[DevelopmentCard, T]

  implicit def toDevelopmentCardSet(specifications: DevelopmentCardSpecificationSet): DevelopmentCardSet[Int] = DevelopmentCardSet(specifications.toList:_*)
}

class DevelopmentCardManager(kn: Int, po: Int, mp: Int, rb: Int, yp: Int)(implicit random: Random) {
  def buildDevelopmentCardDeck: List[DevelopmentCard] = random.shuffle {
    (1 to kn).map(_ => Knight).toList :::
      (1 to po).map(_ => CatanPoint).toList :::
      (1 to mp).map(_ => Monopoly).toList :::
      (1 to rb).map(_ => RoadBuilder).toList :::
      (1 to yp).map(_ => YearOfPlenty).toList
  }
}

case class DevelopmentCardSpecification(
  `type`: DevelopmentCard,
  turnPurchased: Option[Int] = None,
  turnPlayed: Option[Int] = None
)

case class DevelopmentCardSpecificationSet(cards: List[DevelopmentCardSpecification] = Nil) {

  import DevelopmentCardSet._

  lazy val length = cards.length
  lazy val isEmpty = cards.length == 0

  def buyCard(turn: Int, card: DevelopmentCard): DevelopmentCardSpecificationSet = copy(DevelopmentCardSpecification(card, Some(turn), None) :: cards)
  def playCard(turn: Int, card: DevelopmentCard): DevelopmentCardSpecificationSet = {
    def f(c: DevelopmentCardSpecification) = c.turnPlayed.isEmpty && c.`type` == card
    val s = cards.filter(f).sortBy(_.turnPurchased)
    copy(s.headOption.fold(List(DevelopmentCardSpecification(card, None, Some(turn))))(_.copy(turnPlayed = Some(turn)) :: s.tail) ::: cards.filterNot(f))
  }

  def playedCardOnTurn(turn: Int) = cards.find(_.turnPlayed.fold(false)(_ == turn)).isDefined
  def canPlayCardOnTurn(card: DevelopmentCard, turn: Int): Boolean = {
    filterUnPlayed.contains(card) &&
      (card == CatanPoint ||
        (!playedCardOnTurn(turn) &&
          filterUnPlayed.cards.exists { c => c.`type` == card && c.turnPurchased.fold(false)(_ != turn) }))
  }

  lazy val filterUnPlayed = copy(cards.filter(_.turnPlayed.isEmpty))
  lazy val filterPlayed = copy(cards.filter(_.turnPlayed.isDefined))
  lazy val toList: List[DevelopmentCard] = cards.map(_.`type`)
}


sealed trait DevCardTransaction

case class BuyDevelopmentCard(playerId: Int, card: Option[DevelopmentCard]) extends DevCardTransaction
case class PlayDevelopmentCard(playerId: Int, card: DevelopmentCard) extends DevCardTransaction