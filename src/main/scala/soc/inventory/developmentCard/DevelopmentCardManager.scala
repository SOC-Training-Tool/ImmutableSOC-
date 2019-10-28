package soc.inventory.developmentCard

import soc.inventory._

import scala.util.Random

case class DevelopmentCardSet[T: Numeric](kn: T = 0, pt: T = 0, rb: T = 0, mp: T = 0, yp: T = 0) extends CatanSet[DevelopmentCard, T, DevelopmentCardSet[T]]{

  override protected val implWrap: NumericWrapper = NumericWrapper()

  override protected def _copy(map: Map[DevelopmentCard, T]): DevelopmentCardSet[T] = DevelopmentCardSet(
    map.get(Knight).getOrElse(implWrap.wrapped.zero),
    map.get(CatanPoint).getOrElse(implWrap.wrapped.zero),
    map.get(RoadBuilder).getOrElse(implWrap.wrapped.zero),
    map.get(Monopoly).getOrElse(implWrap.wrapped.zero),
    map.get(YearOfPlenty).getOrElse(implWrap.wrapped.zero)
  )

  override val amountMap: Map[DevelopmentCard, T] = Map(Knight -> kn, CatanPoint -> pt, RoadBuilder -> rb, Monopoly -> mp, YearOfPlenty -> yp)
}

object DevelopmentCardSet {

  type PlayedInventory = DevelopmentCardSet[Int]
  type UnplayedInventory = DevelopmentCardSet[Double]

  implicit val emptyBuilderInt: Empty[DevelopmentCard, Int, DevelopmentCardSet[Int]] = () => empty[Int]
  implicit val emptyBuilderDouble: Empty[DevelopmentCard, Double, DevelopmentCardSet[Double]] = () => empty[Double]

  def empty[T: Numeric]: DevelopmentCardSet[T] = {
    val num = implicitly[Numeric[T]]
    DevelopmentCardSet(num.zero, num.zero, num.zero, num.zero, num.zero)
  }

  implicit def toInventory[T](map: Map[DevelopmentCard, T])(implicit num: Numeric[T]): DevelopmentCardSet[T] = {
    DevelopmentCardSet[T](
      map.get(Knight).getOrElse(num.zero),
      map.get(CatanPoint).getOrElse(num.zero),
      map.get(RoadBuilder).getOrElse(num.zero),
      map.get(Monopoly).getOrElse(num.zero),
      map.get(YearOfPlenty).getOrElse(num.zero)
    )
  }

  implicit def toDevelopmentCardSet(specifications: DevelopmentCardSpecificationSet): DevelopmentCardSet[Int] = CatanSet.fromList(specifications.toList)
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
    !playedCardOnTurn(turn) &&
      filterUnPlayed.contains(card) &&
      filterUnPlayed.cards.exists{c => c.`type` == card && c.turnPurchased != turn}
  }

  lazy val filterUnPlayed = copy(cards.filter(_.turnPlayed.isEmpty))
  lazy val filterPlayed = copy(cards.filter(_.turnPlayed.isDefined))
  lazy val toList: List[DevelopmentCard] = cards.map(_.`type`)
}


sealed trait DevCardTransaction

case class BuyDevelopmentCard(playerId: Int, card: Option[DevelopmentCard]) extends DevCardTransaction
case class PlayDevelopmentCard(playerId: Int, card: DevelopmentCard) extends DevCardTransaction