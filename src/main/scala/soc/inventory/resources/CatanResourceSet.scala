package soc.inventory.resources

import soc.inventory._

class CatanResourceSet[T: Numeric](br: T = 0, or: T = 0, sh: T = 0, wh: T = 0, wo: T = 0) extends CatanSet[Resource, T, CatanResourceSet[T]]{

  override protected val implWrap: NumericWrapper = NumericWrapper()

  override protected def _copy(map: Map[Resource, T]): CatanResourceSet[T] = CatanResourceSet[T](
    map.getOrElse(Brick, implWrap.wrapped.zero),
    map.getOrElse(Ore, implWrap.wrapped.zero),
    map.getOrElse(Sheep, implWrap.wrapped.zero),
    map.getOrElse(Wheat, implWrap.wrapped.zero),
    map.getOrElse(Wood, implWrap.wrapped.zero))

  override val amountMap: Map[Resource, T] = Map(Brick -> br, Ore -> or, Sheep -> sh, Wheat -> wh, Wood -> wo)


}

object CatanResourceSet {
  type ResourceSet[S] = CatanResourceSet[S]
  type Resources = ResourceSet[Int]

  def empty[T: Numeric]: ResourceSet[T] = {
    val num = implicitly[Numeric[T]]
    CatanResourceSet[T](num.zero, num.zero, num.zero, num.zero, num.zero)
  }
  val fullBank = CatanResourceSet(19, 19, 19, 19, 19)

  def apply[T: Numeric](br: T = 0, or: T = 0, sh: T = 0, wh: T = 0, wo: T = 0) = new CatanResourceSet(br, or, sh, wh, wo)

  def fromMap[T](resMap: Map[Resource, T])(implicit num: Numeric[T]): ResourceSet[T] =
    apply(
      resMap.getOrElse(Brick, num.zero),
      resMap.getOrElse(Ore, num.zero),
      resMap.getOrElse(Sheep, num.zero),
      resMap.getOrElse(Wheat, num.zero),
      resMap.getOrElse(Wood, num.zero))

  def fromList(resources: Resource*): Resources = {
    resources.foldLeft(empty[Int]) { case (set, res) =>
      val resMap = Map(res -> 1)
      val setToAdd = CatanResourceSet.fromMap(resMap)
      set.add(setToAdd)
    }
  }

  def describe(set: Resources): String = {
    set.amountMap.filter(_._2 > 0).toSeq.map { case (res, amt) => s"$amt ${res.name}s"}.mkString(", ")
  }

//  implicit def encoderInt(implicit n: Numeric[Int]): Encoder[ResourceSet[Int]] = Encoder.forProduct5("Brick", "Ore", "Sheep", "Wheat", "Wood")(rs => (rs.getAmount(Brick), rs.getAmount(Ore), rs.getAmount(Sheep), rs.getAmount(Wheat), rs.getAmount(Wood)))
//  implicit def encoderDouble(implicit n: Numeric[Double]): Encoder[ResourceSet[Double]] = Encoder.forProduct5("Brick", "Ore", "Sheep", "Wheat", "Wood")(rs => (rs.getAmount(Brick), rs.getAmount(Ore), rs.getAmount(Sheep), rs.getAmount(Wheat), rs.getAmount(Wood)))
}

