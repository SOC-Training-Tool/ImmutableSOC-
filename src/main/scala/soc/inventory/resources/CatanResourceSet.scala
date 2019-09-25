package soc.inventory.resources

import soc.inventory._

case class CatanResourceSet[T: Numeric](br: T = 0, or: T = 0, sh: T = 0, wh: T = 0, wo: T = 0) extends CatanSet[Resource, T] {

  override protected val implWrap: NumericWrapper = NumericWrapper()

  override def _copy(map: Map[Resource, T]): CatanResourceSet[T] = CatanResourceSet(
    map.getOrElse(Brick, implWrap.wrapped.zero),
    map.getOrElse(Ore, implWrap.wrapped.zero),
    map.getOrElse(Sheep, implWrap.wrapped.zero),
    map.getOrElse(Wheat, implWrap.wrapped.zero),
    map.getOrElse(Wood, implWrap.wrapped.zero))

  override val amountMap: Map[Resource, T] = Map(Brick -> br, Ore -> or, Sheep -> sh, Wheat -> wh, Wood -> wo)

}

object CatanResourceSet {
  type CatanResourceSet[S] = CatanSet[Resource, S]
  type ResourceSet[S] = CatanResourceSet[S]
  type Resources = ResourceSet[Int]

  def empty[T: Numeric]: CatanResourceSet[T] = {
    val num = implicitly[Numeric[T]]
    CatanResourceSet(num.zero, num.zero, num.zero, num.zero, num.zero)

  }
  val fullBank = CatanResourceSet(19, 19, 19, 19, 19)


  def apply[T](resMap: Map[Resource, T])(implicit num: Numeric[T]): ResourceSet[T] =
    CatanResourceSet[T](
      resMap.getOrElse(Brick, num.zero),
      resMap.getOrElse(Ore, num.zero),
      resMap.getOrElse(Sheep, num.zero),
      resMap.getOrElse(Wheat, num.zero),
      resMap.getOrElse(Wood, num.zero))

  def apply(resources: Resource*): Resources = {
    resources.foldLeft(empty[Int]) { case (set, res) => set.add(CatanResourceSet(Map(res -> 1)))}
  }

  def describe(set: Resources): String = {
    set.amountMap.filter(_._2 > 0).toSeq.map { case (res, amt) => s"$amt ${res.name}s"}.mkString(", ")
  }

//  implicit def encoderInt(implicit n: Numeric[Int]): Encoder[ResourceSet[Int]] = Encoder.forProduct5("Brick", "Ore", "Sheep", "Wheat", "Wood")(rs => (rs.getAmount(Brick), rs.getAmount(Ore), rs.getAmount(Sheep), rs.getAmount(Wheat), rs.getAmount(Wood)))
//  implicit def encoderDouble(implicit n: Numeric[Double]): Encoder[ResourceSet[Double]] = Encoder.forProduct5("Brick", "Ore", "Sheep", "Wheat", "Wood")(rs => (rs.getAmount(Brick), rs.getAmount(Ore), rs.getAmount(Sheep), rs.getAmount(Wheat), rs.getAmount(Wood)))
}

