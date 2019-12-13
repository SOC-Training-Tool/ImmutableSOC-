package soc.inventory.resources

import soc.inventory._

object ResourceSet {
  type ResourceSet[T] = CatanSet[Resource, T]
  type Resources = ResourceSet[Int]

  def apply[T: Numeric](br: T = 0, or: T = 0, sh: T = 0, wh: T = 0, wo: T = 0): ResourceSet[T] = ResourceSet[T](Map[Resource, T](Brick -> br, Wood -> wo, Ore -> or, Sheep -> sh, Wheat -> wh))
  def apply[T: Numeric](resMap: Map[Resource, T]): ResourceSet[T] = CatanSet.fromMap(resMap)
  def apply(resources: Resource*): Resources = CatanSet.fromList(resources.toSeq)

  val fullBank = ResourceSet[Int](19, 19, 19, 19, 19)
  def empty[T: Numeric]: ResourceSet[T] = CatanSet.empty[Resource, T]
}

