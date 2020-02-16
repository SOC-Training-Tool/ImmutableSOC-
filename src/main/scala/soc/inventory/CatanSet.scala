package soc.inventory

case class CatanSet[A <: InventoryItem, T] protected(amountMap: Map[A, T])(implicit num: Numeric[T]) {

  val getTotal = num.toInt(amountMap.values.sum)
  val getTypes: Seq[A] = amountMap.keys.toSeq
  val getTypeCount = amountMap.values.count(num.toDouble(_) > 0)
  val isEmpty: Boolean = getTotal == 0

  def copy(amountMap: Map[A, T] = amountMap): CatanSet[A, T] = CatanSet.fromMap(amountMap)

  def add(amt: T, a: A): CatanSet[A, T] = {
    val curAmount = amountMap.get(a)
    copy(curAmount.fold(amountMap + (a -> amt)){ amount =>
      (amountMap - a) + (a -> num.plus(amount, amt))
    })
  }

  def add(set: CatanSet[A, T]): CatanSet[A, T] = {
    set.getTypes.foldLeft(this.copy(amountMap)){ case (newSet, a) => newSet.add(set(a), a) }
  }

  def subtract(amt: T, a: A): CatanSet[A, T] = {
    val curAmount = amountMap.get(a)
    val newMap = curAmount.fold(amountMap) { amount =>
      (amountMap - a) + (a -> num.max(num.zero, num.minus(amount, amt)))
    }
    copy(newMap)
  }

  def subtract(set: CatanSet[A, T]): CatanSet[A, T] = {
    set.getTypes.foldLeft(this.copy(amountMap)){ case (newSet, a) => newSet.subtract(set(a), a) }
  }

  def contains(amt: T, a: A): Boolean = amountMap.get(a).fold(false)(num.gteq(_, amt))
  def contains(a: A): Boolean = amountMap.get(a).fold(false)(num.gt(_, num.zero))
  def contains(set: CatanSet[A, T]): Boolean = {
    set.getTypes.filter(set.contains).forall(res => contains(set.getAmount(res), res))
  }

  def getAmount(a: A): T = amountMap.getOrElse(a, num.zero)

  def apply(a: A): T = getAmount(a)

  def canEqual(a: Any): Boolean = a.isInstanceOf[CatanSet[A, T]]
  override def equals(that: Any): Boolean = that match {
    case e: CatanSet[A, T] => this.canEqual(e) &&
      e.contains(this.asInstanceOf[CatanSet[A, T]]) && this.contains(e)
    case _ => false
  }

}

object CatanSet {

  def empty[A <: InventoryItem, T: Numeric] = CatanSet(Map.empty[A, T])

  implicit def toList[A <: InventoryItem](set: CatanSet[A, Int]): Seq[A] = set.amountMap.toList.flatMap { case (a, amt) => (1 to amt).map(_ => a)}
  implicit def fromMap[A <: InventoryItem, T: Numeric](invMap: Map[A, T]): CatanSet[A, T] = {
    val numeric = implicitly[Numeric[T]]
    CatanSet(invMap.filter{ case(_, t) => numeric.gt(t, numeric.zero) }.toMap)
  }
  implicit def fromList[A <: InventoryItem](list: Seq[A]): CatanSet[A, Int] = fromMap(list.groupBy(f => f).view.mapValues(_.length).toMap)
  implicit def sum[A <: InventoryItem, T: Numeric](sets: Seq[CatanSet[A, T]]): CatanSet[A, T] = sets.foldLeft(empty[A, T]){case (x, y) => x.add(y) }

}

