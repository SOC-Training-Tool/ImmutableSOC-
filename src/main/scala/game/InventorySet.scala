package game

import shapeless.Coproduct

case class InventorySet[A, T] protected(amountMap: Map[A, T])(implicit num: Numeric[T]) {

  lazy val getTotal: Int = num.toInt(amountMap.values.sum)
  lazy val getTypes: Seq[A] = amountMap.keys.toSeq
  lazy val getTypeCount: Int = amountMap.values.count(num.toDouble(_) > 0)
  lazy val isEmpty: Boolean = getTotal == 0

  def copy(amountMap: Map[A, T] = amountMap): InventorySet[A, T] = InventorySet.fromMap(amountMap)

  def add(amt: T, a: A): InventorySet[A, T] = {
    val curAmount = amountMap.get(a)
    copy(curAmount.fold(amountMap + (a -> amt)){ amount =>
      (amountMap - a) + (a -> num.plus(amount, amt))
    })
  }

  def add(set: InventorySet[A, T]): InventorySet[A, T] = {
    set.getTypes.foldLeft(this.copy(amountMap)){ case (newSet, a) => newSet.add(set(a), a) }
  }

  def subtract(amt: T, a: A): InventorySet[A, T] = {
    val curAmount = amountMap.get(a)
    val newMap = curAmount.fold(amountMap) { amount =>
      (amountMap - a) + (a -> num.max(num.zero, num.minus(amount, amt)))
    }
    copy(newMap)
  }

  def subtract(set: InventorySet[A, T]): InventorySet[A, T] = {
    set.getTypes.foldLeft(this.copy(amountMap)){ case (newSet, a) => newSet.subtract(set(a), a) }
  }

  def contains(amt: T, a: A): Boolean = amountMap.get(a).fold(false)(num.gteq(_, amt))
  def contains(a: A): Boolean = amountMap.get(a).fold(false)(num.gt(_, num.zero))
  def contains(set: InventorySet[A, T]): Boolean = {
    set.getTypes.filter(set.contains).forall(res => contains(set.getAmount(res), res))
  }

  def getAmount(a: A): T = amountMap.getOrElse(a, num.zero)

  def apply(a: A): T = getAmount(a)

  def canEqual(a: Any): Boolean = a.isInstanceOf[InventorySet[A, T]]
  override def equals(that: Any): Boolean = that match {
    case e: InventorySet[A, T] => this.canEqual(e) &&
      e.contains(this.asInstanceOf[InventorySet[A, T]]) && this.contains(e)
    case _ => false
  }

}

object InventorySet {

  def empty[A, T: Numeric] = InventorySet(Map.empty[A, T])

  implicit def toList[A](set: InventorySet[A, Int]): Seq[A] = set.amountMap.toList.flatMap { case (a, amt) => (1 to amt).map(_ => a)}
  implicit def fromMap[A, T: Numeric](invMap: Map[A, T]): InventorySet[A, T] = {
    val numeric = implicitly[Numeric[T]]
    InventorySet(invMap.filter{ case(_, t) => numeric.gt(t, numeric.zero) })
  }
  implicit def fromList[A](list: Seq[A]): InventorySet[A, Int] = fromMap(list.groupBy(f => f).view.mapValues(_.length).toMap)
  implicit def sum[A, T: Numeric](sets: Seq[InventorySet[A, T]]): InventorySet[A, T] = sets.foldLeft(empty[A, T]){case (x, y) => x.add(y) }

}

