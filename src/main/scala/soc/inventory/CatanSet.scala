package soc.inventory

trait CatanSet[A <: InventoryItem, T, U <: CatanSet[A, T, U]] {

  protected case class NumericWrapper()(implicit val wrapped: Numeric[T])
  protected val implWrap: NumericWrapper
  import implWrap.wrapped

  protected def _copy(map: Map[A, T]): U
  def copy: U = _copy(amountMap)

  val amountMap: Map[A, T]

  lazy val getTotal = wrapped.toInt(amountMap.values.sum)
  lazy val getTypes: Seq[A] = amountMap.keys.toSeq
  lazy val getTypeCount = amountMap.values.count(wrapped.toDouble(_) > 0)
  lazy val isEmpty: Boolean = getTotal == 0

  def add(amt: T, a: A): U = {
    val curAmount = amountMap.get(a)
    _copy(curAmount.fold(amountMap + (a -> amt)){ amount =>
      (amountMap - a) + (a -> wrapped.plus(amount, amt))
    })
  }

  def add(set: U): U = {
    set.getTypes.foldLeft(this._copy(amountMap)){ case (newSet, a) => newSet.add(set(a), a) }
  }

  def subtract(amt: T, a: A): U = {
    val curAmount = amountMap.get(a)
    val newMap = curAmount.fold(amountMap) { amount =>
      (amountMap - a) + (a -> wrapped.max(wrapped.zero, wrapped.minus(amount, amt)))
    }
    _copy(newMap)
  }

  def subtract(set: U): U = {
    set.getTypes.foldLeft(this._copy(amountMap)){ case (newSet, a) => newSet.subtract(set(a), a) }
  }

  def contains(amt: T, a: A): Boolean = amountMap.get(a).fold(false)(wrapped.gteq(_, amt))
  def contains(a: A): Boolean = amountMap.get(a).fold(false)(wrapped.gt(_, wrapped.zero))
  def contains(set: U): Boolean = {
    set.getTypes.filter(set.contains).forall(res => contains(set.getAmount(res), res))
  }

  def getAmount(a: A): T = amountMap.getOrElse(a, wrapped.zero)

  def apply(a: A): T = getAmount(a)

  def canEqual(a: Any): Boolean = a.isInstanceOf[CatanSet[A, T, U]]
  override def equals(that: Any): Boolean = that match {
    case e: U => this.canEqual(e) &&
      e.contains(this.asInstanceOf[U]) && this.contains(e)
    case _ => false
  }
}

object CatanSet {

  def toList[A <: InventoryItem, U <: CatanSet[A, Int, U]](set: U): List[A] = set.amountMap.toList.flatMap { case (a, amt) => (1 to amt).map(_ => a)}

}