package soc.inventory.resources

import ResourceSet.{ResourceSet, Resources}
import soc.inventory._

class ProbableResourceSet(val known: ResourceSet[Int], val unknown: ResourceSet[Double]) extends CatanSet[Resource, Double](
  Map[Resource, Double](
    Brick -> (known.getAmount(Brick) + unknown.getAmount(Brick)),
    Ore -> (known.getAmount(Ore) + unknown.getAmount(Ore)),
    Sheep -> (known.getAmount(Sheep) + unknown.getAmount(Sheep)),
    Wheat -> (known.getAmount(Wheat) + unknown.getAmount(Wheat)),
    Wood -> (known.getAmount(Wood) + unknown.getAmount(Wood))
  )) {

  /**
    * How many resources of this type are contained in the set?
    *
    * @param resourceType the type of resource, like { @link SOCResourceConstants#CLAY}
    * @return the number of a kind of resource
    * @see #contains(int)
    * @see #getTotal()
    */
  def getKnownAmount(resourceType: Resource): Int = known.getAmount(resourceType)

  def getUnknownAmount(resourceType: Resource): Double = unknown.getAmount(resourceType)

  /**
    * Does the set contain any resources of this type?
    *
    * @param resourceType the type of resource, like { @link SOCResourceConstants#CLAY}
    * @return true if the set's amount of this resource &gt; 0
    * @see #getAmount(int)
    * @see #contains(ResourceSet)
    */
  override def contains(resourceType: Resource): Boolean = getKnownAmount(resourceType) > 0

  def mightContain(resourceType: Resource): Boolean = unknown.getAmount(resourceType) > 0

  def probabilityContains(resourceType: Resource): Double = {
    if (contains(resourceType)) 1.0
    else getUnknownAmount(resourceType) / getTotal
  }

  def getProbabilityOfResourceInHand(resourceType: Resource): Double = getAmount(resourceType) / getTotal

  /**
    * Get the number of known resource types contained in this set:
    * {@link SOCResourceConstants#CLAY} to {@link SOCResourceConstants#WOOD},
    * excluding {@link SOCResourceConstants#UNKNOWN} or {@link SOCResourceConstants#GOLD_LOCAL}.
    * An empty set returns 0, a set containing only wheat returns 1,
    * that same set after adding wood and sheep returns 3, etc.
    *
    * @return The number of resource types in this set with nonzero resource counts.
    */
  val getResourceTypeCount: Int = known.getTypeCount

  val getResourceTypeMightCount: Int = Resource.list.filter(mightContain).length

  /**
    * Get the total number of resources in this set
    *
    * @return the total number of resources
    * @see #getAmount(int)
    */
  val getKnownTotal: Int = known.getTotal

  val getUnknownTotal: Int =  unknown.getTotal

  lazy override val getTotal: Int = getKnownTotal + getUnknownTotal

  /**
    * @return true if this contains at least the resources in other
    * @param other the sub set, can be { @code null} for an empty resource subset
    * @see #contains(int)
    */
  def contains(other: Resources): Boolean = known.contains(other)

  def mightContain(other: Resources): Boolean =  Resource.list.forall { res => getAmount(res).ceil >= other.getAmount(res) }

  override lazy val toString: String =  Resource.list.filter(getAmount(_) > 0).map { res: Resource =>
    s"${res.name}= ${known.getAmount(res)}:${unknown.getAmount(res)}"
  }.mkString(", ")

  lazy val knownWithProbabilityUnknown: String =  Resource.list.filter(getAmount(_) > 0).map { res: Resource =>
    s"${res.name}= ${getAmount(res)}"
  }.mkString(", ")

//  def cpy(known: ResourceSet[Int] = known, unknown: ResourceSet[Double] = unknown): ProbableResourceSet = {
//    ProbableResourceSet(known.copy(), unknown.copy())
//  }
}

object ProbableResourceSet {

  def apply(known: ResourceSet[Int], unknown: ResourceSet[Double]): ProbableResourceSet = {
    new ProbableResourceSet(known, unknown)
  }

  val empty = ProbableResourceSet(ResourceSet.empty[Int], ResourceSet.empty[Double])
}


