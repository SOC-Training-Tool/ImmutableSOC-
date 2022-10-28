package util

trait MapWrapper[K, V] {
  def m: Map[K, V]

  def contains(k: K) = m.contains(k)
  def get(k: K) = m.get(k)
  def getOrElse(k: K, v: V) = m.getOrElse(k, v)
  def apply(k: K) = m(k)
  def -(k: K) = m - k
  def +(kv: (K, V)) = m + kv
  def values = m.values

  def ++[K1 >: K, V1 >: V](m1: MapWrapper[K1, V1]) = (m ++ m1.m).toMap

  def map[K1, V1] = m.map[K1, V1] _
  def map[B] = m.map[B] _

  def flatMap[K1, V1] = m.flatMap[K1, V1] _
  def flatMap[B] = m.flatMap[B] _
}
