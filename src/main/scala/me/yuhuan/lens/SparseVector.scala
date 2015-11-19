package me.yuhuan.lens

import scala.collection._

/**
 * A simple sparse vector backed by a hash map.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SparseVector(val data: mutable.Map[Int, Double]) {
  @inline def apply(i: Int) = data.getOrElse(i, 0.0)
  override def toString = data.toArray.sortBy(_._1).map{case (k, v) => s"$k:$v"}.mkString(" ")
}

object SparseVector {
  def apply(kvs: (Int, Double)*) = new SparseVector(mutable.HashMap(kvs: _*))
  //def ofOneSpots(oneSpots: Int*) = new SparseVector(mutable.HashMap(oneSpots.map(i ⇒ i → 1.0): _*))
}
