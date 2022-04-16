package com.qwerfah.sort
package algorithm

trait SortAlgorithm:
  def code: String
  def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): (Seq[T], SortStats)


final case class SortStats(permutations: Int = 0, comparisons: Int = 0):
  def +(other: SortStats) = this.copy(
    permutations = this.permutations + other.permutations,
    comparisons = this.comparisons + other.comparisons
  )

  override def toString: String = s"comparisons: $comparisons, permutations: $permutations"
