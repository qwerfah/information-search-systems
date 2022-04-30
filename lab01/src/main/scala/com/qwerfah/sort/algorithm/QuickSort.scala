package com.qwerfah.sort
package algorithm

import scala.collection.mutable.{Seq => MutableSeq}
import scala.util.Random

final case class QuickSort() extends SortAlgorithm:
  override val code: String = "quick_sort"

  override def sort[T](sequence: Seq[T])(using order: Ordering[T]): (Seq[T], SortStats) =
    def recur(seq: Seq[T]): Seq[T] =
      if seq.length <= 1 then seq
      else
        val pivot = seq.head
        val la = seq.tail.filter(order.compare(_, pivot) <= 0)
        val ra = seq.tail.filter(order.compare(_, pivot) > 0)
        (recur(la) :+ pivot) ++ recur(ra)

    recur(sequence) -> SortStats()

final case class QuickSortSE() extends SortAlgorithm:
  override val code: String = "quick_sort"

  override def sort[T](sequence: Seq[T])(using order: Ordering[T]): (Seq[T], SortStats) =
    /** Вся суть функции заключена в производимом ей side-эффекте путем сортировки прямо в переданной ей коллекции, а
      * потому является СМЕРТНЫМ ГРЕХОМ и не рекомендуется к употреблению.
      */
    def recurSE(seq: MutableSeq[T], first: Int, last: Int): SortStats =
      if first >= last then SortStats()
      else
        var (i, j) = (first, last)
        val pivot = seq((first + last) / 2) // seq(first + Random.nextInt(last - first + 1))

        var permutations = 0
        var comparisons = 0

        while i <= j do
          while order.compare(seq(i), pivot) < 0 do i += 1; comparisons += 1
          while order.compare(seq(j), pivot) > 0 do j -= 1; comparisons += 1
          comparisons += 2
          if i <= j then
            val swap = seq(i)
            seq(i) = seq(j)
            seq(j) = swap

            permutations += 1
            i += 1
            j -= 1

        SortStats(permutations, comparisons) + recurSE(seq, first, j) + recurSE(seq, i, last)

    val sorted = MutableSeq(sequence: _*)
    val stats = recurSE(sorted, 0, sorted.length - 1)
    sorted.toSeq -> stats
