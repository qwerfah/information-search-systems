package com.qwerfah.sort
package algorithm

import scala.collection.mutable.{Seq => MutableSeq}
import breeze.macros.expand.sequence
import spire.std.seq

final case class BubbleSort() extends SortAlgorithm:
  override val code: String = "bubble_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): (Seq[T], SortStats) =

    def iter(seq: Seq[T]): (Seq[T], Boolean) =
      val res = seq.tail.foldLeft((Seq.empty[T], seq.head, false)) { case ((res, prev, flag), curr) =>
        if order.compare(prev, curr) > 0 then (res :+ curr, prev, true)
        else (res :+ prev, curr, flag)
      }
      (res._1 :+ res._2) -> res._3

    sequence.indices
      .foldLeft(sequence -> true) { case ((result, flag), _) =>
        if flag then iter(result) else result -> flag
      }
      ._1 -> SortStats()

final case class BubbleSortSE() extends SortAlgorithm:
  override val code: String = "bubble_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): (Seq[T], SortStats) =
    val seqCopy = MutableSeq(sequence: _*)
    var permutations = 0
    var comparisons = 0

    for
      i <- 0 until sequence.length
      j <- 1 until sequence.length
    do
      if order.compare(seqCopy(j - 1), seqCopy(j)) > 0 then
        val swap = seqCopy(j)
        seqCopy(j) = seqCopy(j - 1)
        seqCopy(j - 1) = swap
        permutations += 1

      comparisons += 1

    seqCopy.toSeq -> SortStats(permutations, comparisons)
