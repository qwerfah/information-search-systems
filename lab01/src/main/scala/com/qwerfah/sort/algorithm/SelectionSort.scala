package com.qwerfah.sort
package algorithm

import scala.collection.mutable.{Seq => MutableSeq}

final case class SelectionSort() extends SortAlgorithm:
  override val code: String = "selection_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): (Seq[T], SortStats) =
    val result = sequence.indices.foldLeft(Seq.empty[T] -> sequence) { case ((res, rest), _) =>
      val (min, indexOfMin) = rest.indices.tail.foldLeft(rest.head -> 0) { case ((min, minInd), ind) =>
        if order.compare(rest(ind), rest(minInd)) < 0 then
          rest(ind) -> ind
        else
          rest(minInd) -> minInd
      }
      (res :+ min) -> rest.updated(indexOfMin, rest.head).tail
    }

    result._1 -> SortStats()

final case class SelectionSortSE() extends SortAlgorithm:
  override val code: String = "selection_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): (Seq[T], SortStats) =
    val sorted = MutableSeq(sequence: _*)
    var permutations = 0
    var comparisons = 0
    var minInd = 0

    for 
      i <- 0 until (sorted.length - 1)
    do
      var minInd = i
      for 
        j <- (i + 1) until sorted.length
      do
        if order.compare(sorted(j), sorted(minInd)) < 0 then
          minInd = j
        comparisons += 1
      
      if minInd != i then
        val swap = sorted(i)
        sorted(i) = sorted(minInd)
        sorted(minInd) = swap
        permutations += 1
        
    sorted.toSeq -> SortStats(permutations, comparisons)
