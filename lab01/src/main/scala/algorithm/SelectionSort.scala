package com.qwerfah.sort
package algorithm

final case class SelectionSort() extends SortAlgorithm:
  override val code: String = "selection_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): Seq[T] =
    val result = sequence.indices.foldLeft(Seq.empty[T] -> sequence) { case ((res, rest), _) =>
      val (min, indexOfMin) = rest.indices.tail.foldLeft(rest.head -> 0) { case ((min, minInd), ind) =>
        if order.compare(rest(ind), rest(minInd)) < 0 then
          rest(ind) -> ind
        else
          rest(minInd) -> minInd
      }
      (res :+ min) -> rest.updated(indexOfMin, rest.head).tail
    }

    result._1
