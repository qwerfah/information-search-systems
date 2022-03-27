package com.qwerfah.sort
package algorithm

final case class QuickSort() extends SortAlgorithm:
  override val code: String = "quick_sort"
  
  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): Seq[T] =
    def recur(seq: Seq[T]): Seq[T] =
      if seq.length <= 1 then
        seq
      else
        val pivot = seq.head
        val la = seq.tail.filter(order.compare(_, pivot) <= 0)
        val ra = seq.tail.filter(order.compare(_, pivot) > 0)
        (recur(la) :+ pivot) ++ recur(ra)

    recur(sequence)
