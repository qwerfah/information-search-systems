package com.qwerfah.sort
package algorithm

final case class QuickSort() extends SortAlgorithm:
  override val code: String = "quick_sort"
  
  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): Seq[T] = ???
