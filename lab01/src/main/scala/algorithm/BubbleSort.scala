package com.qwerfah.sort
package algorithm

final case class BubbleSort() extends SortAlgorithm:
  override val code: String = "bubble_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): Seq[T] =

    def iter(seq: Seq[T]): Seq[T] =
      val res = seq.tail.foldLeft(Seq.empty[T] -> seq.head) { case ((res, prev), curr) =>
        if order.compare(prev, curr) > 0 then
          (res :+ curr) -> prev
        else
          (res :+ prev) -> curr
      }
      res._1 :+ res._2

    sequence.indices.foldLeft(sequence) { (result, _) =>
      iter(result)
    }
