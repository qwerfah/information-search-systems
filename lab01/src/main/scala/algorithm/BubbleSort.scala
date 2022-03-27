package com.qwerfah.sort
package algorithm

final case class BubbleSort() extends SortAlgorithm:
  override val code: String = "bubble_sort"

  override def sort[T](sequence: Seq[T])(implicit order: Ordering[T]): Seq[T] =

    def iter(seq: Seq[T]): (Seq[T], Boolean) =
      val res = seq.tail.foldLeft((Seq.empty[T], seq.head, false)) { case ((res, prev, flag), curr) =>
        if order.compare(prev, curr) > 0 then
          (res :+ curr, prev, true)
        else
          (res :+ prev, curr, flag)
      }
      (res._1 :+ res._2) -> res._3

    sequence.indices.foldLeft(sequence -> true) { case ((result, flag), _) =>
      if flag then iter(result) else result -> flag
    }._1
