package com.qwerfah.sort.file.ops

object BlockOps:
  /** Выполняет слияние переданного набора блоков в один. Блоки могут иметь разные длины, но предполагается, что каждый
    * из них отсортирован в соответствии с переданным экземпялром Ordering.
    */
  def merge[T](blocks: Seq[Seq[T]])(using order: Ordering[T]): Seq[T] =
    val (newBlocks, sorted) =
      blocks.flatMap(_.indices).foldLeft(blocks -> List.empty[T]) { case ((blocks, sorted), _) =>
        val (min, minInd) =
          blocks.zipWithIndex.tail.foldLeft(blocks.head.head -> 0) { case ((min, minInd), (block, blockInd)) =>
            if order.compare(block.head, min) < 0 then block.head -> blockInd
            else min -> minInd
          }

        val newBlocks = if blocks(minInd).length > 1 then Seq(blocks(minInd).tail) else Seq.empty
        blocks.patch(minInd, newBlocks, 1) -> (sorted :+ min)
      }

    assert(newBlocks.isEmpty)
    sorted
