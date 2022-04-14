package com.qwerfah.sort
package compare

import algorithm.SortAlgorithm

case class Comparator[T](algorithms: Seq[SortAlgorithm])(implicit generator: SeqGenerator[T]):
  def compare(start: Int, end: Int, step: Int, iters: Int = 10)(implicit integral: Integral[T]): (Seq[Int], Map[String, Seq[Double]]) =
    val lengths = Range.Int(start, end, step)
    val seqs = lengths.map { len => generator.generate(len) }
    
    val measures = algorithms.map { algo =>
      val elapsed = seqs.map { seq =>
        val time = (0 until iters).map { (_: Int) =>
          val startTime = System.currentTimeMillis()
          val sorted = algo.sort(seq)
          System.currentTimeMillis() - startTime
        }.toList
        time.sum.toDouble / time.length.toDouble
      }
      algo.code -> elapsed
    }.toMap
    
    lengths -> measures
