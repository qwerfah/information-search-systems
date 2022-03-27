package com.qwerfah.sort
package compare

import algorithm.SortAlgorithm

case class Comparator[T](algorithms: Seq[SortAlgorithm])(implicit generator: SeqGenerator[T]):
  def compare(start: Int, end: Int, step: Int, iters: Int = 10)(implicit integral: Integral[T]): Map[Int, Map[String, Double]] =
    val result = Range.Int(start, end, step) map { length =>
      val seq = generator.generate(length)
      val elapsed = algorithms map { algo =>
        val time = (0 until iters).map { (_: Int) =>
          val startTime = System.currentTimeMillis()
          val sorted = algo.sort(seq)
          System.currentTimeMillis() - startTime
        }.toList
        algo.code -> (time.sum.toDouble / time.length.toDouble)
      }
      length -> elapsed.toMap
    }
    result.toMap
