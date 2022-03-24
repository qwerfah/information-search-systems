package com.qwerfah.sort
package compare

import algorithm.SortAlgorithm

class Comparator[T](algorithms: Seq[SortAlgorithm], generator: SeqGenerator[T]):
  def compare(start: Int, end: Int, step: Int): Map[Int, Map[String, Long]] =
    val result = Range.Int(start, end, step) map { length =>
      val seq = generator.generate(length)
      val elapsed = algorithms map { algo =>
        val startTime = System.currentTimeMillis()
        val sorted = algo.sort(seq)
        algo.code -> (System.currentTimeMillis() - startTime)
      }
      length -> elapsed.toMap
    }
    result.toMap
