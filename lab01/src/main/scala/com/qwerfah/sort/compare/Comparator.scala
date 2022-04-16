package com.qwerfah.sort
package compare

import algorithm.SortAlgorithm

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.given
import scala.concurrent.duration.Duration

case class Comparator[T](algorithms: Seq[SortAlgorithm], metric: Metric[T, _])(implicit
    generator: SeqGenerator[T],
    integral: Integral[T]
):

  def compare(start: Int, end: Int, step: Int, iters: Int = 10): (Seq[Int], Map[String, Seq[_]]) =
    _compare(Range.Int(start, end, step), iters)

  def compareAsync(
      start: Int,
      end: Int,
      step: Int,
      iters: Int = 10,
      threads: Int = 8
  ): Future[(Seq[Int], Map[String, Seq[_]])] =

    val lengths = Range.Int(start, end, step)
    val stepsPerThread = (lengths.length.toDouble / threads.toDouble).ceil.toInt
    val groupedLengths = lengths.grouped(stepsPerThread).toList
    val futures = for
      lns <- groupedLengths
    yield
     Future { _compare(lns, iters) }

    val future = Future.sequence(futures).map { results =>
      val sorted = results.sortBy(_._1.last)
      val range = sorted.flatMap(_._1)
      val map = sorted.flatMap(_._2.toSeq).groupBy(_._1).map { case (key, value) =>
        key -> value.flatMap(_._2)
      }
      range -> map
    }

    future

  private def _compare(lengths: Seq[Int], iters: Int = 10): (Seq[Int], Map[String, Seq[_]]) =
    val seqs = lengths.map { len => generator.generate(len) }

    val measures = algorithms.map { algo =>
      val elapsed = seqs.map { seq =>
        metric.calc(seq, algo, iters)
      }
      algo.code -> elapsed
    }.toMap

    lengths -> measures


