package com.qwerfah.sort
package compare

import algorithm.SortAlgorithm

/** Метрика, которая будет оцениваться компаратором. */
trait Metric[T, R]:
  def code: String
  def calc(seq: Seq[T], algo: SortAlgorithm, iters: Int): R

/** Метрика - времмя работы алгоритма, оценивается как среднее из указанного числа прогонов. */
final case class TimeMetric[T]()(implicit integral: Integral[T]) extends Metric[T, Double]:
  override val code: String = "Время, мс"
  
  override def calc(seq: Seq[T], algo: SortAlgorithm, iters: Int): Double =
    val time = (0 until iters).map { (_: Int) =>
      val startTime = System.currentTimeMillis()
      algo.sort(seq)
      System.currentTimeMillis() - startTime
    }.toList
    time.sum.toDouble / time.length.toDouble

/** Метрика - число операций сравнения за прогон. */
final case class CompareMetric[T]()(implicit integral: Integral[T]) extends Metric[T, Double]:
  override val code: String = "Кол-во сравнений"

  override def calc(seq: Seq[T], algo: SortAlgorithm, iters: Int): Double =
    val comparisons = (0 until iters).map { (_: Int) =>
      val (_, stats) = algo.sort(seq)
      stats.comparisons
    }.toList
    comparisons.sum.toDouble / comparisons.length.toDouble

/** Метрика - число перестановок элементов коллекции за прогон. Может быть оценена только для методов с side-эффектами,
  * так как традиционные функциональные реализации не подразумевают перестановок внутри коллекций, исходя из
  * предположения об их иммутабельности.
  */
final class PermuteMetric[T](implicit integral: Integral[T]) extends Metric[T, Double]:
  override val code: String = "Кол-во перестановок"

  override def calc(seq: Seq[T], algo: SortAlgorithm, iters: Int): Double =
    val permutations = (0 until iters).map { (_: Int) =>
      val (_, stats) = algo.sort(seq)
      stats.permutations
    }.toList
    permutations.sum.toDouble / permutations.length.toDouble
