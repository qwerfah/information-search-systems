package com.qwerfah.sort

import algorithm._
import compare._
import compare.Implicits._

import breeze.linalg._
import breeze.plot._

object Main extends App:
  val algorithms = BubbleSort() :: SelectionSort() :: QuickSort() :: Nil
  val comparator = Comparator[Int](algorithms)

  val results = comparator.compare(10, 200, 10, 100)

  print(results)
  plotResults(results)

  def plotResults(results: (Seq[Int], Map[String, Seq[Double]])): Unit =
    val f = Figure()
    val p = f.subplot(0)
    val x = results._1.map(_.toDouble)
    results._2 foreach { case (algo, y) =>
      p += plot(x, y, name = algo)
    }

    p.xlabel = "Кол-во элементов"
    p.ylabel = "Время, мс"
    p.legend = true
    f.saveas("lines.png")

