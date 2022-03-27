package com.qwerfah.sort

import algorithm._
import compare._
import compare.Implicits._

object Main extends App:
  val algorithms = BubbleSort() :: SelectionSort() :: QuickSort() :: Nil
  val comparator = Comparator[Int](algorithms)

  val results = comparator.compare(100, 1000, 100)
  print(results)

