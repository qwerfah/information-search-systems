package com.qwerfah.sort

import algorithm._

object Main extends App:
  val sort = SelectionSort()
  import scala.math.Integral.Implicits._
  print(sort.sort(List(5, 4, 3, 2, 1)))

