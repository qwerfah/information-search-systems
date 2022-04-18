package com.qwerfah.sort

import com.qwerfah.sort.file.BalancedMerging

import scala.reflect.ClassTag

object Conversions:
  given Conversion[String, Int] = Integer.parseInt(_)

object ClassTags:
  given IntClassTag: ClassTag[Int] = ClassTag(classOf[Int])

object Main extends App {
  import ClassTags.given
  import Conversions.given

  val sort = new BalancedMerging(5, 5)
  print(sort.sort("input", "output", " "))
}