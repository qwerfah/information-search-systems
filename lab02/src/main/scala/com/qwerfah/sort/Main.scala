package com.qwerfah.sort

import com.qwerfah.sort.file.algorithms._
import com.qwerfah.sort.file.devices.LocalDevice

import scala.reflect.ClassTag

object Conversions:
  given Conversion[String, Int] = Integer.parseInt(_)

object ClassTags:
  given IntClassTag: ClassTag[Int] = ClassTag(classOf[Int])

object Main extends App {
  import ClassTags.given
  import Conversions.given

  val balancedMerging = new BalancedMerging(5, 5)
  println(balancedMerging.sort("input", "output1", " " :: "\n" :: Nil))

  val devices =
    LocalDevice("dev1", 3) :: LocalDevice("dev2", 3) :: LocalDevice("dev3", 3) :: LocalDevice("dev4", 3) :: Nil

  val oscillatedSort = OscillatedSort(devices, 3)
  println(oscillatedSort.sort("input", "output2", " " :: "\n" :: Nil))
}
