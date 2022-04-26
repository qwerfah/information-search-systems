package com.qwerfah.sort

import com.qwerfah.sort.file.algorithms.*
import com.qwerfah.sort.file.devices.LocalDevice
import com.qwerfah.sort.file.ops.FileOps

import scala.reflect.ClassTag

object Conversions:
  given Conversion[String, Int] = Integer.parseInt(_)

object ClassTags:
  given IntClassTag: ClassTag[Int] = ClassTag(classOf[Int])

object Main extends App:
  import ClassTags.given
  import Conversions.given

//  val balancedMerging = BalancedMerging(5, 5)
//  println(balancedMerging.sort("input", "output1", " " :: "\n" :: Nil))

  val devices =
    LocalDevice("dev1", 3) :: LocalDevice("dev2", 3) :: LocalDevice("dev3", 3) :: LocalDevice("dev4", 3) :: LocalDevice("dev5", 3) :: LocalDevice("dev6", 3) :: Nil

//  val oscillatedSort = OscillatedSort(devices, 3)
//  println(oscillatedSort.sort("input", "output2", " " :: "\n" :: Nil))

  val polyphaseSort = PolyphaseSort(devices, 3)
  println(polyphaseSort.sort("input", "output3", " " :: "\n" :: Nil))
