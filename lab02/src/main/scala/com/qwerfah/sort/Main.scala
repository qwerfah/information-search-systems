package com.qwerfah.sort

import com.qwerfah.sort.file.algorithms.*
import com.qwerfah.sort.file.devices.LocalDevice
import com.qwerfah.sort.file.ops.FileOps

import scala.reflect.ClassTag

object Main extends App:
  import ClassTags.given
  import Conversions.given

  def devices =
    LocalDevice("dev1", 3) ::
    LocalDevice("dev2", 3) ::
    LocalDevice("dev3", 3) ::
    LocalDevice("dev4", 3) ::
    LocalDevice("dev5", 3) ::
    LocalDevice("dev6", 3) :: Nil

  val balancedMerging = BalancedMerging(devices, 3)
  println(s"\nBalanced merging: ${balancedMerging.sort("input", "output1", " " :: "\n" :: Nil)}\n")

  val oscillatedSort = OscillatedSort(devices, 3)
  println(s"\nOscillated sort: ${oscillatedSort.sort("input", "output2", " " :: "\n" :: Nil)}\n")

  val polyphaseSort = PolyphaseSort(devices, 3)
  println(s"\npolyphase sort: ${polyphaseSort.sort("input", "output3", " " :: "\n" :: Nil)}\n")
