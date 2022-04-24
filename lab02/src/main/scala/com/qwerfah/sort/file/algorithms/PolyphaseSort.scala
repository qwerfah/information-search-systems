package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.algorithms.FileSort
import com.qwerfah.sort.file.devices.Device
import com.qwerfah.sort.file.ops.FileOps

import java.io.File
import scala.reflect.ClassTag
import scala.util.Try

final case class PolyphaseSort(devices: Seq[Device], blockLength: Int) extends FileSort:
  override def code: String = "PolyphaseSort"

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit] = ???

  def splitStage(input: String, delimeters: Seq[String]): Unit =
    val splitRoundRobin = Iterator.continually(devices.indices.tail).flatten
    val fileIterator = FileOps.fileIterator(new File(input), delimeters)

    while fileIterator.hasNext do
      ()


  def mergeStage: Unit = ???
