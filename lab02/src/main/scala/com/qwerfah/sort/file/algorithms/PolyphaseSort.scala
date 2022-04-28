package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.algorithms.FileSort
import com.qwerfah.sort.file.devices.Device
import com.qwerfah.sort.file.ops.{BlockOps, FileOps}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Try

final case class PolyphaseSort(devices: Seq[Device], blockLength: Int) extends FileSort:
  require(devices.length >= 3)
  require(blockLength > 0)

  override def code: String = "PolyphaseSort"

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Int] = Try {
    val levels = splitStage(input, delimeters)
    val outputSize = mergeStage(output, delimeters, levels)
    Files.write(Paths.get(output), " \n".getBytes(), StandardOpenOption.APPEND)
    outputSize
  }

  def splitStage[T](input: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Int =
    val fileIterator = FileOps.fileIterator(new File(input), delimeters)
    val rowIterator = Iterator.iterate(Seq(1) ++ Seq.fill(devices.length - 2)(0))(nextRow)
    val delim = delimeters.head

    rowIterator.takeWhile(_ => fileIterator.hasNext).foldLeft(0) { case (level, refs) =>
      val deltas = refs.zip(devices.dropRight(1).map(_.filesCount)).map { case (a, b) => a - b }

      (0 until deltas.sum).foldLeft(deltas) { case (deltas, _) =>
        val (devInd, newDeltas) = getDeviceIndexToWrite(deltas)
        val sorted = fileIterator.take(blockLength).map(conversion(_)).toSeq.sorted

        devices(devInd).write(Seq(sorted), delim)
        newDeltas
      }
      level + 1
    }

  def mergeStage[T](output: String, delimeters: Seq[String], levels: Int)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Int =
    var outputSize = -1

    (1 to levels).foreach { level =>
      val inputDevices = devices.filter(_.nonEmpty)
      val outputDevice = devices.find(_.isEmpty).get
      val blocksCount = inputDevices.map(_.filesCount).min

      (0 until blocksCount).foreach { _=>
        val blocks = inputDevices.map(_.read(delimeters.head))
        if level == levels then
          outputDevice.write(output, blocks, delimeters.head)
          outputSize = outputDevice.lastFileSize.getOrElse(-1)
        else
          outputDevice.write(blocks, delimeters.head)
      }
    }

    outputSize

  def getDeviceIndexToWrite(deltas: Seq[Int]): (Int, Seq[Int]) =
    deltas.zipWithIndex.maxByOption(_._1) match {
      case Some((d, ind)) if d > 0 =>
        val newDeltas = deltas.updated(ind, deltas(ind) - 1)
        (ind, newDeltas)
      case _ => throw RuntimeException("unexpected deltas sequence")
    }

  def nextRow(currRow: Seq[Int]): Seq[Int] =
    currRow.headOption.map { head => currRow.tail.map(_ + head) :+ head }.getOrElse(Seq.empty)
