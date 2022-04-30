package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.devices.Device
import com.qwerfah.sort.file.ops.FileOps

import java.io.{File, InputStreamReader, PrintWriter}
import java.util.Scanner
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Try

final case class OscillatedSort(devices: Seq[Device], blockLength: Int) extends FileSort:
  require(devices.nonEmpty)
  require(blockLength > 0)

  override def code: String = "OscillatedSort"

  private val splitBound = devices.length - 1

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Int] = Try {
    // Кольцевая очередь файлов для распределения блоков из исходного файла
    val splitRoundRobin = Iterator.continually(devices.indices).flatten
    // Кольцевая очередь файлов для слияния распределенных блоков
    val mergeRoundRobin = Iterator.continually(devices.indices.reverse).flatten
    // Итератор по файлу, который загружает файл в память блоками размером fileIterator
    val fileIterator = FileOps.fileIterator(input, delimeters)

    val delim = delimeters.head

    println(s"$code: start spliting to devices with merge if ready ...")

    while fileIterator.hasNext do
      val block = fileIterator.take(blockLength).toSeq.map(conversion(_))
      println(s"$code: read block from init file: $block")
      val devInd = splitRoundRobin.next
      devices(devInd).write(Seq(block), delim)
      println(s"$code: write block to output device ${devices(devInd).id}")
      mergeIfReady(delim, mergeRoundRobin)

    println(s"$code: end of init file, merging rest ...")

    val rest = Iterator
      .continually(devices.map(dev => dev.lastFileSize.getOrElse(0) -> dev.read(delim)).filter(_._2.nonEmpty))
      .takeWhile(_.nonEmpty)
    var outputSize = 0

    while rest.hasNext do
      val blocksWithSizes = rest.next
      if blocksWithSizes.length == 1 then
        assert(!rest.hasNext)
        val resultWriter = new PrintWriter(new File(output))
        resultWriter.write(blocksWithSizes.head._2.mkString(delimeters.head))
        resultWriter.close()
        outputSize = blocksWithSizes.head._1
      else devices(mergeRoundRobin.next).write(blocksWithSizes.map(_._2), delim)

    println(s"$code: done")

    outputSize
  }

  /** Сливает (devices.length - 1) блоков одинакого размера в один и записывает в очередное устройство из очереди
    * устройств для слияния. Процесс выполняется рекурсивно до тех пор, пока не будут слиты все наборы из
    * (devices.length - 1) блоков.
    */
  @tailrec private def mergeIfReady[T](delim: String, mergeRoundRobin: Iterator[Int])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Unit =
    val maybeDevicesToMerge = devices
      .filter(_.nonEmpty)
      .groupBy(_.lastFileSize.get)
      .find { case (f, devs) =>
        devs.length == splitBound
      }
      .map(_._2)

    maybeDevicesToMerge match {
      case Some(devicesToMerge) =>
        println(s"$code: devices ready to merge")
        val blocks = devicesToMerge.map(_.read(delim))
        println(s"$code: read blocks from ready devices: $blocks")
        val devInd = mergeRoundRobin.next
        devices(devInd).write(blocks, delim)
        println(s"$code: write blocks to output device ${devices(devInd).id}")
        mergeIfReady(delim, mergeRoundRobin)
      case _ =>
    }
