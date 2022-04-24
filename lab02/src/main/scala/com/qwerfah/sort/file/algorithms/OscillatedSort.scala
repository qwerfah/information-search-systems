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
  override def code: String = "OscillatedSort"

  private val splitBound = devices.length - 1

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit] = Try {
    // Кольцевая очередь файлов для распределения блоков из исходного файла
    val splitRoundRobin = Iterator.continually(devices.indices).flatten
    // Кольцевая очередь файлов для слияния распределенных блоков
    val mergeRoundRobin = Iterator.continually(devices.indices.reverse).flatten
    // Итератор по файлу, который загружает файл в память блоками размером fileIterator
    val fileIterator = FileOps.fileIterator(input, delimeters)

    val delim = delimeters.head

    while fileIterator.hasNext do
      val block = fileIterator.take(blockLength).toSeq.map(conversion(_))
      devices(splitRoundRobin.next).write(Seq(block), delim)
      mergeIfReady(delim, mergeRoundRobin)

    val rest = Iterator.continually(devices.map(_.read(delim)).filter(_.nonEmpty)).takeWhile(_.nonEmpty)

    while rest.hasNext do
      val blocks = rest.next
      if blocks.length == 1 then
        assert(!rest.hasNext)
        val resultWriter = new PrintWriter(new File(output))
        resultWriter.write(blocks.head.mkString(delimeters.head))
        resultWriter.close()
      else devices(mergeRoundRobin.next).write(blocks, delim)
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
        val blocks = devicesToMerge.map(_.read(delim))
        devices(mergeRoundRobin.next).write(blocks, delim)
        mergeIfReady(delim, mergeRoundRobin)
      case _ =>
    }
