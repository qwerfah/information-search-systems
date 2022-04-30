package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.devices.Device
import com.qwerfah.sort.file.ops.{BlockOps, FileOps}

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.util.Scanner
import scala.collection.{Factory, IterableOnce}
import scala.io.BufferedSource
import scala.reflect.ClassTag
import scala.util.Try

final case class BalancedMerging(devices: Seq[Device], blockLength: Int) extends FileSort:
  private final case class Devices(in: Seq[Device], out: Seq[Device]):
    def swap: Devices = this.copy(in = out, out = in)

  require(devices.length > 3 && devices.length % 2 == 0, "Round robin queue length must be positive")
  require(blockLength > 0, "Input file block length must be positive")

  override def code: String = "BalancedMerging"

  private var inOutDevices = Devices(devices.take(devices.length / 2), devices.drop(devices.length / 2))

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Int] = Try {
    print(s"$code: spliting input...\n")
    splitStage(input, delimeters)
    print(s"$code: splited, merging...\n")
    val outputLength = mergeStage(output, delimeters)
    print(s"$code: done\n")
    outputLength
  }

  private def mergeStage[T: ClassTag](output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Int =
    var inIndices = inOutDevices.in.zipWithIndex.filter(_._1.nonEmpty).map(_._2)
    val delim = delimeters.head

    // Сливаем, пока не останется два файла для слияния, это и будет результат
    while inIndices.length > 2 do
      val outputs = Iterator.continually(inOutDevices.out.indices).flatten

      // Читаем из входных файлов, пока не прочитаем все
      while inIndices.nonEmpty do
        println(s"$code: reading from devices: ${for i <- inIndices yield inOutDevices.in(i).id}")
        val blocks = for i <- inIndices yield inOutDevices.in(i).read(delim)
        println(s"$code: read blocks: $blocks")
        val outInd = outputs.next()
        inOutDevices.out(outInd).write(blocks, delim)
        println(s"$code: wrote blocks to dev ${inOutDevices.out(outInd).id}")
        inIndices = inOutDevices.in.zipWithIndex.filter(_._1.nonEmpty).map(_._2)

      inOutDevices = inOutDevices.swap
      inIndices = inOutDevices.in.zipWithIndex.filter(_._1.nonEmpty).map(_._2)

    for i <- inIndices do assert(inOutDevices.in(i).filesCount == 1)

    // Формируем и записываем результирующий блок
    println(s"$code: reading from devices: ${for i <- inIndices yield inOutDevices.in(i).id}")
    val blocks = for i <- inIndices yield inOutDevices.in(i).read(delim)
    println(s"$code: read blocks: $blocks")
    inOutDevices.in.head.write(output, blocks, delim)
    println(s"$code: wrote to result file")
    inOutDevices.in.head.lastFileSize.getOrElse(-1)

  private def splitStage[T: ClassTag](input: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Unit =
    val roundRobin = Iterator.continually(inOutDevices.out.indices).flatten
    val fileIterator = FileOps.fileIterator(input, delimeters)
    val delim = delimeters.head

    while fileIterator.hasNext do
      val block = fileIterator.take(blockLength).map(conversion(_)).toSeq
      print(s"$code: block read: ${block.mkString(delim)}\n")
      val writerInd = roundRobin.next
      inOutDevices.out(writerInd).write(Seq(block), delim)
      print(s"$code: block wrote to file ${inOutDevices.out(writerInd).id}\n")

    inOutDevices = inOutDevices.swap
