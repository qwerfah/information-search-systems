package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.ops.{BlockOps, FileOps}

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.util.Scanner
import scala.collection.{Factory, IterableOnce}
import scala.io.BufferedSource
import scala.reflect.ClassTag
import scala.util.Try

final case class BalancedMerging(deviceCount: Int, blockLength: Int) extends FileSort:
  require(deviceCount > 0, "Round robin queue length must be positive")
  require(blockLength > 0, "Input file block length must be positive")

  override def code: String = "BalancedMerging"

  private var files = (0 until deviceCount).map { i =>
    new File(s"temp#1#$i")
  } -> (0 until deviceCount).map { i =>
    new File(s"temp#2#$i")
  }

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit] = Try {
    print("spliting...\n")
    splitStage(input, delimeters)
    print("splited, merging...\n")
    mergeStage(output, delimeters)
    print("done\n")
  }

  private def mergeStage[T: ClassTag](output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Unit =
    var inputs = files._1.map(FileOps.fileIterator(_, delimeters)).filter(_.hasNext)
    var currBlockLen = blockLength

    // Сливаем, пока не останется два файла для слияния, это и будет результат
    while inputs.length > 2 do
      val writers = files._2.map(new PrintWriter(_))
      val outputs = Iterator.continually(writers.indices).flatten
      val inputsLength = inputs.length

      // Читаем из входных файлов, пока не прочитаем все
      while inputs.nonEmpty do
        val block = formBlock(inputs, currBlockLen) // Формируем новый блок из первых блоков всех входных файлов
        writers(outputs.next())
          .write(block.mkString(delimeters.head)) // записываем очередной блок в следующий файл из round-robin
        inputs = inputs.filter(_.hasNext) // Оставляем только еще недочитанные файлы

      writers.foreach(_.close())

      files = files.swap // Свапаем входные и выходные файлы местами
      inputs = files._1.map(FileOps.fileIterator(_, delimeters)).filter(_.hasNext)
      currBlockLen *= inputsLength

    // Формируем и записываем результирующий блок
    val block = formBlock(inputs, currBlockLen)
    val resultWriter = new PrintWriter(new File(output))
    resultWriter.write(block.mkString(delimeters.head))
    resultWriter.close()

  private def formBlock[T: ClassTag](iterators: Seq[Iterator[String]], currBlockLen: Int)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Seq[T] =
    // Загружаем по блоку из каждого итератора
    val blocks = iterators.collect {
      case it if it.hasNext =>
        it.take(currBlockLen).toSeq.map(conversion(_))
    }

    // Выполняем слияние всех загруженных блоков в один упорядоченный
    BlockOps.merge(blocks)

  private def splitStage[T: ClassTag](input: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Unit =
    val writers = files._1.map(new PrintWriter(_))
    val roundRobin = Iterator.continually(writers.indices).flatten
    val bufferedSource = scala.io.Source.fromFile(input)
    val fileIterator = FileOps.fileIterator(input, delimeters)
    val delim = delimeters.head

    while fileIterator.hasNext do
      val block = fileIterator.take(blockLength).toSeq
      val sorted = block.map(conversion(_)).sorted
      print(s"${sorted.mkString(delim)}\n")
      writers(roundRobin.next).write(s"${sorted.mkString(delim)}$delim")

    writers.foreach(_.close())
    bufferedSource.close()
