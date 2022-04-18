package com.qwerfah.sort.file

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import scala.collection.{Factory, IterableOnce}
import scala.io.BufferedSource
import scala.reflect.ClassTag
import scala.util.Try

class BalancedMerging(queueLength: Int, blockLength: Int) extends FileSort:
  require(queueLength > 0, "Round robin queue length must be positive")
  require(blockLength > 0, "Input file block length must be positive")

  override def code: String = "BalancedMerging"

  private var files = (0 until queueLength).map { i =>
    new File(s"temp#1#$i")
  } -> (0 until queueLength).map { i =>
    new File(s"temp#2#$i")
  }

  def sort[T: ClassTag](input: String, output: String, delim: String)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit] =
    Try {
      print("spliting...\n")
      splitStage(input, delim)
      print("splited, merging...\n")
      mergeStage(output, delim)
      print("done\n")
    }

  private def mergeStage[T: ClassTag](output: String, delim: String)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Unit =
    var sources = files._1.map(scala.io.Source.fromFile(_)).filter(_.nonEmpty)

    // Сливаем, пока не останется два файла для слияния, это и будет результат
    while sources.length > 2 do
      var inputs = sources.map(_.getLines())
      val writers = files._2.map(new PrintWriter(_))
      val outputs = Iterator.continually(writers.indices).flatten

      // Читаем из входных файлов, пока не прочитаем все
      while inputs.nonEmpty do
        val block = formBlock(inputs, delim) // Формируем новый блок из первых блоков всех входных файлов
        writers(outputs.next()).write(block.mkString(delim)) // записываем очередной блок в следующий файл из round-robin
        inputs = inputs.filter(_.hasNext) // Оставляем только еще недочитанные файлы

      sources.foreach(_.close())
      writers.foreach(_.close())

      files = files.swap // Свапаем входные и выходные файлы местами
      sources = files._1.map(scala.io.Source.fromFile(_)).filter(_.nonEmpty)

    // Формируем и записываем результирующий блок
    val block = formBlock(sources.map(_.getLines()), delim)
    val resultWriter = new PrintWriter(new File(output))
    resultWriter.write(block.mkString(delim))
    resultWriter.close()

  private def formBlock[T: ClassTag](iterators: Seq[Iterator[String]], delim: String)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Seq[T] =
    // Загружаем по блоку из каждого итератора
    val blocks = iterators.collect {
      case it if it.hasNext =>
        val block = it.next().trim.split(delim).map(conversion(_))
        //assert(block.length == blockLength)
        block
    }

    val newBlockLength = blocks.map(_.length).sum

    // Выполняем слияние всех загруженных блоков в один упорядоченный
    val (newBlocks, sorted) = (0 until newBlockLength).foldLeft(blocks -> List.empty[T]) { case ((blocks, sorted), _) =>
      val (min, minInd) = blocks.zipWithIndex.tail.foldLeft(blocks.head.head -> 0) {
        case ((min, minInd), (block, blockInd)) =>
          if order.compare(block.head, min) < 0 then block.head -> blockInd
          else min -> minInd
      }

      val newBlocks = if blocks(minInd).length > 1 then Seq(blocks(minInd).tail) else Seq.empty
      blocks.patch(minInd, newBlocks, 1) -> (sorted :+ min)
    }

    assert(newBlocks.isEmpty)

    sorted

  private def isDegreeOfTwo(num: Int): Boolean =
    val log = math.log(blockLength) / math.log(2)
    log == log.toInt

  private def splitStage[T: ClassTag](input: String, delim: String)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Unit =
    val writers = files._1.map(new PrintWriter(_))
    val roundRobin = Iterator.continually(writers.indices).flatten
    val bufferedSource = scala.io.Source.fromFile(input)

    val lastBlock = bufferedSource.getLines().foldLeft(Array.empty[String]) { case (rest, line) =>
      val blocks = (rest ++ line.trim.split(delim)).grouped(blockLength)
      var lastBlock: Option[Array[String]] = None

      for block <- blocks do
        if block.length == blockLength then
          val sorted = block.map(conversion(_)).sorted
          print(s"${sorted.mkString(delim)}\n")
          writers(roundRobin.next).write(s"${sorted.mkString(delim)}\n")
        else lastBlock = Some(block)

      lastBlock.getOrElse(Array.empty[String])
    }

    if lastBlock.nonEmpty then
      writers(roundRobin.next).write(lastBlock.map(conversion(_)).sorted.mkString(delim))

    writers.foreach(_.close())
    bufferedSource.close()
