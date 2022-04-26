package com.qwerfah.sort.file.devices

import com.qwerfah.sort.file.ops.{BlockOps, FileOps}

import java.io.{File, PrintWriter}
import scala.collection.mutable.Stack as MutableStack

final case class LocalDevice(override val id: String, override val blockLength: Int) extends Device:
  val files: MutableStack[File] = MutableStack.empty
  val fileSizes: MutableStack[Int] = MutableStack.empty

  override def lastFileSize: Option[Int] = fileSizes.headOption
  
  override def filesCount: Int = files.length

  override def nonEmpty: Boolean = files.nonEmpty

  override def isEmpty: Boolean = files.isEmpty

  override def write[T](output: String, blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit =
    val file = new File(output)
    write(file, blocks, delim)

  override def write[T](blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit =
    val file = new File(s"$id#file#${files.length}")
    write(file, blocks, delim)

  override def write[T](file: File, blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit =
    val writer = new PrintWriter(file)

    val block = if blocks.length == 1 then blocks.head.sorted else BlockOps.merge(blocks)
    val blockCount = math.ceil(block.length.toDouble / blockLength.toDouble).toInt
    writer.write(block.map(_.toString).mkString(delim))

    writer.close()
    files.push(file)
    fileSizes.push(blockCount)

  override def read[T](delim: String)(using conversion: Conversion[String, T]): Seq[T] =
    if files.isEmpty then Seq.empty
    else
      val file = files.pop()
      val fileSize = fileSizes.pop()
      val fileIterator = FileOps.fileIterator(file, Seq(delim))
      val block = (0 until fileSize).flatMap { _ =>
        fileIterator.take(blockLength).map(conversion(_)).toSeq
      }
      assert(!fileIterator.hasNext)
      block

