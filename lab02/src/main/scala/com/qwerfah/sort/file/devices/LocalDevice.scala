package com.qwerfah.sort.file.devices

import com.qwerfah.sort.file.ops.{BlockOps, FileOps}

import java.io.{File, PrintWriter}
import scala.collection.mutable.Stack as MutableStack

final case class LocalDevice(override val id: String, override val blockLength: Int) extends Device:
  val files: MutableStack[File] = MutableStack.empty
  val fileSizes: MutableStack[Int] = MutableStack.empty

  override def lastFileSize: Int = fileSizes.head

  override def write[T](blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit =
    val file = new File(s"$id#file#${files.length}")
    val writer = new PrintWriter(file)
    val blocksPerBlock = math.ceil(blocks.head.length.toDouble / blockLength.toDouble).toInt
    val blockCount = blocks.length

    if blockCount == 1 then writer.println(blocks.head.sorted.map(_.toString).mkString(delim))
    else writer.println(BlockOps.merge(blocks).map(_.toString).mkString(delim))

    writer.close()
    files.push(file)
    fileSizes.push(blockCount * blocksPerBlock)

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

