package com.qwerfah.sort.file.devices

import java.io.{File, PrintWriter}

/** Описывает базовый функционал устройства для записи/чтения блоков данных произвольного типа. Устройство работает по
 * принципу LIFO, то есть метод read возвращает блоки, которые были записаны в последнем вызове метода write.
 */
trait Device:
  /** Идентификатор устройства, используется для вывода отладочной информации и */
  def id: String

  /** Размер одного записываемого/считываемого блока. */
  def blockLength: Int

  /** Размер последнего записанного на устройство файла в блоках. */
  def lastFileSize: Option[Int]
  
  def filesCount: Int
  
  def nonEmpty: Boolean

  def isEmpty: Boolean

  /** Записать на устройство набор блоков. Набор записывается в отдельный файл на этом устройстве. Данные из блоков
   * записываются последовательно и линейно через указанный разделитель..
   */
  def write[T](blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit
  def write[T](output: String, blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit
  def write[T](file: File, blocks: Seq[Seq[T]], delim: String)(using order: Ordering[T]): Unit

  /** Считать все блоки из последнего записанного на устройство файла. */
  def read[T](delim: String)(using conversion: Conversion[String, T]): Seq[T]