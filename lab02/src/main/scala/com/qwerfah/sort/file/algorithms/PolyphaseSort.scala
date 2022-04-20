package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.algorithms.FileSort

import scala.reflect.ClassTag
import scala.util.Try

class PolyphaseSort extends FileSort:
  override def code: String = "PolyphaseSort"

  def sort[T: ClassTag](input: String, output: String, delimeters: Seq[String])(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit] = ???
