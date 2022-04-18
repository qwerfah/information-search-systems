package com.qwerfah.sort.file

import scala.reflect.ClassTag
import scala.util.Try

class PolyphaseSort extends FileSort:
  override def code: String = "PolyphaseSort"

  def sort[T: ClassTag](input: String, output: String, delim: String)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit] = ???
