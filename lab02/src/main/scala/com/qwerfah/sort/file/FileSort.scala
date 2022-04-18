package com.qwerfah.sort.file

import scala.reflect.ClassTag
import scala.util.Try

trait FileSort:
  def code: String
  def sort[T: ClassTag](input: String, output: String, delim: String)(using
      conversion: Conversion[String, T],
      order: Ordering[T]
  ): Try[Unit]
