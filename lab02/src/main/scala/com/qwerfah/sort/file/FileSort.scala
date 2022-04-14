package com.qwerfah.sort.file

trait FileSort:
  def code: String
  def sort[T]: Seq[T]
