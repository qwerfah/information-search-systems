package com.qwerfah.sort
package algorithm

trait SortAlgorithm:
  def code: String
  def sort[T](sequence: Seq[T]): Seq[T]
