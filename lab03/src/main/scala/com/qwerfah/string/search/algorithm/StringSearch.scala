package com.qwerfah.string.search.algorithm

trait StringSearch:
  def code: String

  def search(needle: String, haystack: String): List[Int]
  
  protected final val notFound: List[Int] = List.empty
  protected final val notIndex: Int = -1
