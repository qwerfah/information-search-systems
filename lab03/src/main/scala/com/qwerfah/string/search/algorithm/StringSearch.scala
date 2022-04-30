package com.qwerfah.string.search.algorithm

trait StringSearch:
  def code: String
  
  def search(needle: String, haystack: String): Int
  
  protected val notFound: Int = -1
