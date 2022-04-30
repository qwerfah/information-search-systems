package com.qwerfah.string.search.algorithm

class SimpleSearch extends StringSearch:
  override val code: String = "SimpleSearch"

  override def search(needle: String, haystack: String): List[Int] =
    val needleLength = needle.length
    val haystackLength = haystack.length

    if needleLength == 0 then
      notFound
    else if needleLength <= haystackLength then
      val searchLength = haystack.length - needleLength
      (0 to searchLength).map { i =>
        val indices = (0 until needleLength).takeWhile(j => haystack(i + j) == needle(j))
        if indices.length == needleLength then i else notIndex
      }.filter(_ >= 0).toList
    else
      notFound


