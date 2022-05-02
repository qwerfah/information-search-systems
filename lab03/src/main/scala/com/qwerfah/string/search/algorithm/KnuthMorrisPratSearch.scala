package com.qwerfah.string.search.algorithm

import scala.collection.mutable.Map as MutableMap

final case class KnuthMorrisPratSearch() extends StringSearch:
  override val code: String = "KnuthMorrisPratSearch"

  override def search(needle: String, haystack: String): List[Int] =
    val prefix = KnuthMorrisPratSearch.prefix(needle).toList
    val needleLength = needle.length
    var k = 0

    (0 until haystack.length).toList.flatMap { i =>
      while k > 0 && haystack(i) != needle(k) do
        k = prefix(k - 1)
      k += (if haystack(i) == needle(k) then 1 else 0)
      if k == needleLength then
        k = prefix(k - 1)
        Some(i - needleLength + 1)
      else None
    }


object KnuthMorrisPratSearch:
  private val _prefix: MutableMap[Int, Int] = MutableMap.empty

  private def updatePrefix(str: String, i: Int): Int =
    _prefix.getOrElseUpdate(i, {
      if i == 0 then 0
      else
        var k = _prefix(i - 1)
        while k > 0 && str(i) != str(k) do
          k = _prefix(k - 1)
        if str(i) == str(k) then k + 1 else k
    })

  def prefix(str: String): LazyList[Int] =
    _prefix.clear()
    LazyList.from(0 until str.length).map(i => updatePrefix(str, i))

