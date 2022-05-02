package com.qwerfah.string.search

import com.qwerfah.string.search.algorithm.KnuthMorrisPratSearch

@main def hello: Unit =
  val search = KnuthMorrisPratSearch()
  val pr = KnuthMorrisPratSearch.prefix("abcdabcabcdabcdab")
  println(pr.toList)
