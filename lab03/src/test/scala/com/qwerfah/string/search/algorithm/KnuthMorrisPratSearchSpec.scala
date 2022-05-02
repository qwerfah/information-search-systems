package com.qwerfah.string.search.algorithm

class KnuthMorrisPratSearchSpec extends StringSearchSpec:
  override protected final lazy val algo: StringSearch = KnuthMorrisPratSearch()

  test("KnuthMorrisPratSearch should evaluate prefix function correctly") {
    assertEquals(
      KnuthMorrisPratSearch.prefix("").toList,
      List.empty
    )
    assertEquals(
      KnuthMorrisPratSearch.prefix("a").toList,
      List(0)
    )
    assertEquals(
      KnuthMorrisPratSearch.prefix("aa").toList,
      List(0, 1)
    )
    assertEquals(
      KnuthMorrisPratSearch.prefix("abcdabscabcdabia").toList,
      List(0, 0, 0, 0, 1, 2, 0, 0, 1, 2, 3, 4, 5, 6, 0, 1)
    )
    assertEquals(
      KnuthMorrisPratSearch.prefix("abcdabcabcdabcdab").toList,
      List(0, 0, 0, 0, 1, 2, 3, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6)
    )
  }
