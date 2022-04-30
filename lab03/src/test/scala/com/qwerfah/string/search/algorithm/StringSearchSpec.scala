package com.qwerfah.string.search.algorithm

trait StringSearchSpec extends munit.FunSuite:
  protected def algo: StringSearch

  test(s"${algo.code} should search correctly") {
    assertEquals(algo.search("some", "sm"), -1)
    assertEquals(algo.search("some", "smthing"), -1)
    assertEquals(algo.search("some", "some"), 0)
    assertEquals(algo.search("some", "something"), 0)
    assertEquals(algo.search("some", "thingsome"), 5)
    assertEquals(algo.search("some", "thingsomething"), 5)
    assertEquals(algo.search("some", "thingsomethingsome"), 5)
  }
