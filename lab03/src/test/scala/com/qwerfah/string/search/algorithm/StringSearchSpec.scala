package com.qwerfah.string.search.algorithm

trait StringSearchSpec extends munit.FunSuite:
  protected def algo: StringSearch

  test(s"${algo.code} should search correctly") {
    assertEquals(algo.search("some", "sm"), List.empty)
    assertEquals(algo.search("some", "smthing"), List.empty)
    assertEquals(algo.search("some", "some"), List(0))
    assertEquals(algo.search("some", "something"), List(0))
    assertEquals(algo.search("some", "thingsome"), List(5))
    assertEquals(algo.search("some", "thingsomething"), List(5))
    assertEquals(algo.search("some", "thingsomethingsome"), List(5, 14))
  }
