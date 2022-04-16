package com.qwerfah.sort
package algorithm

class SelectionSortSpec extends SortAlgorithmSpec:
  "Selection sort" should "sort correctly sequences of any AnyVal type" in test(
    SelectionSort()
  )
  "Selection sort with side effects" should "sort correctly sequences of any AnyVal type" in test(
    SelectionSortSE()
  )
