package com.qwerfah.sort
package algorithm

class QuickSortSpec extends SortAlgorithmSpec:
  "Quick sort" should "sort correctly sequences of any AnyVal type" in test(QuickSort())
