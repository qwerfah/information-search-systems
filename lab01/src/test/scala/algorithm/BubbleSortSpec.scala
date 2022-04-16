package com.qwerfah.sort
package algorithm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BubbleSortSpec extends SortAlgorithmSpec:
  "Bubble sort" should "sort correctly sequences of any AnyVal type" in test(
    BubbleSort()
  )
  "Bubble sort with side effects" should "sort correctly sequences of any AnyVal type" in test(
    BubbleSortSE()
  )
