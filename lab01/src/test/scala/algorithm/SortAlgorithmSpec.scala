package com.qwerfah.sort
package algorithm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait SortAlgorithmSpec extends AnyFlatSpec with Matchers:
  def test(algo: SortAlgorithm): Unit =
    val intSeq = List(5, 4, 3, 2, 1, 2, 3, 4, 5)
    val doubleSeq = List(5D, 4D, 3D, 2D, 1D, 2D, 3D, 4D, 5D)
    val charSeq = List('b', 'c', 'f', 'd', 'j', 'a')

    algo.sort(intSeq) shouldBe List(1, 2, 2, 3, 3, 4, 4, 5, 5)
    algo.sort(doubleSeq) shouldBe List(1D, 2D, 2D, 3D, 3D, 4D, 4D, 5D, 5D)
    algo.sort(charSeq) shouldBe List('a', 'b', 'c', 'd', 'f', 'j')
