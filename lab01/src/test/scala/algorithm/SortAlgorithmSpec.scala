package com.qwerfah.sort
package algorithm

import com.qwerfah.sort.compare.SeqGenerator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math.Ordering.Implicits._

trait SortAlgorithmSpec extends AnyFlatSpec with Matchers:
  def test(algo: SortAlgorithm): Unit =
    val intSeq = compare.Implicits.RandomIntGenerator.generate(30)
    val shortSeq = compare.Implicits.RandomShortGenerator.generate(30)
    val longSeq = compare.Implicits.RandomLongGenerator.generate(30)
    val doubleSeq = compare.Implicits.RandomDoubleGenerator.generate(30)
    val charSeq = compare.Implicits.RandomCharGenerator.generate(30)
    val byteSeq = compare.Implicits.RandomByteGenerator.generate(30)

    algo.sort(intSeq)._1 shouldBe intSeq.sorted
    algo.sort(shortSeq)._1 shouldBe shortSeq.sorted
    algo.sort(longSeq)._1 shouldBe longSeq.sorted
    algo.sort(doubleSeq)._1 shouldBe doubleSeq.sorted
    algo.sort(charSeq)._1 shouldBe charSeq.sorted
    algo.sort(byteSeq)._1 shouldBe byteSeq.sorted
