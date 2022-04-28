package com.qwerfah.sort.file.algorithms

import com.qwerfah.sort.file.devices.{Device, LocalDevice}
import com.qwerfah.sort.file.ops.FileOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag
import scala.util.Success

trait SortSpec extends AnyFlatSpec with Matchers:
  def sortSpec[TSort <: FileSort, TElem: ClassTag](
      factory: (Seq[Device], Int) => TSort,
      sorted: Seq[TElem],
      deviceCount: Int = 6,
      blockLength: Int = 3,
      input: String = "input",
      output: String = "output",
      delimeters: Seq[String] = " " :: "\n" :: Nil
  )(using
      conversion: Conversion[String, TElem],
      order: Ordering[TElem]
  ): Unit =
    val devices = (1 to deviceCount).map { i => LocalDevice(s"dev$i", blockLength) }
    val sortAlgo = factory(devices, blockLength)

    sortAlgo.sort[TElem](input, output, delimeters) shouldBe 
      Success(math.ceil(sorted.length.toDouble / blockLength.toDouble).toInt)
    val resultIt = FileOps.fileIterator(output, delimeters)
    resultIt.takeWhile(_ => resultIt.hasNext).map(conversion(_)).toSeq shouldBe sorted

final class BalancedMergingSpec extends SortSpec:
  private val expected =
    Seq(1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 56)

  "BalancedMerging" should "sort correctly any file that can be splited into sequence of tokens" in {
    import com.qwerfah.sort.Conversions.given
    import com.qwerfah.sort.ClassTags.given

    sortSpec[BalancedMerging, Int]((devices, blockLength) => BalancedMerging(devices, blockLength), expected)
  }

final class OscillatedSortSpec extends SortSpec:
  private val expected =
    Seq(1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 56)

  "OscillatedSort" should "sort correctly any file that can be splited into sequence of tokens" in {
    import com.qwerfah.sort.Conversions.given
    import com.qwerfah.sort.ClassTags.given

    sortSpec[OscillatedSort, Int]((devices, blockLength) => OscillatedSort(devices, blockLength), expected)
  }

final class PolyphaseSortSpec extends SortSpec:
  private val expected =
    Seq(1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 56)

  "PolyphaseSort" should "sort correctly any file that can be splited into sequence of tokens" in {
    import com.qwerfah.sort.Conversions.given
    import com.qwerfah.sort.ClassTags.given

    sortSpec[PolyphaseSort, Int]((devices, blockLength) => PolyphaseSort(devices, blockLength), expected)
  }
