package com.qwerfah.sort

import algorithm._
import compare._
import compare.Implicits._

import breeze.linalg._
import breeze.plot._

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.given
import scala.concurrent.duration.Duration

@main def main() =
  val algorithms = BubbleSortSE() :: SelectionSortSE() :: QuickSortSE() :: Nil

  val timeMetric = TimeMetric[Int]()
  val compareMetric = CompareMetric[Int]()
  val permuteMetric = PermuteMetric[Int]()

  val timeComparator = Comparator[Int](algorithms, timeMetric)
  val compareComparator = Comparator[Int](algorithms, compareMetric)
  val permuteComparator = Comparator[Int](algorithms, permuteMetric)

  val timeResultsF = timeComparator.compareAsync(100, 1000, 10, 100)
  val compareResultsF = Future {
    print("DEBUG: comparisons comparison scheduled\n")
    val results = compareComparator.compare(100, 1000, 10, 1)
    print("DEBUG: comparisons comparison finished\n")
    results
  }
  val permuteResultsF = Future {
    print("DEBUG: permutations comparison scheduled\n")
    val results = permuteComparator.compare(100, 1000, 10, 1)
    print("DEBUG: permutations comparison finished\n")
    results
  }

  val future = for
    timeResults <- timeResultsF
    compareResults <- compareResultsF
    permuteResults <- permuteResultsF
  yield
    print("\nResults:\n")
    print(s"Time: \n$timeResults\n")
    print(s"Comparisons: \n$compareResults\n")
    print(s"Permutations: \n$permuteResults\n")

    plotResults(timeResults.asInstanceOf[(Seq[Int], Map[String, Seq[Double]])], timeMetric.code)
    plotResults(compareResults.asInstanceOf[(Seq[Int], Map[String, Seq[Double]])], compareMetric.code)
    plotResults(permuteResults.asInstanceOf[(Seq[Int], Map[String, Seq[Double]])], permuteMetric.code)

  Await.result(future, Duration.Inf)

  testExample(algorithms)

def testExample(algorithms: List[SortAlgorithm], length: Int = 10): Unit =
  val randomIntGenerator = implicitly[RandomIntGenerator]
  val directIntGeneretor = new SequentialIntGenerator { override val order = Order.Direct }
  val reverseIntGeneretor = new SequentialIntGenerator { override val order = Order.Reverse }

  val seqs = Map(
    "Direct order" -> directIntGeneretor.generate(length).toList,
    "Reverse order" -> reverseIntGeneretor.generate(length).toList,
    "Random order" -> randomIntGenerator.generate(length).toList
  )

  for (order, seq) <- seqs
  do print(s"$order: $seq\n")

  for algo <- algorithms do
    print(s"\n${algo.code}:\n")
    for (order, seq) <- seqs do
      val (sorted, stats) = algo.sort(seq)
      print(s"$order - sorted: $sorted; stats: $stats\n")

  // Console.in.read

def plotResults(results: (Seq[Int], Map[String, Seq[Double]]), metric: String): Unit =
  val f = Figure()
  val p = f.subplot(0)
  val x = results._1.map(_.toDouble)
  results._2 foreach { case (algo, y) =>
    p += plot(x, y, name = algo)
  }

  p.xlabel = "Кол-во элементов"
  p.ylabel = metric
  p.legend = true
  p.title = metric
  f.saveas("lines.png")
