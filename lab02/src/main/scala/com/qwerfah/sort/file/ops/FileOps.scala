package com.qwerfah.sort.file.ops

import java.io.File
import java.util.Scanner
import scala.util.{Failure, Success, Try}

object FileOps:
  def fileIterator(input: String, delimeters: Seq[String]): Iterator[String] =
    fileIterator(new File(input), delimeters)

  def fileIterator(input: File, delimeters: Seq[String]): Iterator[String] =
    val scanner = new Scanner(input)
    scanner.useDelimiter(delimeters.mkString("|"))
    Iterator
      .continually(Try {
        scanner.next()
      } match {
        case s @ Success(_) => s
        case f @ Failure(_) =>
          scanner.close()
          f
      })
      .takeWhile(f => f.isSuccess)
      .map(_.get)
