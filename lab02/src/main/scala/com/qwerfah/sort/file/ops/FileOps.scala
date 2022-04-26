package com.qwerfah.sort.file.ops

import java.io.File
import java.util.Scanner
import scala.util.{Failure, Success, Try}

object FileOps:
  def fileIterator(input: String, delimeters: Seq[String]): Iterator[String] =
    fileIterator(new File(input), delimeters)

  /** Создает итератор по файлу, который считывает данные из файла токенами с помощью экземпляра Scanner. Границы
    * каждого токена определяются списком разделителей. Итератор может быть использован для чтения из файла блоками с
    * помощью take().toList. Как только итератор достигнет конца файла, используемый для чтения из файла Scanner будет
    * освобожден, а каждая последующая попытка считать блок токенов из файла будет возвращать пустую
    * последовательность.
    */
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
