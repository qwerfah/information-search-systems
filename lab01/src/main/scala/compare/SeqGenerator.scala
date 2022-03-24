package com.qwerfah.sort
package compare

import scala.util.Random

trait SeqGenerator[T]:
  def generate(length: Int): Seq[T]

trait IntGenerator extends SeqGenerator[Int]:
  override def generate(length: Int): Seq[Int] =
    0 until length map { _ => Random.nextInt }
    
trait DoubleGenerator extends SeqGenerator[Double]:
  override def generate(length: Int): Seq[Double] =
    0 until length map { _ => Random.nextDouble }