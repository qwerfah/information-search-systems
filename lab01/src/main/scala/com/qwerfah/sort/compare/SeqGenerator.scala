package com.qwerfah.sort
package compare

import scala.util.Random

trait SeqGenerator[T]:
  def generate(length: Int): Seq[T]

enum Order:
  case Direct, Reverse

trait SequentialIntGenerator extends SeqGenerator[Int]:
  def order: Order
  override def generate(length: Int): Seq[Int] =
    order match
      case Order.Direct => Range.Int(0, 1000, (1000D / length.toDouble).ceil.toInt)
      case Order.Reverse => Range.Int(1000, 0, (-1000D / length.toDouble).ceil.toInt)


trait RandomIntGenerator extends SeqGenerator[Int]:
  override def generate(length: Int): Seq[Int] =
    0 until length map { _ => Random.nextInt }

trait RandomShortGenerator extends SeqGenerator[Short]:
  override def generate(length: Int): Seq[Short] =
    0 until length map { _ => Random.nextInt.toShort }

trait RandomLongGenerator extends SeqGenerator[Long]:
  override def generate(length: Int): Seq[Long] =
    0 until length map { _ => Random.nextLong }

trait RandomDoubleGenerator extends SeqGenerator[Double]:
  override def generate(length: Int): Seq[Double] =
    0 until length map { _ => Random.nextDouble }

trait RandomCharGenerator extends SeqGenerator[Char]:
  override def generate(length: Int): Seq[Char] =
    Random.alphanumeric.take(length)

trait RandomByteGenerator extends SeqGenerator[Byte]:
  override def generate(length: Int): Seq[Byte] =
    Random.nextBytes(length)

object Implicits:
  implicit object RandomIntGenerator    extends RandomIntGenerator
  implicit object RandomShortGenerator  extends RandomShortGenerator
  implicit object RandomLongGenerator   extends RandomLongGenerator
  implicit object RandomDoubleGenerator extends RandomDoubleGenerator
  implicit object RandomCharGenerator   extends RandomCharGenerator
  implicit object RandomByteGenerator   extends RandomByteGenerator