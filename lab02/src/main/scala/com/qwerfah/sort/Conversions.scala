package com.qwerfah.sort

object Conversions:
  given Conversion[String, Int] = Integer.parseInt(_)
