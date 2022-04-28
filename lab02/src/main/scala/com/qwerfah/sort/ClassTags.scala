package com.qwerfah.sort

import scala.reflect.ClassTag

object ClassTags:
  given IntClassTag: ClassTag[Int] = ClassTag(classOf[Int])
