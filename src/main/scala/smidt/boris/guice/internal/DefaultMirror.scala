package smidt.boris.guice.internal

import scala.reflect.runtime.universe

object DefaultMirror {
  implicit val mirror = universe.runtimeMirror(getClass.getClassLoader)
}