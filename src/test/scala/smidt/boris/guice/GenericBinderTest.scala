package smidt.boris.guice

import com.google.inject.Guice
import javax.inject.{Inject, Singleton}
import net.codingwell.scalaguice.{ScalaModule, ScalaMultibinder}
import org.scalatest.FunSuite

import scala.collection.JavaConverters._

trait SuperClass[A] {
  type Unknown = A
  def print(a:A) = {
    println("hello: " + a)
  }
}

class SuperClassString extends SuperClass[String]

class SuperClassInt extends SuperClass[Int]

class SuperClassLong extends SuperClass[Long]

class SuperClassShort  extends SuperClass[Short]

class SuperClassDouble  extends SuperClass[Double]

class SuperClassDoubling extends SuperClass[Double] {
  override def print(a: Double): Unit = {
    super.print(a * 2)
  }
}

class SuperClassFloat extends SuperClass[Float]

trait SuperClassArray[T] extends SuperClass[Array[T]]

class SuperClassArrayString extends SuperClassArray[String]


object TestModule extends ScalaModule {

  override def configure(): Unit = {
    implicit val scanner = SearchScope(
      include = ScopeDef()
    )
    val stringMulti = ScalaMultibinder.newSetBinder[String](binder)
    stringMulti.addBinding.toInstance("A")
    val x = GenericBinder(binder).bind1[SuperClass]
  }

}

@Singleton
class TestPrinter @Inject() (p: SuperClass[java.lang.Float]) {

  def print(): Unit = {
    p.print(10.2f)
  }

}
class GenericBinderTest extends FunSuite {
  test("should detect all the unique classes") {
    val injector = Guice.createInjector(TestModule)
    println(injector.getAllBindings.asScala)

  }
}
