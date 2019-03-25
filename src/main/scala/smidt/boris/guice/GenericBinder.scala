package smidt.boris.guice

import com.google.inject.{Binder, TypeLiteral}
import com.typesafe.scalalogging.StrictLogging
import io.github.classgraph.{ClassGraph, ScanResult}
import smidt.boris.guice.internal.DefaultMirror._
import smidt.boris.guice.internal.TypeConverter

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.higherKinds
import scala.ref.WeakReference
import scala.reflect.runtime.universe._


//todo define a schearch scope for the user and cache the scan result if the scope is the same.
// automaticly close after a certain time.

case class ScopeDef(
                     packagesVal: Seq[String] = Seq(),
                     jarsVal: Seq[String] = Seq(),
                     modulesVal: Seq[String] = Seq(),
                     classesVal: Seq[String] = Seq(),
                   ) {

  def packages(p: String*) = {
    this.copy(packagesVal = p)
  }

  def jar(j: String*) = {
    this.copy(jarsVal = j)
  }

  def modules(m: String*) = {
    this.copy(modulesVal = m)
  }

  def classes(c: String*) = {
    this.copy(classesVal = c)
  }
}

case class SearchScope(
                        include: ScopeDef = ScopeDef(),
                        exclude: ScopeDef = ScopeDef(),
                        scanJars: Boolean = true,
                        scanNestedJars: Boolean = true,
                        scanDirs: Boolean = true,
                        scanModules: Boolean = true
                      )


/**
  * helps for creating bindings by searching the classpath
  */
class GenericBinder(implicit val binder: Binder){
  trait TypeTagCaputerer[T] {

    def apply()(implicit typeTag: TypeTag[T], scanner: SearchScope) = GenericBinder.bindType(typeTag.tpe)

  }
  //cannot pass type tag of higher kinded type so i have to lower the kind
  //compile time enforce that the type you are binding has a type parameter
  def bind1[F[_]] = new TypeTagCaputerer[F[_]] {}

  def bind2[F[_, _]] = new TypeTagCaputerer[F[_, _]] {}

  def bind3[F[_, _, _]] = new TypeTagCaputerer[F[_, _, _]] {}

  def bind4[F[_, _, _, _]] = new TypeTagCaputerer[F[_, _, _, _]] {}

  def bind5[F[_, _, _, _, _]] = new TypeTagCaputerer[F[_, _, _, _, _]] {}

  def bind6[F[_, _, _, _, _, _]] = new TypeTagCaputerer[F[_, _, _, _, _, _]] {}

  def bind7[F[_, _, _, _, _, _, _]] = new TypeTagCaputerer[F[_, _, _, _, _, _, _]] {}

  def bind8[F[_, _, _, _, _, _, _, _]] = new TypeTagCaputerer[F[_, _, _, _, _, _, _, _]] {}

  def bindAllImplementing[T:TypeTag]() = GenericBinder.bindAllImplementing[T]
}

object GenericBinder extends StrictLogging {

  def apply(implicit binder: Binder): GenericBinder = new GenericBinder

  val cache = mutable.Map[SearchScope, WeakReference[ScanResult]]()

  def clearCache() = {
    cache.collect{ case (_,WeakReference(x)) =>
      x.close()
    }
    cache.clear()
  }

  def bindAllImplementing[T:TypeTag] = {
    val typeTag = implicitly[TypeTag[T]]

  }

  private def scan(scope: SearchScope) = {
    cache.get(scope)
      .collect { case WeakReference(x) => x }
      .getOrElse {
        val exc = scope.exclude
        val inc = scope.include

        val scan = new ClassGraph()
          .whitelistClasses(inc.classesVal: _*)
          .whitelistJars(inc.jarsVal: _*)
          .whitelistModules(inc.modulesVal: _*)
          .whitelistPackages(inc.packagesVal: _*)
          .blacklistClasses(exc.classesVal: _*)
          .blacklistJars(exc.jarsVal: _*)
          .blacklistModules(exc.modulesVal: _*)
          .blacklistPackages(exc.packagesVal: _*)
          .verbose()
          .scan()
        cache.put(scope, WeakReference(scan))
        scan
      }
  }

  private def bindType(tpe: Type)(implicit scope: SearchScope, binder:Binder) = synchronized {
    val scanResult = scan(scope)
    val superType = tpe.dealias.typeSymbol

    val names = scanResult.getClassesImplementing(superType.fullName)
      .getStandardClasses()
      .getNames
      .asScala

    val bindings = names.map(mirror.staticClass).map { x =>
      val baseType = x.toType.baseType(superType)
      (baseType, x.toType)
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2))

    bindings.filterNot(_._2.size == 1).foreach { pair =>
      println(s"mutliple bindings detected for: ${pair._1}: [${pair._2.mkString(", ")}]")
    }

    bindings.filter(_._2.size == 1)
      .map(binding => unsafeBind(binding._1, binding._2.head))
  }

  private def typeLiteral(tpe: Type) = TypeLiteral.get(TypeConverter.scalaTypeToJavaType(tpe)).asInstanceOf[TypeLiteral[Any]]

  private def unsafeBind(x: Type, y: Type)(implicit binder: Binder) = {
    binder.bind(typeLiteral(x)).to(typeLiteral(y))
  }
}
