package smidt.boris.guice.internal
import java.lang.reflect.{Type => JavaType}

import com.google.inject.internal.MoreTypes.WildcardTypeImpl
import com.google.inject.util.Types._

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{Type => ScalaType, _}

object TypeConverter {
  private val anyType = typeOf[Any]

  object ArrayType {
    private val arraySymbol = symbolOf[Array[_]]

    def unapply(tpe: ScalaType): Option[ScalaType] = {
      tpe match {
        case TypeRef(pre, sym, args) if sym == arraySymbol => args.headOption
        case _ => None
      }

    }
  }

  object ClassType {

    def unapply(tpe: ScalaType): Option[(ClassSymbol, Seq[ScalaType])] = {
      tpe match {
        case TypeRef(pre, sym, args) if sym.isClass => Some((sym.asClass, args))
        case _ => None
      }

    }
  }

  object WildcardType {
    private val bottomType = typeOf[Nothing]
    private val topType = typeOf[Any]
    def unapply(tpe: ScalaType): Option[(List[ScalaType], List[ScalaType])] = {
      tpe match {
        case BoundedWildcardType(bounds) => unapply(bounds)
        case TypeRef(pre, sym, args) if sym.isType && !sym.isSpecialized =>
          unapply(sym.asType.info)
        case TypeBounds(lo, hi) =>
          val lowerBounds = if(lo =:= bottomType) Nil else List(lo)
          Some((lowerBounds, List(hi)))
        case _ => None
      }

    }

  }

  def scalaTypeToJavaType(scalaType: ScalaType)(implicit mirror: Mirror): JavaType = {
    scalaType.dealias match {
      case `anyType` => classOf[java.lang.Object]
      case ExistentialType(symbols, underlying) => scalaTypeToJavaType(underlying)
      case ArrayType(argType) => arrayOf(scalaTypeToJavaType(argType))
      case ClassType(symbol, args) => {
        val rawType = mirror.runtimeClass(symbol)
        val ownerType = findOwnerOf(symbol, mirror)
        args.map(arg => scalaTypeToJavaType(arg)) match {
          case Nil => toWrapper(rawType)
          case mappedArgs if ownerType.nonEmpty => newParameterizedTypeWithOwner(ownerType.get, rawType, mappedArgs:_*)
          case mappedArgs => newParameterizedType(rawType, mappedArgs:_*)
        }
      }
      case WildcardType(lowerBounds, upperBounds) => {
        val mappedUpperBounds = upperBounds.map(bound => scalaTypeToJavaType(bound)).toArray
        val mappedLowerBounds = lowerBounds.map(bound => scalaTypeToJavaType(bound)).toArray
        new WildcardTypeImpl(mappedUpperBounds, mappedLowerBounds)
      }
      case _ => throw new UnsupportedOperationException(s"Could not convert scalaType $scalaType to a javaType: " + scalaType.dealias.getClass.getName)
    }
  }

  private def findOwnerOf(symbol: universe.ClassSymbol, mirror: Mirror): Option[JavaType] = {
    val owner = symbol.owner

    if (!owner.isPackage && (owner.isModuleClass && owner.owner.isPackage)) { //workaround for when owner is a top level object
      // here we need to resolve class without the $ suffix for some reason
      val clazz = mirror.staticClass(owner.asClass.fullName)
      Some(mirror.runtimeClass(clazz))
    } else if (!owner.isPackage && owner.isClass) {
      Some(mirror.runtimeClass(owner.asClass))
    } else {
      None
    }
  }

  private def toWrapper(c: JavaType) = c match {
    case java.lang.Byte.TYPE => classOf[java.lang.Byte]
    case java.lang.Short.TYPE => classOf[java.lang.Short]
    case java.lang.Character.TYPE => classOf[java.lang.Character]
    case java.lang.Integer.TYPE => classOf[java.lang.Integer]
    case java.lang.Long.TYPE => classOf[java.lang.Long]
    case java.lang.Float.TYPE => classOf[java.lang.Float]
    case java.lang.Double.TYPE => classOf[java.lang.Double]
    case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
    case java.lang.Void.TYPE => classOf[java.lang.Void]
    case cls => cls
  }

}