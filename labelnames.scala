import scala.deriving.*
import scala.compiletime.*
import scala.reflect.ClassTag

import scala.compiletime.ops.any.*

case class MyCaseClass(text: String, number: Long, boolean: Boolean)

inline def getLabels[A](using mirror: Mirror.Of[A]): List[String] =
  getElemLabels[mirror.MirroredElemLabels]

inline def getElemLabels[A <: Tuple]: List[String] =
  inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val headElementLabel = constValueOpt[head].toString
      val tailElementLabels = getElemLabels[tail]
      headElementLabel :: tailElementLabels
  }

object LabelNamesMain extends App {
  println(getLabels[MyCaseClass])
  // println(getTypes[MyCaseClass])
}

/*
inline def getTypes[A](using mirror: Mirror.Of[A]): List[String] =
  getElemTypes[mirror.MirroredElemTypes]

inline def getElemTypes[A <: Tuple]: List[String] =
  inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val headElementLabel = guessTypeName[head]
      val tailElementLabels = getElemTypes[tail]
      headElementLabel :: tailElementLabels
  }

transparent inline def guessTypeName[T]: String =
  inline erasedValue[T] match
    case _: Byte    => "Byte"
    case _: Char    => "Char"
    case _: Short   => "Short"
    case _: Int     => "Int"
    case _: Long    => "Long"
    case _: Float   => "Float"
    case _: Double  => "Double"
    case _: Boolean => "Boolean"
    case _: String  => "String"
    case _: Unit    => "Unit"
    case _          => "unknown"
 */
