import scala.compiletime.*

inline def guessTypeName[T]: String =
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

inline def isTuple[T]: Boolean =
  inline erasedValue[T] match
    case _: (h *: t) => true
    case _           => false

object ErasedValueMain extends App {
  println(guessTypeName[String])
  println(guessTypeName[List[Boolean]])
  println(isTuple[List[Boolean]])
  println(isTuple[(String, Int, Long)])
}
