import scala.deriving.*
import scala.compiletime.*

case class Record(name: String, value: Long)

def mirrorForRecord(): String = {
  summon[Mirror.Of[Record]] match {
    case _: Mirror.Sum     => "Sum"
    case _: Mirror.Product => "Product"
  }
}

class RegularClass

// def mirrorForRegularClass(): String = {
//   summon[Mirror.Of[RegularClass]] match {
//     case _: Mirror.Sum     => "Sum"
//     case _: Mirror.Product => "Product"
//   }
// }

object Main extends App {
  val result = mirrorForRecord()
  println(result)

}
