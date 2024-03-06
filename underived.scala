import scala.deriving.*
import scala.compiletime.*

trait MyTypeClass[T] {
  def doSomethingFunny(value: T): Long
}

object MyTypeClass {
  given MyTypeClass[String] = _.length
  given MyTypeClass[Int] = _.toLong
  given MyTypeClass[Long] = identity(_)
}

extension [T](t: T)(using tc: MyTypeClass[T]) {
  def doSomethingFunny = tc.doSomethingFunny(t)
}

case class UserDefinedType(text: String, number: Long)

// we want this to be derived
given MyTypeClass[UserDefinedType] = new MyTypeClass[UserDefinedType] {
  override def doSomethingFunny(value: UserDefinedType): Long =
    value.number.doSomethingFunny + value.text.doSomethingFunny
}

object UnderivedMain extends App {
  val userDefinedValue = UserDefinedType("hello", 0)
  println(userDefinedValue.doSomethingFunny)
}
