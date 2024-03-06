import scala.deriving.*
import scala.compiletime.*

trait MyTypeClass[T] {
  def doSomethingFunny(value: T): Long
}

object MyTypeClass {
  given MyTypeClass[String] = _.length
  given MyTypeClass[Int] = _.toLong
  given MyTypeClass[Long] = identity(_)

  // Say we only know how to implement it for products, no need to use Mirror.Of
  inline def derived[T](using mirror: Mirror.ProductOf[T]): MyTypeClass[T] =
    val instancesForMembers =
      summonInstancesForMembers[mirror.MirroredElemTypes]
    instanceForProduct(instancesForMembers)

  inline def summonInstancesForMembers[Members <: Tuple]: List[MyTypeClass[?]] =
    inline erasedValue[Members] match {
      case _: (member *: restOfMembers) =>
        summonInline[MyTypeClass[member]] ::
          summonInstancesForMembers[restOfMembers]

      case EmptyTuple => Nil
    }

  inline def instanceForProduct[T](
      instancesForMembers: List[MyTypeClass[?]]
  ) =
    new MyTypeClass[T] {
      override def doSomethingFunny(value: T): Long =
        val membersIterator = value.asInstanceOf[Product].productIterator
        val membersWithInstances = membersIterator.zip(instancesForMembers)

        membersWithInstances
          .map((member, instance) =>
            instance.asInstanceOf[MyTypeClass[Any]].doSomethingFunny(member)
          )
          .reduce(_ + _)
    }

}

extension [T](t: T)(using tc: MyTypeClass[T]) {
  def doSomethingFunny = tc.doSomethingFunny(t)
}

case class UserDefinedType(text: String, number: Long) derives MyTypeClass

object DeriveMain extends App {
  val userDefinedValue = UserDefinedType("hello", 0)
  println(userDefinedValue.doSomethingFunny)
}
