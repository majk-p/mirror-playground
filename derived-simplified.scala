import scala.deriving.*
import scala.compiletime.*

trait MyTypeClass[T] {
  def doSomethingFunny(value: T): Long
}

object MyTypeClass {
  given MyTypeClass[String] = _.length
  given MyTypeClass[Int] = _.toLong
  given MyTypeClass[Long] = identity(_)

  inline def derived[T](using mirror: Mirror.ProductOf[T]): MyTypeClass[T] =

    /** Tuple.Map Converts a tuple `(T1, ..., Tn)` to `(F[T1], ..., F[Tn])`, so
      * effectively we have for Product (A, B, C, D) we end up with a tuple like
      * (MyTypeClass[A], MyTypeClass[B], MyTypeClass[C], MyTypeClass[D])
      */
    type ElemTypesTypeClassInstance =
      Tuple.Map[mirror.MirroredElemTypes, MyTypeClass]

    // this summons all (MyTypeClass[A], MyTypeClass[B], MyTypeClass[C], MyTypeClass[D])
    val instancesForMembers = summonAll[ElemTypesTypeClassInstance]

    instanceForProduct(
      instancesForMembers.toList.asInstanceOf[List[MyTypeClass[?]]]
    )

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
