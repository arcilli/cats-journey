package org.arrnaux
package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| (combine) extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative

  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //    list.foldLeft(0)(_ |+| _)

  // MONOIDS

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInts = intMonoid.combine(23, 55) // 78
  val zero = intMonoid.empty // 0


  import cats.instances.string._ // bring the implicit Monoid[String] in scope

  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]

  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)


  // extension methods for Monoids - |+|

  import cats.syntax.monoid._ // either this one or cats.syntax.semigroup._

  val combinedOptionFancy = Option(3) |+| Option(7)


  // TODO 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  // hint: you don't need to construct the monoid itself
  val phoneBooks = List(
    Map(
      "Alize" -> 235,
      "Bobiță" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Tina" -> 123
    ),
    Map(
      "Vio" -> 999
    )
  )

  // TODO 2 solving:
  import cats.instances.map._ // This will import a monoid of a Map that will have the corresponding empty value
  val massivePhonebook = combineFold(phoneBooks)

  // TODO 3:
  case class ShoppingCart(items: List[String], total: Double) // DO NOT use Double for currency

  // hint: define your own monoid - Monoid.instance
  // hint #2: use combineByFold

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty, 0.0),
    (s1, s2) => {
      ShoppingCart(s1.items ++ s2.items, s1.total + s2.total)
    }
  )


  // TODO 3:
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  val shoppingCartItems = List(
    ShoppingCart(List("Milk", "Honey", "Beer"), 55),
    ShoppingCart(List("Chocolate", "Biscuits"), 20),
  )

  val massiveShoppingCart = checkout(shoppingCartItems)

  def main(args: Array[String]): Unit = {
    //    println(sumLeft)
    //    println(sumRight)
    //    println(combineFold(numbers))
    //    println(combineFold(List("some", "very", "nice", "thing")))


    // println(massivePhonebook)
    println(massiveShoppingCart)


  }
}
