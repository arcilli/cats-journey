package org.arrnaux
package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("bX", "vB")

  // specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)


  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)


  // TODO 1: without modifying the code we have so far, support a new type for a semigroup
  // hint: use the same pattern we sued with Eq
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (ex1, ex2) =>
    Expense(Math.max(ex1.id, ex2.id), ex1.amount + ex2.amount)
  }

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSUm = 2 |+| 3 // 2 __combine__ 3, it requires the presence of an implicit Semigroup[Int]
  val aStringConcat = "we" |+| "as wib"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 55)


  // TODO 2: implement reduceThings2 with the |+|
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//    list.reduce((x, y) => x |+| y)
    list.reduce(_ |+| _)


  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("A", "b", "C", "d", "E")
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit Semigroup[String]

    import cats.instances.option._
    // the compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another Option with the summed elements
    // the compiler will produce an implicit Semigroup[Option[String]] - combine will produce another Option with the concatenated elements
    // the same is true for any type with an implicit Semigroup
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // this will be an Option[Int] containing the all of all the numbers

    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
//    println(reduceThings(stringOptions))

    // test ex. 1
    val expenses = List(Expense(1, 22), Expense(2, 54), Expense(55, 555))
    println(reduceThings(expenses))

    // test ex. 2
    println(reduceThings(expenses))
  }
}
