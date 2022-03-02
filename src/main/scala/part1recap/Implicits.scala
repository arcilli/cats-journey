package org.arrnaux
package part1recap

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  //  val impersonableString = new ImpersonableString("Peter")
  //  impersonableString.greet

  // It is called "extension method"
  val greeting = "Peter".greet // compiler does new ImpersonableString("Peter").greet

  // famous example of extension methods example: importing implicit conversions in scope

  import scala.concurrent.duration._

  val oneSec: FiniteDuration = 1.second


  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount: Int = 10
  val incremented2: Int = increment(2) // implicit argument 10 is passed by the compiler

  def multiply(x: Int)(implicit times: Int) = x * times

  val times2 = multiply(2)


  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")


  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}"}
         |""".stripMargin
  }
  val personsJson: String = listToJson(List(Person("Alice"), Person("Bobiță")))
  // implicit argument is used to PROVE THE EXISTENCE of a type


  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] { // Product is the supertype for any case class
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}": "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  case class Cat(catName: String)

  // in the background, the compiler will do:
  // listToJson(List(...))(oneArgCaseClassSerializer[Cat])
  val catsToJson = listToJson(List(Cat("Tom"), Cat("Gurlfield")))
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // implicit methods can be used also as a conversion (but this is DISCOURAGED)


  def main(args: Array[String]): Unit = {
    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("Gigel")))
  }
}
