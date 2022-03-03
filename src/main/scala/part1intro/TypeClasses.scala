package org.arrnaux
package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  // part1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part2 - create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String =
      s"""
         |"$value"
         |""".stripMargin.trim
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String =
      s"""
         |"${value.toString}"
         |""".stripMargin.trim
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         | {"name: "${value.name}", "age": "${value.age}"}
         |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ", ", "]")

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      // if you have declare a JSONSerializer for the type T, which declared as implicit,
      // then the compiler automatically "transforms" @value into a JSONSerializable object,
      // so in the end you can call "value.toJson"
      def toJson: String = serializer.toJson(value)
    }
  }


  def main(args: Array[String]): Unit = {
    println(convertListToJSON(List(Person("Alice", 23), Person("Costel", 45))))

    val bob = Person("Bob", 35)
    import JSONSyntax._
    println(bob.toJson)
  }
}
