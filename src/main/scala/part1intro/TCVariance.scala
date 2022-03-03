package org.arrnaux
package part1intro

object TCVariance{

  import cats.Eq
  import cats.instances.int._ // this brings into scope Eq[Int] TC instance
  import cats.instances.option._ // construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found - this is related to variance

  // variance recap
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat extends Animal, so Cage[Cat] is a subtype of Cage[Anima]
  // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type will propagate subtyping BACKWARDS to the generic type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat extends Animal, then Ven[Animal] is a subtype of Vet[Cat]
  // Cat <: Animal, then Vet[Animal] <: Vet[Cat]
  // on runtime, I will __provide__ to you a __better_ Vet,
  // a Vet[Animal], which can heal any animal, so it can work on your cat as well.

  // rule of thumb: if a generic type "HAS a T" => covariant
  //                if it "ACTS/OPERATES on T"  => contravariant
  // the variance affect how TC instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

  makeSound[Animal] // ok, because we have an implicit defined above
  makeSound[Cat] // ok, the TC instance is also applicable to Cats

  // rule 1: contravariant TC can use the superclass instances if nothing is available strictly for that type

  // this has implications for subtypes
  // eg.:
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]


  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "cats everywhere"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present


  // rule 3: you can't have them both
  // Cats uses INVARIANT type classes
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat])      // ok - the compiler will inject CatsShow as implicit
    //println(organizeShow[Animal]) // this will not compile - ambiguous because we have 2 potential implicits
  }
}
