package org.arrnaux
package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43

  // simplified definition of a functor
  trait MyFunctor[F[_]] {
    // generic type argument?? What's this?
    // higher-kinded type
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor

  import cats.Functor
  import cats.instances.list._ // this includes Functor[List]s

  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)


  import cats.instances.try_._

  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // Why would we need to define such thing?
  // To generalize the API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def d010xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def d010xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)


  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]


  // smart constructors
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      }
  }

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40,
    Tree.branch(5, Tree.leaf(10), Tree.leaf(30)),
    Tree.leaf(45)
    )
  val incrementedTree = tree.map(_ + 10) // it is an extension method provided by the functor typeclass from the above
  // import, it works because you have an implicit functor of type Tree in the scope


  // TODO 2: write a shorter do10xShorter method by using the ext extension methods
  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  // F[_] : Functor means that there is an implicit functor F in the scope of this method
  // it is called "context-bound"


  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(2)))
    println(do10x(Try(45)))
    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  }
}
