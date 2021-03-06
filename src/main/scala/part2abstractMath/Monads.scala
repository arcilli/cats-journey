package org.arrnaux
package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List("a", "b", "c", "d")
  // TODO 1.1 how do you create all combinations of (number, char)?
  val combinationsList: List[(Int, String)] = numbersList.flatMap(n => charsList.map(c => (n, c)))

  val combinationsListFor: List[(Int, String)] = for {
    number <- numbersList
    char <- charsList
  } yield (number, char)
  // The above 2 implementations are identical

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO: 1.2: how do you create the combination of (number, char)?
  val combinationOption: Option[(Int, Char)] = numberOption.flatMap(k => charOption.map(x => (k, x)))
  val combinationOptionFor: Option[(Int, Char)] = for {
    number <- numberOption
    char <- charOption
  } yield (number, char)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('W')
  // TODO 1.3: how do you create the combination of (number, char)?
  val combinationFuture: Future[(Int, Char)] = numberFuture.flatMap(k => charFuture.map(c => (k, c)))
  val combinationFutureFor: Future[(Int, Char)] = for {
    number <- numberFuture
    char <- charFuture
  } yield (number, char)


  /**
   * The pattern consist of 2 fundamental operations:
   *  - wrapping a value into a M(onadic) value
   *  - the flatMap mechanism
   *
   * MONADS
   */
  trait MyMonad[M[_]] { // higher-kinded type  class, M is itself generic
    // capability #1
    def pure[A](value: A): M[A]

    // capability #2: flatMap
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // This functor is implemented just using #map and #flatMap
    // We can say that a monad "extends" a functor because the fundamental method of a functor, which is #map
    def map[A, B](ma: M[A])(f: A => B): M[B] = {
      flatMap(ma)(x => pure(f(x)))
    }
  }

  // Cats monad

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)


  import cats.instances.list._

  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO 2: use a Monad[Future]

  import cats.instances.future._

  val futureMonad = Monad[Future] // requires an implicit ExecutionContext
  val aFuture: Future[Int] = futureMonad.pure(55)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 2))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], chars: Option[Char]): Option[(Int, Char)] = number.flatMap(n => chars.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], chars: Future[Char]): Future[(Int, Char)] = number.flatMap(n => chars.map(c => (n, c)))


  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods on monads - weider imports - pure, flatMap
  import cats.syntax.applicative._ // pure is here

  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap // flatMap is here

  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monads extend Functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)

  import cats.syntax.functor._ // map is here
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsForComprehension[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    import cats.syntax.flatMap._
    for {
      x <- ma
      y <- mb
    } yield (x, y) // the same as ma.flatMap(a => mb.map(b => (a, b)))
  }

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    println(getPairs(numberFuture, charFuture))
  }
}
