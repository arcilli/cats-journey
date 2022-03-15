package org.arrnaux
package part2abstractMath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  val aManualEither: Either[String, Int] = Right(42)

  // either is also a Monad
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading: LoadingOr[Int] = loadingMonad.flatMap(anEither)(
    n => if (n % 2 == 0) Right(n + 1) else Left("Loading something")
  )

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available ")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(
    orderStatus => trackLocation(orderStatus)
  )
  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a webApp
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "40444"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // DO NOT CHANGE THE CODE

  // TODO: provide a real implementation of HttpService using Try, Option, Future, Either

  def main(args: Array[String]): Unit = {

  }
}
