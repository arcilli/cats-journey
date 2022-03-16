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

 def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = {
   for {
     conn <- service.getConnection(config)
     response <- service.issueRequest(conn, payload)
   } yield response
 }



  // DO NOT CHANGE THE CODE
  // TODO: provide a real implementation of HttpService using Try, Option, Future, Either

  /*
  Requirements:
  - if the host & port were found in the config map, then we'll return a M containing a connection with those values
    - otherwise, the method will fail, according to the logic of the type M
    (for Try it will return a Failure, for Option it will return None, for Future it will be a Future, for Either it will be a Left)

  - the issueRequest method returns a M containing the string: "request (payload) has been accepted" if the payload is less than 20 chars
  otherwise the method will fail, according to the logic of type M

  TODO: provide a real implementation of HttpService using Try, Option, Future, Either
  */

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = {
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(host = h, port = p)

      // basically like you would say:
      // cfg.get("host").flatMap(h => cfg.get("port").map(p => Connection(h, p)))
    }

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length >= 20) None
      else Some(s"request ($payload) has been accepted")
    }
  }

  val responseOption: Option[String] = OptionHttpService.getConnection(config).flatMap(conn => OptionHttpService.issueRequest(conn, "hello, Http service"))
  // or we could use for comprehension
  val responseOptionFor: Option[String] = for {
    connection <- OptionHttpService.getConnection(config)
    response <- OptionHttpService.issueRequest(connection, "hello, Http service")
  } yield response


  // TODO: implement another HttpService with LoadingOr or ErrorOr

  object ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      if (!(cfg.contains("host") && cfg.contains("port"))) {
        Left(new RuntimeException("Connection could not be established: invalid config map"))
      }
      else {
        Right(Connection(cfg("host"), cfg("port")))
      }
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if (payload.length >= 20) {
        Left(new RuntimeException("Payload too large"))
      } else {
        Right(s"request ($payload) has been accepted")
      }
    }
  }

  val errorOrResponse: ErrorOr[String] = for {
    conn <- ErrorOrHttpService.getConnection(config)
    response <- ErrorOrHttpService.issueRequest(conn, "Hi ErrorOr server")
  } yield response

  def main(args: Array[String]): Unit = {
    import cats.instances.option._
    println(getResponse(OptionHttpService, "Hello option"))
    println(getResponse(ErrorOrHttpService, "HelloErrorOrHttp"))
  }
}
