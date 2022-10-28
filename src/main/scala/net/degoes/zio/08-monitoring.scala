package net.degoes.zio

import zio._
import zio.metrics.Metric

object SimpleLogging extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Add logging using `ZIO.log` around each update of the ref.
   */
  val program =
    for {
      ref   <- Ref.make(0)
      _     <- ZIO.foreachParDiscard(0 to 10)(i => ZIO.log("Updating ref") *> ref.update(_ + i)) 
      // can also use ZIO.logInfo or ZIO.logError w/ specific lvl, default is info if not specified
      //alternatively can use this at the end instead do zio.log
      //@@ ZIOAspect.logged("")
      value <- ref.get
    } yield value

  /**
   * EXERCISE
   *
   * Surround `program` in `LogLevel.Error` to change its log level.
   */
  val program2: ZIO[Any, Nothing, Int] = LogLevel.Error(program)

  val run = program *> program2
}

object LogSpan extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `ZIO.logSpan` add a log span of "createUser" to the whole function.
   */
   // log span let yous add the label for the logs and lets you have 1 format easily instead of manually adding it:
  // ZIO.log(s"[http-client] Creating user ${userName}")
  // ZIO.log(s"[db-client] Creating user ${userName}")
  def createUser(userName: String, passHash: String, salt: Long): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.log(s"Creating user ${userName}")
    } yield ()

  

  val run =
    //createUser("sherlockholmes", "jkdf67sf6", 21381L) *>
    createUser("sherlockholmes", "jkdf67sf6", 21381L) *>
      ZIO.logSpan("createUser")(createUser("sherlockholmes", "jkdf67sf6", 21381L))
}

object CounterExample extends ZIOAppDefault {
  final case class Request(body: String)
  final case class Response(body: String)

  /**
   * EXERCISE
   *
   * Use the constructors in `Metric` to make a counter metric that accepts
   * integers as input.
   */
  lazy val requestCounter: Metric.Counter[Int] = 
    //Metric.counter("request").contramap(_.toLong) // convert int to lonng
    Metric.counterInt("request") // or just use counterInt, also have counterDouble, etc

  /**
   * EXERCISE
   *
   * Use methods on the counter to increment the counter on every request.
   */
  def processRequest(request: Request): Task[Response] =
    requestCounter.incrementBy(1) *> ZIO.succeed(Response("OK"))

  /**
   * EXERCISE
   *
   * Use methods on the counter to print out its value.
   *
   * NOTE: In real applications you don't need to poll metrics because they
   * will be exported to monitoring systems.
   *
   */
  lazy val printCounter: ZIO[Any, Nothing, Unit] = 
    for {
      counter <- requestCounter.value
      _ <- Console.printLine(s"Counter value: ${counter.count}").orDie // since it redquires no failures, use orDie to move to unreccoverable channel
    } yield ()

  lazy val run = {
    val processor = processRequest(Request("input")).delay(100.millis).repeatN(99)
    val printer   = printCounter.schedule(Schedule.fixed(100.millis))

    processor.race(printer)
  }
}
