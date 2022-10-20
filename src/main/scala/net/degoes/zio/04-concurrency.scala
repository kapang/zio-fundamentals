package net.degoes.zio

import zio._

object ForkJoin extends ZIOAppDefault {

  val printer =
    Console.printLine(".").repeat(Schedule.recurs(10))

  /**
   * EXERCISE
   *
   * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then
   * print out a message, "Forked", then join the fiber using `Fiber#join`,
   * and finally, print out a message "Joined".
   */
  val run =
    for {
      fiber <- printer.fork // now we have 2 fibers
      _ <- Console.printLine("Forked")
      _ <- fiber.join // blocks until both fibers are completed
      _ <- Console.printLine("Joined")
    } yield ()
}

object ForkInterrupt extends ZIOAppDefault {

  val infinitePrinter =
    Console.printLine(".").forever

  /**
   * EXERCISE
   *
   * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then
   * print out a message, "Forked", then using `ZIO.sleep`, sleep for 100
   * milliseconds, then interrupt the fiber using `Fiber#interrupt`, and
   * finally, print out a message "Interrupted".
   */
  val run =
    //(infinitePrinter *> ZIO.sleep(10.millis))
    for {
      fiber <- infinitePrinter.fork
      _ <- Console.printLine("Forked")
      _ <- ZIO.sleep(10.millis) *> fiber.interrupt // dont call join, cuz otherwise will never complete since ur printing forever and next line will never call
      _ <- Console.printLine("Interrupted")
    } yield ()
}

// TODO: watch vid
object ParallelFib extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Rewrite this implementation to compute nth fibonacci number in parallel.
   */
  def fib(n: Int): UIO[BigInt] = {
    def loop(n: Int, original: Int): UIO[BigInt] =
      if (n <= 1) ZIO.succeed(n)
      else
        ZIO.suspendSucceed {
          loop(n - 1, original).zipWith(loop(n - 2, original))(_ + _)
        }

    loop(n, n)
  }

  val run =
    (for {
      _ <- Console.printLine(
            "What number of the fibonacci sequence should we calculate?"
          )
      n <- Console.readLine.orDie.flatMap(input => ZIO.attempt(input.toInt)).eventually
      f <- fib(n)
      _ <- Console.printLine(s"fib(${n}) = ${f}")
    } yield ())
}

object TimeoutExample extends ZIOAppDefault {
  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n)
    else
      ZIO.suspendSucceed {
        fib(n - 1).zipWith(fib(n - 2))(_ + _)
      }

  /**
   * EXERCISE
   *
   * Use `ZIO#timeout` to add a timeout to the following code so that
   * it doesn't run for more than 10 milliseconds.
   *
   * Print out a message if it timed out.
   */
  lazy val run = fib(20).timeout(10.millis) // timeout under the hood uses a separate fiber
}

object RaceExample extends ZIOAppDefault {
  def loadFromCache: Task[String] =
    ZIO.succeed("Loaded from cache!").delay(1.second)

  def loadFromDB: Task[String] =
    ZIO.succeed("Loaded from DB!").delay(500.millis)

  /**
   * EXERCISE
   *
   * Use `ZIO#race` to race the preceding two tasks and print out the
   * winning success value.
   *
   */
  lazy val run = loadFromCache race loadFromDB 
  // db will complete first since delay is shorter; it will interrupt the other one and not leave it running
  // if one fails, the other one will continue on still to race
}

object AlarmAppImproved extends ZIOAppDefault {

  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Any, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .attempt(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback = Console.printLine("You didn't enter a number of seconds!") *> getAlarmDuration

    for {
      _        <- Console.printLine("Please enter the number of seconds to sleep: ")
      input    <- Console.readLine
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /**
   * EXERCISE
   *
   * Create a program that asks the user for a number of seconds to sleep,
   * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
   * prints a dot every second that the alarm is sleeping for, and then
   * prints out a wakeup alarm message, like "Time to wakeup!!!".
   */
  val printer = Console.printLine(".").delay(1.second).forever
  val run =
    for {
      sleepDur <- getAlarmDuration
      //_ <- ZIO.sleep(sleepDur) zipPar printer // zipPar lets u do it in parallel but it won't stop it since printer is inf and it requires
      // both to finish; it gets both success params
      _ <- ZIO.sleep(sleepDur) race printer 
      _ <- Console.printLine("Time to wakeup!!!")
    } yield ()

    // alternative solution
    // for {
    //   sleepDur <- getAlarmDuration
    //   fiber <- printer.fork
    //   _ <- ZIO.sleep(sleepDur) *> fiber.interrupt // interrupt it after sleep is done
    //   _ <- Console.printLine("Time to wakeup!!!")
    // } yield ()
}

object ParallelZip extends ZIOAppDefault {

  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n)
    else
      ZIO.suspendSucceed {
        (fib(n - 1) zipWith fib(n - 2))(_ + _)
      }

  /**
   * EXERCISE
   *
   * Compute fib(10) and fib(13) in parallel using `ZIO#zipPar`, and display
   * the result.
   */
  val run =
    fib(10) zipPar fib(13)
}

/**
 * The Ref data type is a way for ZIO effects to utilize state. It is basically
 * a concurrent-safe version of Scala's own `var`, but integrated into ZIO.
 */
object RefExample extends ZIOAppDefault {
  import zio.Random._

  import zio.Clock._
  import zio.stm._

  /**
   * Some state to keep track of all points inside a circle,
   * and total number of points.
   */
  final case class PiState(
    inside: Ref[Long], // normally use var here w/o Ref
    total: Ref[Long]
  )

  /**
   * A function to estimate pi.
   */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
   * A helper function that determines if a point lies in
   * a circle of 1 radius.
   */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
   * An effect that computes a random (x, y) point.
   */
  val randomPoint: ZIO[Any, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /**
   * EXERCISE
   *
   * Using `Ref#update`, make a function that adds a point into the state.
   * If the point is inside the circle then increment `PiState#inside`. In any
   * case, increment `PiState#total`.
   */
  def addPoint(point: (Double, Double), piState: PiState): UIO[Unit] = 
    // alternative solution to use when
    //piState.inside.update(_ + 1).when(insideCircle(point._1, point._2)) *> piState.inside.update(_ + 1)
    
    for {
      _ <- if (insideCircle(point._1, point._2)) piState.inside.update(_ + 1) else ZIO.unit // it internally stores a long, so increment that counter
      _ <- piState.total.update(_ + 1)
      //_ <- piState.total.get.flatMap(v => piState.total.set(v + 1)) // this is not the same as above since they are not done atomically
      // 2 fibers, so we'll have inconsistent states, hence why you should use update since it's atomic
    } yield ()

  /**
   * EXERCISE
   *
   * Build a multi-fiber program that estimates the value of `pi`. Print out
   * ongoing estimates continuously until the estimation is complete.
   */
   def printer(piState: PiState) = //: IO[IOException, Nothing] = 
    (for {
      inside <- piState.inside.get
      total <- piState.total.get
      //pi = estimatePi(inside, total) // this is not a zio effect
      pi <- ZIO.succeed(estimatePi(inside, total))
      _ <- Console.printLine(s"Current val of PI: $pi")
    } yield ()).forever

  def worker(piState: PiState) =
    (for {
      point <- randomPoint
      _  <- addPoint(point, piState)
    } yield ()).forever

  val run =
   
    for {
      // inside <- Ref.make(0L)
      // total <- Ref.make(0L)
      //piState = PiState(inside, total)
      // more elgantly as
      piState <- Ref.make(0L).zipWith(Ref.make(0L))(PiState(_, _))
      fiber <- printer(piState).fork

      // Option 1: using your own fibers
      //_ <- ZIO.foreach(1 to 10) (_ => worker(piState).fork) // each worker needs to be separate fiber
      // can also try List.fill(10)(1) instead of range 1 to 10

      // Option 2: this will create # fibers for ur machine which may not actually be 10
      //_ <- ZIO.foreachPar(1 to 10) (_ => worker(piState)) 

      // ZIO2: how to tweak it, specify it below can also be parallelUnbound for infinite fibers used
       _ <- ZIO.foreachPar(1 to 10) (_ => worker(piState)) @@ ZIOAspect.parallel(10)
      _ <- fiber.join
    } yield ()
}

object PromiseExample extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Do some computation that produces an integer. When yare done, complete
   * the promise with `Promise#succeed`.
   */
  def doCompute(result: Promise[Nothing, Int]): UIO[Unit] = 
    // ZIO.succeed(50).flatMap(result.succeed(_)).unit // add .unit cuz we don't care about the boolean returned to match the return type
    ZIO.succeed(50).intoPromise(result).unit
    //intoPromise was switched out from flatMap

  /**
   * EXERCISE
   *
   * Fork the above computation in a separate fiber, giving it a promise
   * that it can use, and then wait for the promise to be completed,
   * using `Promise#await`.
   */
  lazy val waitForCompute: ZIO[Any, Nothing, Unit] = 
    for {
      promise <- Promise.make[Nothing, Int]
      _ <- doCompute(promise).fork
      _ <- promise.await // this lets you wait for the promise to finish
    } yield ()

  val run =
    waitForCompute
}

object FiberRefExample extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Make the child increment the ref and see how the output of the program
   * changes.
   */
  def makeChild(ref: FiberRef[Int]) =
    for {
      _ <- ref.get.debug("child initial value")
      _ <- ref.update(_ + 1)
      _ <- ref.get.debug("child after update")
    } yield ()

  val run =
    for {
      ref   <- FiberRef.make[Int](0, identity(_), _ + _)
      _     <- ref.get.debug("parent before fork") // good for debugging in the fiber
      child <- makeChild(ref).fork
      _     <- ref.get.debug("parent after fork")
      _     <- child.join
      _     <- ref.get.debug("parent after join")
    } yield ()
}
