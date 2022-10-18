package net.degoes.zio

import zio._ // this is zio 2 so it's nice
// import zio.console._ //used to be for printline

/*
 * INTRODUCTION
 *
 * ZIO effects are immutable data values that model a possibly complex series
 * of async, concurrent, resourceful, and contextual computations.
 *
 * The only effect type in ZIO is called ZIO, and has three type parameters,
 * which permit accessing context from an environment (`R`), failing with a
 * value of a certain type (`E`), and succeeding with a value of a certain
 * type (`A`).
 *
 * Unlike Scala's Future, ZIO effects are completely lazy. All methods on ZIO
 * effects return new ZIO effects. No part of the workflow is executed until
 * one of the `unsafeRun*` functions are called.
 *
 * ZIO effects are transformed and combined using methods on the ZIO data type.
 * For example, two effects can be combined into a sequential workflow using
 * an operator called `zip`. Similarly, two effects can be combined into a
 * parallel workflow using an operator called `zipPar`.
 *
 * The operators on the ZIO data type allow very powerful, expressive, and
 * type-safe transformation and composition, while the methods in the ZIO
 * companion object allow building new effects from simple values (which are
 * not themselves effects).
 *
 * In this section, you will explore both the ZIO data model itself, as well
 * as the very basic operators used to transform and combine ZIO effects, as
 * well as a few simple ways to build effects.
 */

/**
 * A good mental model for ZIO[R, E, A] is:
 * {{{
 *   ZEnvironment[R] => Either[E, A]
 * }}}
 * This can be interpreted as a function which, given a ZIO environment
 * (which is a map that contain classes of different types), a ZIO
 * returns either a failure of type E or a success of type A.
 */
object ZIOModel {

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO companion object.
   */
  object ZIO {
    def succeed[A](success: => A): ZIO[Any, Nothing, A] = 
      ZIO(_ => Right(success))
      // regardless of what env, it's disgarded which is why it's Any
      //ZIO(env => Right(success))
      
    def fail[E](error: => E): ZIO[Any, E, Nothing] = 
      ZIO(_ => Left(error))

    // ZIO.effect in zio1, but ti's ZIO.attemp in zio2
    def attempt[A](code: => A): ZIO[Any, Throwable, A] = 
      ZIO(_ => 
        try { 
          Right(code) 
        } catch { 
          case e: Throwable => Left(e) // this works in this toy project
          //case e: NonFatal => Left(e) 
          // it only catches non fatal in zio, not all throwables, e.g. you never want to catch a outof memory ex
          // zio will just let it happen
          // Victor says - this is cool for lib, but for services here, throwable may make more sense?
          // Jorge says - we might still want to catch all trouble cuz we don't want to lose any errors, we want to make sure
          // we're logging all errors; in zio 2, u don't need to handle all errors all the time, they can be forgotten because
          // zio 2 automatically logs them - TMR
        }
      )
    // alternate station suggested by Vlad
    /*  
      ZIO(_ => 
        scala.util.Try(code).toEither
      )
    */

    def environment[R]: ZIO[R, Nothing, ZEnvironment[R]] = 
      ZIO(env => Right(env))
      // or simplified
      //ZIO(Right(_))
  }

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO class.
   */
  final case class ZIO[-R, +E, +A](run: ZEnvironment[R] => Either[E, A]) { self =>
    def map[B](f: A => B): ZIO[R, E, B] = ZIO(env => self.run(env) map f)

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO(
        env => self.run(env).map(f)
        .map(_.run(env))
        .flatten
      )


    def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
      self.flatMap(a => that.map(a -> _)) // to conver to zio

    def either: ZIO[R, Nothing, Either[E, A]] = 
      ZIO(r => Right(self.run(r))) // wrap w/ Right to return zio success

    def provide(r: ZEnvironment[R]): ZIO[Any, E, A] = 
      ZIO(_ => self.run(r)) // we can ignore the env and just use R

    def orDie(implicit ev: E <:< Throwable): ZIO[R, Nothing, A] =
      ZIO(r => self.run(r).fold(throw _, Right(_)))
      // intentionaly forgetting that we can fail, creating a description of throwing an ex, will be explained tmr
  }

  // thid doesn't actually call println just describing it
  def printLine(line: String): ZIO[Any, Nothing, Unit] =
    ZIO.attempt(println(line)).orDie

  val readLine: ZIO[Any, Nothing, String] =
    ZIO.attempt(scala.io.StdIn.readLine()).orDie

  def run[A](zio: ZIO[Any, Throwable, A])(implicit unsafe: Unsafe): A =
    zio.run(ZEnvironment.empty).fold(throw _, a => a)
    // this is calling the same run() func as above in ZIO class
    // not too much to worry about the unsafe, will be explained later


  /**
   * Run the following main function and compare the results with your
   * expectations.
   */
  def main(args: Array[String]): Unit =
    Unsafe.unsafe { implicit u =>
      run {
        printLine("Hello, what is your name?").flatMap(
          _ => readLine.flatMap(name => printLine(s"Your name is: ${name}"))
        )
      }
    }
}

object ZIOTypes {
  type ??? = Nothing

  /**
   * EXERCISE
   *
   * Provide definitions for the ZIO type aliases below.
   */
  type Task[+A]     = ZIO[Any, Throwable, A] // equivalent to Future, alwasy fails w/ throwable
  type UIO[+A]      = ZIO[Any, Nothing, A] // UIO - unexceptional , cannot fail
  type RIO[-R, +A]  = ZIO[R, Nothing, A] // RIO - requires an R env
  type IO[+E, +A]   = ZIO[Any, E, A] // IO no env 
  type URIO[-R, +A] = ZIO[R, Nothing, A] // no env, cuz theres no E in between the type and also the U prefix
}

object SuccessEffect extends ZIOAppDefault {
  // in zio 1
  // no need to return exit code
  // def run(args: List[String]): ZIO[Console with Random with System, Nothing, ExitCode] = ???

  /**
   * EXERCISE
   *
   * Using `ZIO.succeed`, create an effect that succeeds with the string
   * "Hello World".
   */
  val run =
    ZIO.succeed("Hello World") // very easy, it has a ZIO[Any, Nothing, Nothing] type return here
}

object HelloWorld extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement a simple "Hello World!" program by invoking `Console.printLine`
   * to create an effect that, when executed, will print out "Hello World!" to
   * the console.
   */
  // in zio 1: ZIO[Console, IOException, Unit]
  val run = //: IO[IOException, Unit]  =
    Console.printLine("Hello World!")
    // we're not returning anything, IO is more specific than TASK
    // in zio 1, it's putStrLn(), it came from haskell
}

object SimpleMap extends ZIOAppDefault {
  import Console.readLine

  /**
   * EXERCISE
   *
   * Using `ZIO#map`, map the string success value of `Console.readLine` into an
   * integer (the length of the string)`.
   */
  val run =
    Console.readLine.map(_.length) //getStrLn
}

object PrintSequenceZip extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `zip`, compose a sequence of `Console.printLine` effects to produce an effect
   * that prints three lines of text to the console.
   */
  val run = // ZIO[Any, IOException, Unit] is the ret type, 
    // new thing in zio 2
    Console.printLine("Line 1") zip // <*> is equivalent to zip
      Console.printLine("Line 2") zip
      Console.printLine("Line 3")
    
}

object PrintSequence extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), compose a sequence of `Console.printLine` effects to
   * produce an effect that prints three lines of text to the console.
   */
   // what if 2nd effect fails? then everything fials; it's failfast not fail safe no partial result
  val run =
    Console.printLine("Line 1") *> // this collects only the right result; also zipLeft <* is available
      Console.printLine("Line 2") *>
      Console.printLine("Line 3")
}

object PrintReadSequence extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), sequentially compose a `Console.printLine` effect, which
   * models printing out "Hit Enter to exit...", together with a `Console.readLine`
   * effect, which models reading a line of text from the console.
   */
  val run =
    // in this case can be zip or zipright
    // if you have them as separate, then the 1st value will be discarded, e.g.
    //Console.printLine("Hit Enter to exit...")
    //Console.readLine 
    Console.printLine("Hit Enter to exit...") *> Console.readLine 
}

object SimpleDuplication extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * In the following program, the expression `Console.printLine("Hello again")`
   * appears three times. Factor out this duplication by introducing a new
   * value that stores the expression, and then referencing that variable
   * three times.
   */
  val printHelloAgain = Console.printLine("Hello again")
   
  val run = {
    // Console.printLine("Hello") *>
    //   Console.printLine("Hello again") *>
    //   Console.printLine("Hello again") *>
    //   Console.printLine("Hello again")
    Console.printLine("Hello") *>
      printHelloAgain *>
      printHelloAgain *>
      printHelloAgain
  }
}

object FlatMap extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * The following program is intended to ask the user for their name, then
   * read their name, then print their name back out to the user. However,
   * the `zipRight` (`*>`) operator is not powerful enough to solve this
   * problem, because it does not allow a _subsequent_ effect to depend
   * on the success value produced by a _preceding_ effect.
   *
   * Solve this problem by using the `ZIO#flatMap` operator, which composes
   * a first effect together with a "callback", which can return a second
   * effect that depends on the success value produced by the first effect.
   */
  val run =
    Console.printLine("What is your name?") *>
      Console.readLine *> // Use .flatMap(...) here
      Console.printLine("Your name is: ")
}

object PromptName extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * The following program uses a combination of `zipRight` (`*>`), and
   * `flatMap`. However, this makes the structure of the program harder
   * to understand. Replace all `zipRight` by `flatMap`, by ignoring the
   * success value of the left hand effect.
   */
  val run =
    //Console.printLine("What is your name?") *>
    //  Console.readLine.flatMap(name => Console.printLine(s"Your name is: ${name}"))
    
    Console.printLine("What is your name?").flatMap(_ =>
      Console.readLine.flatMap(name => Console.printLine(s"Your name is: ${name}"))
    )

  /**
   * EXERCISE
   *
   * Implement a generic "zipRight" that sequentially composes the two effects
   * using `flatMap`, but which succeeds with the success value of the effect
   * on the right-hand side.
   */
  def myZipRight[R, E, A, B](
    left: ZIO[R, E, A],
    right: ZIO[R, E, B]
  ): ZIO[R, E, B] =
    left.flatMap(_ => right) // _ means u discard the resutl of the first one here
}

object ForComprehension extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Rewrite the following program to use a `for` comprehension.
   */
  val run =
    // Console
    //   .printLine("What is your name?")
    //   .flatMap(
    //     _ => Console.readLine.flatMap(name => Console.printLine(s"Your name is: ${name}"))
    //   )
    for {
      _ <- Console.printLine("What is your name?")
      name <- Console.readLine
      _ <- Console.printLine(s"Your name is: ${name}") // only last line is map, all other ones are flatmap
      // if you use name = Console.readLine,  this just assign it to a local var inside of the flatmap here, not a map
    } yield ()
    // this is good esp for beginning zio, it's more natural and reads nicer
}

object ForComprehensionBackward extends ZIOAppDefault {

  val readInt = Console.readLine.flatMap(string => ZIO.attempt(string.toInt)).orDie

  /**
   * EXERCISE
   *
   * Rewrite the following program, which uses a `for` comprehension, to use
   * explicit `flatMap` and `map` methods. Note: each line of the `for`
   * comprehension will translate to a `flatMap`, except the final line,
   * which will translate to a `map`.
   */
  val run = {
    // for {
    //   _   <- Console.printLine("How old are you?")
    //   age <- readInt
    //   _ <- if (age < 18) Console.printLine("You are a kid!")
    //       else Console.printLine("You are all grown up!")
    // } yield ()
    Console.printLine("How old are you?").flatMap {
      _ => readInt
      .flatMap { age =>
        (if (age < 18) Console.printLine("You are a kid!") else Console.printLine("You are all grown up!")).
          map(_ => ()) // map to a unit since yield (), this is not actually necessary to write here but it's how it's transalted
      }
    }
    // can also transform the last one to map instead if we're not showing the direct translate
    // Console.printLine("How old are you?").flatMap {
    //   _ => readInt
    //   .map { age =>
    //     if (age < 18) Console.printLine("You are a kid!") else Console.printLine("You are all grown up!"))
    //   }
    // }
  }
}

object NumberGuesser extends ZIOAppDefault {
  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) Console.printLine("You guessed correctly!")
    else Console.printLine(s"You did not guess correctly. The answer was ${random}")

  /**
   * EXERCISE
   *
   * Choose a random number (using `Random.nextInt`), and then ask the user to guess
   * the number (using `Console.readLine`), feeding their response to `analyzeAnswer`,
   * above.
   */
   //side note, the imiplicit trace is used during compile time for system lvl tracing
  val run =
    // for {
    //   _ <- Console.printLine("Introduce your guess")
    //   random <- Random.nextInt // is RNG a side effect in zio? it's description only so no;
    //   guess <- Console.readLine 
    //   _ <- analyzeAnswer(random, guess)
    // } yield ()  
    // more elegantly: can consolidate into 2 lines  
    for {
      random <- Random.nextInt
      guess <- Console.printLine("Introduce your guess") *> Console.readLine 
      _ <- analyzeAnswer(random, guess)
    } yield ()
}

object SingleSyncInterop extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using ZIO.attempt, convert `println` into a ZIO function.
   */
  def myPrintLn(line: String): Task[Unit] = ZIO.attempt(println(line)) // encapsulates any scala code into zio effect (zio.apply in zio1)

  val run =
    myPrintLn("Hello World!")
}

object MultipleSyncInterop extends ZIOAppDefault {

  /**
   * Using `ZIO.attempt`, wrap Scala's `println` method to lazily convert it
   * into a functional effect, which describes the action of printing a line
   * of text to the console, but which does not actually perform the print.
   */
  def printLine(line: String): Task[Unit] = ZIO.attempt(println(line))

  /**
   * Using `ZIO.attempt`, wrap Scala's `scala.io.StdIn.readLine()` method to
   * lazily convert it into a ZIO effect, which describes the action of
   * printing a line of text to the console, but which does not actually
   * perform the print.
   */
  val readLine: Task[String] = ZIO.attempt(scala.io.StdIn.readLine())

  val run = {
    for {
      _    <- printLine("Hello, what is your name?")
      name <- readLine
      _    <- printLine(s"Good to meet you, ${name}!")
    } yield ()
  }
  // no diff for user, but the above is better
  // all side effects will still be handled by this
  // zio effects are interruptable, you can't interrupt each one in this case before the next executes which is important
  // which mean no context switching which means could be less performant. by thread interrupt we mean virtual thread interrupt - fiber
  // ZIO.attempt {
  //   println("Hello, what is your name?"
  //   val name = scala.ioStdIn.readLine()
  //   println(s"Good to meet you, ${name}!")
  // }
}

object AsyncExample extends ZIOAppDefault {
  import scala.concurrent.ExecutionContext.global

  // too many callback base
  def loadBodyAsync(onSuccess: String => Unit, onFailure: Throwable => Unit): Unit =
    global.execute { () =>
      if (scala.util.Random.nextDouble() < 0.01) onFailure(new java.io.IOException("Could not load body!"))
      else onSuccess("Body of request")
    }

  /**
   * EXERCISE
   *
   * Using `ZIO.async`, convert the above callback-based API into a
   * nice clean ZIO effect.
   */
  lazy val loadBodyAsyncZIO: ZIO[Any, Throwable, String] =
    ZIO.async(
      callback => loadBodyAsync(
        body => callback(ZIO.succeed(body)), 
        e => callback(ZIO.fail(e))
       )
    )

  val run =
    for {
      body <- loadBodyAsyncZIO
      _    <- Console.printLine(body)
    } yield ()
}

object FutureExample extends ZIOAppDefault {
  import scala.concurrent.Future

  trait User

  def getUser(userId: Long): Future[User] = ???

  /**
   * EXERCISE
   *
   * Using `ZIO.fromFuture`, convert the above Future-based API into a
   * ZIO-based one.
   */
  def getUserZIO(userId: Long): Task[User] = ???

  val run = ???
}
