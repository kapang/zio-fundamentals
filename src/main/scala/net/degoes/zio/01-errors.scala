package net.degoes.zio

import zio._
import java.io.IOException

/*
 * INTRODUCTION
 *
 * ZIO effects model failure, in a way similar to the Scala data types `Try`
 * and `Either`. Unlike exceptions, ZIO effects are statically-typed, allowing
 * you to determine if and how effects fail by looking at type signatures.
 *
 * ZIO effects have a large number of error-related operators to transform
 * and combine effects. Some of these "catch" errors, while others transform
 * them, and still others combine potentially failing effects with fallback
 * effects.
 *
 * In this section, you will learn about all these operators, as well as the
 * rich underlying model of errors that ZIO uses internally.
 */

object ErrorConstructor extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `ZIO.fail`, construct an effect that models failure with any
   * string value, such as "Uh oh!". Explain the type signature of the
   * effect.
   */
  val failed: ZIO[Any, String, Nothing] = ZIO.fail("Uh oh!") // it can be anything inside is fine for error channel, but it's String here

  val run =
    failed.foldZIO(Console.printLine(_), Console.printLine(_))
}

object ErrorRecoveryOrElse extends ZIOAppDefault {

  val failed = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#orElse` have the `run` function compose the preceding `failed`
   * effect with another effect.
   */
  val run = // ZIO[Any, Nothing, Int] it says it cannot fail, because the fall back always suceeds
    failed.orElse(ZIO.succeed(100)) //orElse lets you have a fail back if it fails; if first effect succeeds, then else is not executed
    //ex.
    //failed.orElse(Console.printLine("Hello")) // this will be IOException because now the fallback can fail w/ IOException
}

object ErrorShortCircuit extends ZIOAppDefault {
  val failed: ZIO[Any, Any, Unit] = // compiler looks for least upper bound, only common beetween String and IOException is Any
    for {
      _ <- Console.printLine("About to fail...")
      _ <- ZIO.fail("Uh oh!")
      _ <- Console.printLine("This will NEVER be printed!")
    } yield ()

  /**
   * EXERCISE
   *
   * Using `ZIO#orElse`, compose the `failed` effect with another effect that
   * succeeds with an exit code.
   */
  val run =
    failed.orElse(ZIO.succeed(ExitCode.success)) // zio exit code same as scala exit code
}

object ErrorRecoveryFold extends ZIOAppDefault {

  val failed = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#fold`, map both failure and success values of `failed` into
   * the unit value.
   */
  val run = //[IO, String]
    failed.fold(_ => (), _ => ()) // both success and fail is unit here
}

object ErrorRecoveryCatchAll extends ZIOAppDefault {

  val failed: ZIO[Any, String, Nothing] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#catchAll`, catch all errors in `failed` and print them out to
   * the console using `Console.printLine`.
   */
  val run = // ZIO[Any, Nothing, String]
    //failed.catchAll(err => Console.printLine(err))
    failed.catchAll(Console.printLine(_))
}

object ErrorRecoveryFoldZIO extends ZIOAppDefault {

  val failed: ZIO[Any, String, String] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#foldZIO`, print out the success or failure value of `failed`
   * by using `Console.printLine`.
   */
  val run =
    failed.foldZIO(err => Console.printLine(err), success => Console.printLine(success)) // in zio1, it's .foldM()
    //failed.foldZIO(Console.printLine(_), Console.printLine(_))
}

object ErrorRecoveryEither extends ZIOAppDefault {

  val failed: ZIO[Any, String, Int] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#either`, surface the error of `failed` into the success
   * channel, and then map the `Either[String, Int]` into an exit code.
   */
  val run = ???
    // failed.either.map { either =>
    //   case Left(_) => ExitCode.failure
    //   case Right(_) => ExitCode.success
    // }
}

object ErrorRecoveryIgnore extends ZIOAppDefault {

  val failed: ZIO[Any, String, Int] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#ignore`, simply ignore the failure of `failed`.
   */
  val run =
    failed.ignore // this completely forgets abt it, it doesn't even move to irrecoverable err channel; it ignores both success and errors
    // it basically drops the return value
}

object ErrorRefinement1 extends ZIOAppDefault {
  import java.io.IOException
  import scala.io.StdIn.readLine

  val broadReadLine: IO[Throwable, String] = ZIO.attempt(scala.io.StdIn.readLine())

  /**
   * EXERCISE
   *
   * Using `ZIO#refineToOrDie`, narrow the error type of `broadReadLine` into
   * an `IOException`:
   */
  val myReadLine: IO[IOException, String] = broadReadLine.refineToOrDie[IOException]

  def myPrintLn(line: String): UIO[Unit] = ZIO.succeed(println(line))

  val run: IO[IOException, Unit] = // it will be throwable if we  just used broadReadLine
    (for {
      _    <- myPrintLn("What is your name?")
      name <- myReadLine
      _    <- myPrintLn(s"Good to meet you, ${name}!")
    } yield ())
}

object ErrorRefinement2 extends ZIOAppDefault {

  import java.io.IOException
  import java.util.concurrent.TimeUnit

  val broadReadLine: IO[Throwable, String] = ZIO.attempt(scala.io.StdIn.readLine())
  val myReadLine: IO[IOException, String] = broadReadLine.refineToOrDie[IOException]

  /**
   * EXERCISE
   *
   * Create an effect that will get a `Duration` from the user, by prompting
   * the user to enter a decimal number of seconds. Use `refineToOrDie` to
   * narrow the error type as necessary.
   */
  lazy val getAlarmDuration: ZIO[Any, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.attempt(
        Duration.fromSeconds(java.lang.Long.parseLong(input)) // this throws NumberFormatException
      ).refineToOrDie[NumberFormatException] // but attempt uses failure, so we can refine to above?


    def fallback(input: String): ZIO[Any, IOException, Duration] =
      Console.printLine(s"The input ${input} is not valid.") *> getAlarmDuration

    for {
      _        <- Console.printLine("Please enter the number of seconds to sleep: ")
      input    <- Console.readLine
      duration <- parseDuration(input) orElse fallback(input) // <> means orElse
    } yield duration
  }

  /**
   * EXERCISE
   *
   * Create a program that asks the user for a number of seconds to sleep,
   * sleeps the specified number of seconds using `ZIO.sleep(d)`, and then
   * prints out a wakeup alarm message, like "Time to wakeup!!!".
   */
  val run =
    for {
      userDuration <- getAlarmDuration
      _ <- ZIO.sleep(userDuration)
      _ <- Console.printLine("Time to wakeup!!!")
    } yield ()
    // what’s the difference in putting the console.printLine in the yield vs the last line in the for comprehension?

    
}

object ZIOFinally extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `ZIO#ensuring`, attach an effect to the `tickingBomb`
   * effect, which will be executed whether `tickingBomb` succeeds
   * or fails. Print out a message to the console saying "Executed".
   */
  //lazy val tickingBomb2 = tickingBomb.ensuring(Console.printLine("Executed").ignore) // since we don't care if its printed or not we can swallow the error
  lazy val tickingBomb2 = tickingBomb.ensuring(Console.printLine("Executed").orDie) // move it to the irrecoverable channel when it fails since it expects it not to fail in a recoverable way
  // also orDieWith(e => new Exception("Failed")) // alternative way to process the error before moving to irrecoverable ch

  /**
   * EXERCISE
   *
   * See if you can break the guarantee of `ZIO#ensuring` so that
   * "Executed" is not printed out.
   */
   // java equivalent to ensuring is try/finally
  val tickingBomb =
    ZIO.fail("Boom!").delay(1.second)

  val run = tickingBomb2
}

// The full ZIO mental model:
// ZEnvironment[R] => Either[Cause[E], A] 
// cause contains all history of errors recoverable and irrecoverable and interrupts
object SequentialCause extends ZIOAppDefault {

  val failed1 = Cause.fail("Uh oh 1")
  val failed2 = Cause.fail("Uh oh 2")

  /**
   * EXERCISE
   *
   * Using `Cause.++`, form a sequential cause by composing `failed1`
   * and `failed2`.
   */
  lazy val composed = failed1 ++ failed2

  /**
   * EXERCISE
   *
   * Using `Cause.prettyPrint`, dump out `composed` to the console.
   */
  val run =
    Console.printLine(composed.prettyPrint) // prints it out prettily
}

object ParalellCause extends ZIOAppDefault {

  val failed1 = Cause.fail("Uh oh 1")
  val failed2 = Cause.fail("Uh oh 2")

  /**
   * EXERCISE
   *
   * Using `Cause.&&`, form a parallel cause by composing `failed1`
   * and `failed2`.
   */
  lazy val composed = failed1 && failed2

  /**
   * EXERCISE
   *
   * Using `Cause.prettyPrint`, dump out `composed` to the console.
   */
  val run =
    Console.printLine(composed.prettyPrint)
}

object Sandbox extends ZIOAppDefault {

  val failed1    = ZIO.fail("Uh oh 1")
  val failed2    = ZIO.fail("Uh oh 2")
  val finalizer1 = ZIO.fail(new Exception("Finalizing 1!")).orDie
  val finalizer2 = ZIO.die(new Exception("Finalizing 2!"))

  val composed = ZIO.uninterruptible {
    (failed1 ensuring finalizer1) zipPar (failed2 ensuring finalizer2)
    // other method is race 
  }

  /**
   * EXERCISE
   *
   * Using `ZIO#sandbox`, sandbox the `composed` effect and print out the
   * resulting `Cause` value to the console using `Console.printLine`.
   */
  val run =
    // this moves the err from irrecoverable to recoverable channel
    // map/flatmap is for success vs catchAll for error channels
    // sees all the errors
    // composed.sandbox.catchAll {
    //   cause => Console.printLine(cause.prettyPrint)
    // }
    // shortcut, can just use catchAllCause instead of 
    composed.catchAllCause {
      cause => Console.printLine(cause.prettyPrint)
    }
    // if we did not use sandbox, we would only see one of the recoverable error "Uh oh 1" instead of the full trace 
    // composed.catchAll {
    //   cause => Console.printLine(cause.prettyPrint)
    // }
}
