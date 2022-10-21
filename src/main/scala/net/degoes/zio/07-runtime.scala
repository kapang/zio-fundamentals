package net.degoes.zio

import zio._
import scala.concurrent.ExecutionContext
import java.io.IOException

object CustomRuntime {
  val defaultEnvironment  = ZEnvironment.empty
  val defaultRuntimeFlags = RuntimeFlags.default
  val defaultFiberRefs    = FiberRefs.empty

  final case class AppConfig(name: String)

  /**
   * EXERCISE
   *
   * Create a custom runtime that bundles a value of type `AppConfig` into the
   * environment.
   */
  lazy val customRuntime = Runtime(defaultEnvironment, defaultFiberRefs, defaultRuntimeFlags)

  val program: ZIO[AppConfig, IOException, Unit] =
    for {
      appConfig <- ZIO.service[AppConfig]
      _         <- Console.printLine(s"Application name is ${appConfig.name}")
      _         <- Console.printLine("What is your name?")
      name      <- Console.readLine
      _         <- Console.printLine(s"Hello, ${name}!")
    } yield ()

  /**
   * EXERCISE
   *
   * Using the `unsafe.run` method of the custom runtime you created,
   * execute the `program` effect above.
   *
   * NOTE: You will have to use `Unsafe.unsafe { implicit u => ... }`
   * or `Unsafe.unsafe { ... }` (Scala 3) in order to call `unsafe.run`.
   */
   // run() returns type Exit[E, A] Either[Cacuse[E], A]
  def main(args: Array[String]): Unit =
    Unsafe.unsafe(implicit u => customRuntime.unsafe.run(
      program.provideEnvironment(ZEnvironment(AppConfig("my app")))//.getOrThrow() TODO; why does this fail
    ))
    // intentinal added unsafe block to meka it easier to tell we're in an unaaeot
}

object RunToFutureExample {
  final case class AppConfig(name: String)

  lazy val runtime = Runtime.default

  val program: ZIO[AppConfig, IOException, Unit] =
    for {
      appConfig <- ZIO.service[AppConfig]
      _         <- Console.printLine(s"Application name is ${appConfig.name}")
      _         <- Console.printLine("What is your name?")
      name      <- Console.readLine
      _         <- Console.printLine(s"Hello, ${name}!")
    } yield ()

  /**
   * EXERCISE
   *
   * Using the `unsafe.runToFuture` method of the default runtime,
   * execute the `program` effect above.
   *
   * NOTE: You will have to use `Unsafe.unsafe { implicit u => ... }`
   * or `Unsafe.unsafe { ... }` (Scala 3) in order to call `unsafe.runToFuture`.
   */
  def main(args: Array[String]): Unit =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.runToFuture(
      program.provideEnvironment(ZEnvironment(AppConfig("my app")))
    )) // this is a CancelableFuture type, it start executing immediately but we didn't wait for it to complete
}

object ThreadPool extends ZIOAppDefault {

  lazy val dbPool: Executor = Executor.fromExecutionContext(ExecutionContext.global)

  /**
   * EXERCISE
   *
   * Using `ZIO#onExecutor`, write an `onDatabase` combinator that runs the
   * specified effect on the database thread pool.
   */
  def onDatabase[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = 
    zio.onExecutor(dbPool) // or onExecutionContext or ZIOAspect.onExecutor(dbPool)
    // zio @@ ZIOAspect.onExecutor(dbPool)
    // matter of preference

  /**
   * EXERCISE
   *
   * Implement a combinator to print out thread information before and after
   * executing the specified effect.
   */
  def threadLogged[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    val log = ZIO.succeed {
      val thread = Thread.currentThread()

      val id        = thread.getId()
      val name      = thread.getName()
      val groupName = thread.getThreadGroup().getName()

      println(s"Thread($id, $name, $groupName)")
    }

    log *> zio <* log // zip right then left to wrap logs w/ zio
    //zio
  }

  /**
   * EXERCISE
   *
   * Use the `threadLogged` combinator around different effects below to
   * determine which threads are executing which effects.
   */
  val run =
    (threadLogged(Console.printLine("Main")) *>
      onDatabase {
        threadLogged(Console.printLine("Database")) *>
          ZIO.blocking {
            threadLogged(Console.printLine("Blocking"))
          } *>
          threadLogged(Console.printLine("Database"))
      } *>
      threadLogged(Console.printLine("Main")))
}

object CustomLogger extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `ZLogger.simple`, create a logger that dumps text strings to the console
   * using `println`.
   */
  lazy val simpleLogger: ZLogger[String, Unit] = ZLogger.simple(s => println(s"LOG: $s"))
  // this is more of a front end logging
  // can also integrate w/ zio logging lib to configure w/ slf4j or log4j etc for backend logging

  /**
   * EXERCISE
   *
   * Create a layer that will install your simple logger using Runtime.addLogger.
   */
  lazy val withCustomLogger: ZLayer[Any, Nothing, Unit] = Runtime.addLogger(simpleLogger)

  /**
   * EXERCISE
   *
   * Using `ZIO#provide`, inject the custom logger into the following effect
   * and verify your logger is being used.
   */
  val run =
    //ZIO.log("Hello World!").provide(withCustomLogger)
    ZIO.log("Hello World!").provide(withCustomLogger, Runtime.removeDefaultLoggers)
    // optionally can also remove the default logger w/ 2nd param
}
