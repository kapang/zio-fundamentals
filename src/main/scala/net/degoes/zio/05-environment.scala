package net.degoes.zio

import zio._

/**
 * ZIO environment is a type-indexed map that allows you to store a number of
 * objects of different types. ZIO calls these objects "services", because
 * they contain bundles of functionality consumed your application.
 */
object TypeIndexedMap extends ZIOAppDefault {
  trait Logging
  object Logging extends Logging

  trait Database
  object Database extends Database

  trait Cache
  object Cache extends Cache

  //val envLogging = ZEnvironment(Logging: Logging) // instead of Logging.type it let it infer or add a return type for envLogging
  val envLogging: ZEnvironment[Logging] = ZEnvironment(Logging) // instead of Logging.type it let it infer or add a return type for envLogging

  val envDatabase = ZEnvironment(Database: Database)

  val envCache = ZEnvironment(Cache: Cache)

  /**
   * EXERCISE
   *
   * Using the `++` operator on `ZEnvironment`, combine the three maps
   * (`envLogging`, `envDatabase`, and `envCache`) into a single map that
   * has all three objects.
   */
  val allThree: ZEnvironment[Database with Cache with Logging] = 
    envLogging ++ envDatabase ++ envCache

  /**
   * EXERCISE
   *
   * Using `ZEnvironment#get`, which can retrieve an object stored in
   * the map, retrieve the logging, database, and cache objects from
   * `allThree`. Note that you will have to specify the type parameter,
   * as it cannot be inferred (the map needs to know which of the objects
   * you want to retrieve, and that can be specified only by type).
   */
  lazy val logging  = allThree.get[Logging]
  lazy val database = allThree.get[Database]
  lazy val cache    = allThree.get[Cache]

  val run = ???
}

object AccessEnvironment extends ZIOAppDefault {

  final case class Config(host: String, port: Int)


  // in zio 1, zio 2 just need to use ZEnvironment under the hood
  // val z2: ZIO[Has[HttpClient] with Has[DbClient] with Has[ConfigService], Nothing, String] = ??? 

  // // in zio 2
  // val z: ZIO[HttpClient with DbClient with ConfigService, Nothing, String] = ???

  /**
   * EXERCISE
   *
   * Using `ZIO.service`, access a `Config` service from the environment, and
   * extract the `host` field from it.
   */
   // ZEnvironment[R] => Either[E, A]
  val accessHost: ZIO[Config, Nothing, String] = 
    // for {
    //   config <- ZIO.service[Config]
    // } yield config.host
    // don't need for comprehnsion
    ZIO.service[Config].map(_.host)


  /**
   * EXERCISE
   *
   * Using `ZIO.serviceWith`, access a `Config` service from the environment, and
   * extract the `port` field from it.
   */
   // this is the short hand of the above
  val accessPort: ZIO[Config, Nothing, Int] = ZIO.serviceWith[Config](_.port)

  /**
   * EXERCISE
   *
   * Using `ZIO.serviceWithZIO`, access a `Config` service from the environment, and
   * print the `port` field from it.
   */
  val printPort: ZIO[Config, Nothing, Unit] = 
    ZIO.serviceWithZIO[Config](config => Console.printLine(config.port).orDie)
    // serviceWithZIO lets you execute it as  a zio  effect

  val run = {
    val config = Config("localhost", 7878)

    (for {
      host <- accessHost
      port <- accessPort
      _    <- Console.printLine(s"Configuration: ${host}:${port}")
    } yield ()).provideEnvironment(ZEnvironment(config))
  }
}

object ProvideEnvironment extends ZIOAppDefault {

  final case class Config(server: String, port: Int)

  final case class DatabaseConnection() {
    def query(query: String): Task[Int] = ZIO.attempt(42)
  }

  val getServer: ZIO[Config, Nothing, String] =
    ZIO.service[Config].map(_.server)

  val useDatabaseConnection: ZIO[DatabaseConnection, Throwable, Int] =
    ZIO.serviceWithZIO[DatabaseConnection](_.query("SELECT * FROM USERS"))

  /**
   * EXERCISE
   *
   * Compose both the `getServer` and `useDatabaseConnection` effects together
   * and run them.
   * In order to do this successfully, you will have to use
   * `ZIO#provideEnvironment` to give them the environment that they need in
   * order to run.
   */
  val run = {
    val config = Config("localhost", 7878)

    (getServer zip useDatabaseConnection)
    // requires both zenv, since getServer reqs config and database conn requires db connection
      .provideEnvironment(ZEnvironment(Config("test-server", 8080)) ++ ZEnvironment(DatabaseConnection())
    )
  }
}

/**
 * In ZIO, layers are values that contain construction logic for services in
 * your  application. Services provide functionality like persistence or
 * logging or authentication, and they are used by business logic.
 *
 * A layer is a lot like a constructor, but may have complex initialization
 * or finalization, or may produce more than one service.
 *
 * ZIO has compile-time, type-safe wiring up of layers, which allows you to
 * optionally use ZIO for dependency-injection. The wire-up of layers
 * is done in a resource-safe, failure-aware way, with maximum parallelism
 * to decrease application startup time.
 *
 * Layers bring more power and compositionality to constructors. Although you
 * don't have to make your own layers to benefit from ZIO, layers can make
 * it easier and safer to assemble applications out of modules.
 */
object LayerEnvironment extends ZIOAppDefault {

  import java.io.IOException

  type MyFx = Logging with Files

  trait Files {
    def read(file: String): IO[IOException, String]
    // def read(file: String): ZIO[Config, IOException, String] 
    // in general not a good idea to pass a zenv (e.g. Config) here
    // it makes it more complicated and you're also leaking impl detail since this is a interface
    // esp if there are multiple of this then it becomes complicated
  }
  object Files {

    /**
     * EXERCISE
     *
     * Create a mock implementation of the `Files` service.
     */
    // type FilesMock
    final case class FilesMock() extends Files {
      override def read(file: String): IO[IOException, String] = ZIO.succeed("contents")
    }

    object FilesMock {
      /**
       * EXERCISE
       *
       * Using `ZLayer.succeed`, create a layer that provides a `FilesMock`
       */
      val layer: ZLayer[Any, Nothing, Files] = //ULayer[Files] = 
        ZLayer.succeed(FilesMock())
      // use the trait not the type FilesMock - similar to OOP priniciple
    }


  }

  trait Printer {
    def print(line: String): UIO[Unit]
  } //TODO check vid, not sure why this wasn't gone over
  object Printer {

    /**
      * EXERCISE
      *
      * Create a live implementation of the `Logging` service that requires `Printer`
      * and uses the printer to print logs.
      */
    // type LoggingLive
    final case class PrinterLive() extends Printer {
      override def print(line: String): UIO[Unit] = Console.printLine(line).orDie
    }
    object PrinterLive {

      /**
        * EXERCISE
        *
        * Using `ZLayer.fromFunction`, create a layer that provides a `LoggingLive`
        */
      val layer: ZLayer[Any, Nothing, Printer] = //URLayer[Printer, Logging]
        ZLayer.succeed(PrinterLive())
    }
  }

  trait Logging {
    def log(line: String): UIO[Unit]
  }
  object Logging {

    /**
     * EXERCISE
     *
     * Create a live implementation of the `Logging` service that requires `Printer`
     * and uses the printer to print logs.
     */
    // type LoggingLive
    final case class LoggingLive(printer: Printer) extends Logging {
      override def log(line: String): UIO[Unit] = printer.print(s"LOG: $line")
    }
    object LoggingLive {

      /**
       * EXERCISE
       *
       * Using `ZLayer.fromFunction`, create a layer that provides a `LoggingLive`
       */
      val layer: ZLayer[Printer, Nothing, Logging] = //URLayer[Printer, Logging] 
        //ZLayer.succeed(LoggingLive()) we can't do this because we don't nhave a printer,
        
        // use fromFunction instead
        //ZLayer.fromFunction(printer => LoggingLive(printer))
        // in zio 1, we used (LoggingLive(_)).toLayer
        ZLayer.fromFunction(LoggingLive(_))
    }
  }

  /**
   * EXERCISE
   *
   * Discover the inferred type of `effect`, and write it out explicitly.
   */
  val effect: ZIO[Files with Logging, IOException, Unit] =
    for {
      files   <- ZIO.service[Files]
      logging <- ZIO.service[Logging]
      file    <- files.read("build.sbt")
      _       <- logging.log(file)
    } yield ()

  val run = {

    /**
     * EXERCISE
     *
     * Create a layer using `ZLayer.make` and specifying all the pieces that go into the layer.
     */
    val fullLayer: ULayer[Files with Logging] = // ZLayer.make[Files with Logging](???)
      ZLayer.make[Files with Logging](Files.FilesMock.layer, Logging.LoggingLive.layer, Printer.PrinterLive.layer)

    /**
     * EXERCISE
     *
     * Using `ZIO#provideEnvironment`, provide `fullLayer` into `effect1` to remove its dependencies.
     */
    val effect1: ZIO[Files with Logging, IOException, Unit] = ???

    // zlayer describes how to build an env, for demo only on how it works, effect2 is the real way to do it
    for {
      zenv <- fullLayer.build
    } yield effect1.provideEnvironment(zenv)

    /**
     * EXERCISE
     *
     * Using `ZIO#provideLayer`, provide `fullLayer` into `effect2` to remove its dependencies.
     */
    val effect2: ZIO[Files with Logging, IOException, Unit] = ???

    effect2.provideLayer(fullLayer)

    /**
     * EXERCISE
     *
     * Using `ZIO#provide`, provide into the effect all the individual required layers to remove its dependencies.
     */
    val effect3: ZIO[Files with Logging, IOException, Unit] = ???

    //effect3.provide(Files.FilesMock.layer, Logging.LoggingLive.layer, Printer.PrinterLive.layer)

    
    // if you add ZLayer.Debug.tree you can get a dependency tree
    // or you can do ZLayer.Debug.mermaid and it will generate a mermaid depedency graph in the brwoser for you
    effect3.provide(Files.FilesMock.layer, Logging.LoggingLive.layer, Printer.PrinterLive.layer, ZLayer.Debug.tree)
  }
}
