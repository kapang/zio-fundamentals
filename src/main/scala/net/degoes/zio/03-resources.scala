package net.degoes.zio

import zio._
import java.text.NumberFormat
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.io.Source

object Cat extends ZIOAppDefault {

  import java.io.IOException

  def getScalaSource(file: String): Source = Source.fromFile(file)

  /**
   * EXERCISE
   *
   * Using `ZIO.attemptBlocking`, implement a function to read a file on the
   * blocking thread pool, storing the result into a string. You may find
   * it helpful to use the `getScalaSource` helper method defined above.
   */
  def readFile(file: String): ZIO[Any, IOException, String] = // can also be ZIO[Blocking, IOException, String]
    //  ZIO.blocking {
    //   (for {
    //       source <- ZIO.attempt(getScalaSource(file))
    //       contents <- ZIO.attempt(source.mkString("\n"))
    //   } yield contents).refineToOrDie[IOException] // to convert Throwable more finely as IOException
    //  }
    // equivalent to
    // (for {
    //     source <- ZIO.attemptBlocking(getScalaSource(file))
    //     contents <- ZIO.attemptBlocking(source.mkString("\n"))
    // } yield contents).refineToOrDie[IOException] // to convert Throwable more finely as IOException
    // which is also equivalent to
    for {
        source <- ZIO.attemptBlockingIO(getScalaSource(file))
        contents <- ZIO.attemptBlockingIO(source.mkString("\n")) // auto convert Throwable more finely as IOException
    } yield contents 


  /**
   * EXERCISE
   *
   * Implement a version of the command-line utility "cat", which dumps the
   * contents of the specified file to standard output.
   */
  val run = ??? //TODO fix this
    // for {
    //   args <- getArgs // check that it has at least 1 arg
    //   contents <- if (args.nonEmpty) {     // zio effect by default that lets you rerad command line args to get a file name from the user
    //     readFile(args.head)
    //   } else Console.printLineError("Usage: cat <filename") *> ZIO.fail("Error") // need to add the error otherwise content is type Any
    //   _ <- Console.printLine(contents)
    // } yield ()

      
}

object CatEnsuring extends ZIOAppDefault {

  import java.io.IOException
  import scala.io.Source

  def open(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(Source.fromFile(file))

  def close(source: Source): ZIO[Any, IOException, Unit] =
    ZIO.attemptBlockingIO(source.close())

  /**
   * EXERCISE
   *
   * Using `ZIO#ensuring`, implement a safe version of `readFile` that cannot
   * fail to close the file, no matter what happens during reading.
   */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO.uninterruptible {
      for {
        source   <- open(file) // if file open fails, there's nothing to close
        contents <- ZIO.attempt(source.getLines().mkString("\n")).ensuring(close(source).orDie)
      } yield contents
    }.refineToOrDie[IOException]

  val run =
    for {
      args <- getArgs
      fileName <- ZIO
                   .fromOption(args.headOption)
                   .tapError(err => Console.printLine(s"You must specify a file name on the command line: $err")) 
                   // tapError lets u do something to the error channel or tapBoth allows u to talk to both regular and error channel
                   // same fiber; nice helper so that you dont have to do *> Console.printline ...
      contents <- readFile(fileName)
      _        <- Console.printLine(contents)
    } yield ()
}

object CatAcquireRelease extends ZIOAppDefault {

  import java.io.IOException
  import scala.io.Source

  def open(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(scala.io.Source.fromFile(file))

  def close(source: Source): ZIO[Any, IOException, Unit] =
    ZIO.attemptBlockingIO(source.close())

  /**
   * EXERCISE
   *
   * Using `ZIO#acquireReleaseWith`, implement a safe version of `readFile` that
   * cannot fail to close the file, no matter what happens during reading.
   */
  def readFile(file: String): ZIO[Any, IOException, String] = 
    ZIO.acquireReleaseWith(open(file))(source => close(source).orDie) { source =>
      ZIO.attemptBlockingIO(source.getLines().mkString("\n"))//.ensuring(close(source).orDie)  no longer need the ensuring w/ the acquire
    }

  val run =
    for {
      args <- getArgs
      fileName <- ZIO
                   .fromOption(args.headOption)
                   .tapError(_ => Console.printLine("You must specify a file name on the command line"))
      contents <- readFile(fileName)
      _        <- Console.printLine(contents)
    } yield ()

}

object SourceScoped extends ZIOAppDefault {

  import java.io.IOException

  import scala.io.Source

  final class ZSource private (private val source: Source) {
    def execute[T](f: Source => T): ZIO[Any, IOException, T] =
      ZIO.attemptBlockingIO(f(source))
  }
  object ZSource {

    /**
     * EXERCISE
     *
     * Use the `ZIO.acquireRelease` constructor to make a scoped effect that
     * succeeds with a `ZSource`, whose finalizer will close the opened resource.
     */
    def make(file: String): ZIO[Scope, IOException, ZSource] = {
      // An effect that acquires the resource:
      val open = ZIO.attemptBlockingIO(new ZSource(Source.fromFile(file)))

      // A function that, when given the resource, returns an effect that
      // releases the resource:
      val close: ZSource => ZIO[Any, Nothing, Unit] =
        _.execute(_.close()).orDie

      ZIO.acquireRelease(open)(close) // we are not using the resource, so no need for acquireReleaseWith
    
    }
  }

  /**
   * EXERCISE
   *
   * Using `ZSource.make`, as well as `ZIO.scoped` (to define the scope in
   * which resources are open), read the contents of the specified file into
   * a `String`.
   */
  def readFile(file: String): ZIO[Any, IOException, String] = // if we change it to ZIO[Scope, IOException, String] it will compile
    ZIO.scoped { 
      // ^ this is needed if you wish to remove the Scope to pass and just use Any as input; 
      // this will automatically close it using the all resources we defined to close in the make.close() above 
      for {
        zSource <- ZSource.make(file)
        contents <- zSource.execute(_.mkString("\n"))
      } yield contents
    }

  /**
   * EXERCISE
   *
   * Write an app that dumps the contents of the files specified by the
   * command-line arguments to standard out.
   */
  val run =
    ???
}

object CatIncremental extends ZIOAppDefault {

  import java.io.{ FileInputStream, IOException, InputStream }

  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Any, IOException, Unit] = ZIO.attemptBlockingIO(is.close())

    final def read: ZIO[Any, IOException, Option[Chunk[Byte]]] =
      ZIO.attemptBlockingIO {
        val array = Array.ofDim[Byte](1024)
        val len   = is.read(array)
        if (len < 0) None
        else Some(Chunk.fromArray(array).take(len))
      }
  }

  /**
   * EXERCISE
   *
   * Refactor `FileHandle` so that creating it returns a scoped effect, so
   * that it is impossible to forget to close an open handle.
   *
   * HINT: `ZIO.acquireRelease` is the easiest way to do this!
   */
  object FileHandle {
    final def open(file: String): ZIO[Any, IOException, FileHandle] =
      // before refactor
      //ZIO.attemptBlockingIO(new FileHandle(new FileInputStream(file)))
      ZIO.scoped {
        ZIO.acquireRelease(ZIO.attemptBlockingIO(new FileHandle(new FileInputStream(file))))(_.close.orDie)
      }
  }

  /**
   * EXERCISE
   *
   * Implement an incremental version of `cat` that pulls a chunk of bytes at
   * a time, stopping when there are no more chunks left.
   */
  def cat(fh: FileHandle): ZIO[Any, IOException, Unit] =
    for {
      someChunk <- fh.read
      _ <- someChunk match {
        case None => ZIO.unit //.succeed(())
        case Some(chunk) => Console.print(new String(chunk.toArray, StandardCharsets.UTF_8)) *> cat(fh) 
        // Console.print instead of printLine dont want to add additional new lines
        // recursively call cat to continue reading
      } 
    } yield ()

  /**
   * EXERCISE
   *
   * Implement an incremental version of the `cat` utility, using
   * `ZIO#acquireRelease` or `ZManaged` to ensure the file is closed in the
   * event of error or interruption.
   */
  val run =
    getArgs.map(_.toList).flatMap {
      case file :: Nil =>
        /**
         * EXERCISE
         *
         * Open the specified file, safely create and use a file handle to
         * incrementally dump the contents of the file to standard output.
         */
        // ZIO.scoped { // if you omit this here, ZIO will take care of calling the scoped for you if u dont need to manually close
          for {
            fh <- FileHandle.open(file)
            _ <- cat(fh)
          } yield ()
        // }

      case _ => Console.printLine("Usage: cat <file>")
    }
}

object AddFinalizer extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using `ZIO.addFinalizer`, which adds a finalizer to the currently open
   * scope, implement your own version of `ZIO.acquireRelease`. Note: The
   * version you implement need not be safe in the presence of interruption,
   * but it should be safe in the presence of errors.
   */
   // addFinalizer is very low lvl, very rare usage
  def acquireRelease[R, E, A](acquire: ZIO[R, E, A])(release: A => ZIO[R, Nothing, Any]): ZIO[R with Scope, E, A] = 
    for {
      a <- acquire
      _ <- ZIO.addFinalizer(release(a)).uninterruptible // this adds the scope here; that's how zio know how to close the resourrces to the scope
    } yield a

  val run =
    for {
      _ <- acquireRelease(Console.printLine("Acquired!"))(_ => Console.printLine("Released!").orDie)
      _ <- ZIO.fail("Uh oh!")
    } yield ()
}
