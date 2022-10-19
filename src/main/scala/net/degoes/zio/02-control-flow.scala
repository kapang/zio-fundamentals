package net.degoes.zio

import zio._
import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Looping extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement a `repeat` combinator using `flatMap` (or `zipRight`) and recursion.
   */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    if (n <= 0) ZIO.succeed(Chunk.empty) // can create an empty chunk, succeed will create an effect
    else {
      for {
        a <- effect // runs the effect to obtain an 'a'
        chunk <- repeat(n - 1)(effect) // repeat the next effect
        // this is inside of a flatmap, so even if no map at the yield, it will not be tail recursive in scala, but in ZIO it is because
        // internally it creates its own heap and stack so it can still be effectively tail recursive in zio. stores a constant in heap space
      } yield a +: chunk // prepend operator for zio chunks also; yield returns a map at the end so it's not a tail recursive, just recursive here
    
      // alternative method instead of the for comprehension, but for comprehension is preferred since it's nicer to read
      //effect.zipWith(repeat(n - 1)(effect))((a, chunk) => a +: chunk)
      //effect.zipWith(repeat(n - 1)(effect))((_ +: _)
    }

  val run =
    repeat(100)(Console.printLine("All work and no play makes Jack a dull boy"))
}

object Interview extends ZIOAppDefault {
  import java.io.IOException

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `getAllAnswers` function in such a fashion that it will ask
   * the user each question and collect them all into a list.
   */
  def getAllAnswers(questions: List[String]): ZIO[Any, IOException, List[String]] =
    questions match {
      case Nil     => ZIO.succeed(List.empty)
      case q :: qs => {
        for {
          _ <- Console.printLine(q)
          ans <- Console.readLine
          answers <- getAllAnswers(qs)
        } yield ans :: answers
        
        // alternative but less readable answer
        //(Console.printLine(q *> Console.readLine).zipWith(getAllAnswers(qs)(_ :: _)))
        }
    }



  /**
   * EXERCISE
   *
   * Use the preceding `getAllAnswers` function, together with the predefined
   * `questions`, to ask the user a bunch of questions, and print the answers.
   */
  val run =
    for {
      answers <- getAllAnswers(questions)
      _ <- Console.printLine(answers)
    } yield ()
}

object InterviewGeneric extends ZIOAppDefault {
  import java.io.IOException

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `iterateAndCollect` function.
   */
   // same as above but more generic version
  def iterateAndCollect[R, E, A, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as match {
      case Nil     => ZIO.succeed(List.empty)
      case a :: as => 
        for {
          result <- f(a)
          results <- iterateAndCollect(as)(f)
        } yield result :: results
    }

  val run =
    for {
      answers <- iterateAndCollect(questions)(question => Console.printLine(question) *> Console.readLine)
      _ <- Console.printLine(answers)
    } yield ()
}

//  good news zio already has a method for iterateAndCollect, no need to implement this yourselves
object InterviewForeach extends ZIOAppDefault {

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Using `ZIO.foreach`, iterate over each question in `questions`, print the
   * question to the user (`Console.printLine`), read the answer from the user
   * (`Console.readLine`), and collect all answers into a collection. Finally, print
   * out the contents of the collection.
   */
  val run =
    for {
      answers <- ZIO.foreach(questions)(question => Console.printLine(question) *> Console.readLine)
      _ <- ZIO.foreach(answers)(answer => Console.printLine(answer)) // allows to print each answer in separate line as well
    } yield ()
}

object WhileLoop extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement the functional effect version of a while loop so the
   * application runs correctly.
   */
  def whileLoop[R, E, A](cond: UIO[Boolean])(zio: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] = {
    // for {
    //   condition <- cond // extracts the effect into a boolean
    //   chunk <- if (!condition) ZIO.succeed(Chunk.empty)
    //         //else zio.flatMap(a => whileLoop(cond)(zio)) // this is already an effect so no need to wrap it in an effect
    //         else {
    //           for {
    //             a <- zio
    //             as <- whileLoop(cond)(zio)
    //           }
    //         } yield a +: as // +: instead of :: here because Chunk is a list?
    // } yield chunk

    // refactor it so no nested for comprehension
    // val step = 
    //   for {
    //       a <- zio
    //       as <- whileLoop(cond)(zio)
    //     }
    //   } yield a +: as // +: instead of :: here because Chunk is a list?
      
    // can be refactored this way
    val step = zio.zipWith(whileLoop(cond)(zio)(_ +: _))

    // for {
    //   condition <- cond // extracts the effect into a boolean
    //   chunk <- if (!condition) ZIO.succeed(Chunk.empty) else step
    // } yield chunk
    
    // refactor to this one liner 
    ZIO.ifZIO(cond)(step, ZIO.succeed(Chunk.empty))
    // not recommended to do this for beginner since it's harder to read
    //ZIO.ifZIO(cond)(zio.zipWith(whileLoop(cond)(zio)(_ +: _)), ZIO.succeed(Chunk.empty) 
  }

  val run = {
    def loop(variable: Ref[Int]) = // ZIO's Ref is kind of like a mutable reference but works in functional style
      whileLoop(variable.get.map(_ < 100)) {
        for {
          value <- variable.get
          _     <- Console.printLine(s"At iteration: ${value}")
          _     <- variable.update(_ + 1)
        } yield ()
      }

    (for {
      variable <- Ref.make(0)
      _        <- loop(variable)
    } yield 0)
  }
}

object Iterate extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement the `iterate` function such that it iterates until the condition
   * evaluates to false, returning the "last" value of type `A`.
   */
  def iterate[R, E, A](start: A)(cond: A => Boolean)(f: A => ZIO[R, E, A]): ZIO[R, E, A] =
    for {
      _ <- if (!cond(start)) ZIO.succeed(start) // start is the only value A
      else {
        for {
          newA <- f(start)
          lastA <- iterate(newA)(cond)(f)
        } yield lastA 
      }
    }

  val run =
    iterate(0)(_ < 100) { i =>
      Console.printLine(s"At iteration: ${i}").as(i + 1) // as() shorthand for map disgarding the input value to map e.g. map (_ => i + 1)
      // if you need to map to unit can also just do .unit
    }
}

object TailRecursive extends ZIOAppDefault {
  trait Response
  trait Request {
    def returnResponse(response: Response): Task[Unit]
  }

  lazy val acceptRequest: Task[Request] = ZIO.attempt(new Request {
    def returnResponse(response: Response): Task[Unit] =
      ZIO.attempt(println(s"Returning response ${response}"))
  })

  def handleRequest(request: Request): Task[Response] = ZIO.attempt {
    println(s"Handling request ${request}")
    new Response {}
  }

  /**
   * EXERCISE
   *
   * Make this infinite loop (which represents a webserver) effectfully tail
   * recursive.
   */
  lazy val webserver: Task[Nothing] = // infinite loops are returned as nothing
    for {
      request  <- acceptRequest
      response <- handleRequest(request)
      _        <- request.returnResponse(response)
      nothing  <- webserver
    } yield nothing

  // this is tail rec version from zio pov but not from scala pov so can't put @tailrec 
  lazy val webserver2: Task[Nothing] = // infinite loops are returned as nothing
    acceptRequest.flatMap { request =>
      handleRequest(request).flatMap { response =>
        request.returnResponse(response).flatMap { _ => webserver
          // webserver.map { nothing =>
          //   nothing
          // } // not necessary to keep this extra map; this is the part that zio will complain is not tail rec
        }
      }
    }

  lazy val webserver3: Task[Nothing] = 
    (for {
      request  <- acceptRequest
      response <- handleRequest(request)
      _        <- request.returnResponse(response)
    } yield ()).forever // this will allow zio to do it infinitely by itself to be tail rec, no need to do it yourself above

  val run =
    (for {
      fiber <- webserver.fork
      _     <- ZIO.sleep(100.millis)
      _     <- fiber.interrupt
    } yield ())
}
