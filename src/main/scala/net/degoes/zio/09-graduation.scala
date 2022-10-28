package net.degoes.zio

import zio._

object Sharding extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Create N workers reading from a Queue, if one of them fails, then wait
   * for the other ones to process their current item, but terminate all the
   * workers.
   *
   * Return the first error, or never return, if there is no error.
   */
  def shard[R, E, A](
    queue: Queue[A],
    n: Int,
    worker: (String, A) => ZIO[R, E, Unit]
  ): ZIO[R, Nothing, E] = {
    // val z: ZIO[R, E, Seq[Unit]] = 
    ZIO.
      foreachPar(1 to n) { i =>
        (for {
          a <- queue.take
          _ <- worker(s"worker-$i", a)//.uniterruptable
        
        } yield ()).forever
      }
      .flip
      .mapError(_.head)
  }

  val run = {
    def makeWorker(ref: Ref[Int]): (String, Int) => ZIO[Any, String, Unit] =
      (id: String, item: Int) =>
        for {
          count <- ref.get
          _ <- if (count < 100) Console.printLine(s"Worker ${id} is processing item ${item} after ${count}").orDie
              else ZIO.fail(s"Uh oh, worker ${id} failed processing ${item} after ${count}")
          _ <- ref.update(_ + 1)
        } yield ()

    for {
      queue <- Queue.bounded[Int](100)
      ref   <- Ref.make(0)
      _     <- queue.offer(1).forever.fork
      error <- shard(queue, 10, makeWorker(ref))
      _     <- Console.printLine(s"Failed with ${error}")
    } yield ()
  }
}

object SimpleActor extends ZIOAppDefault {

  sealed trait Command
  case object ReadTemperature                       extends Command
  final case class AdjustTemperature(value: Double) extends Command

  type TemperatureActor = Command => Task[Double]

  /**
   * EXERCISE
   *
   * Using ZIO Queue and Promise, implement the logic necessary to create an
   * actor as a function from `Command` to `Task[Double]`.
   */
   //  side note: zio actors are pretty much abandoned now as it's not really needed when working w/ zio
  def makeActor(initialTemperature: Double): UIO[TemperatureActor] = {
    type Bundle = (Command, Promise[Nothing, Double])


    def processCommand(inbox: Queue[Bundle], state: Ref[Double]): UIO[Unit] = { // returns a temperature
      for {
        bundle <- inbox.take // takes an item from the inbox
        (command, promise) = bundle
        _ <- (command match {
                          case ReadTemperature => state.get.debug("get") // add debug statements
                          case AdjustTemperature(value) => state.updateAndGet(_ + value).debug("update") // lets you modify and get the new value
                        }).intoPromise(promise) // store the result to complete the promise
        
      } yield ()
    }
    
    //TemperatureActor is just a function here
    // actor is like a mailbox or queue of msgs
    for {
      inbox <- Queue.bounded[Bundle](100)
      state <- Ref.make(initialTemperature) // equivalent to mutable var
      fiber <- processCommand(inbox, state).forever.fork // to do the work, create a fiber to process it
    } yield { command =>
      //inbox.offer(command) *> state.get // add the command to the queue in the actor; we can't get the current state immediately after queing it, we have to process it first
      
      // need to sync between fibers, using promises
      for {
        promise <- Promise.make[Nothing, Double]
        
        //_ <- inbox.offer(command, promise)  
        //temperature <- promise.await // wait for the command to finish before returning

        //simplified above 2 lines as this
        temperature <- inbox.offer(command, promise) *> promise.await 
      } yield temperature
    }
  }

  val run = {
    val temperatures = (0 to 100).map(_.toDouble)

    (for {
      actor <- makeActor(0)
      _     <- ZIO.foreachParDiscard(temperatures)(temp => actor(AdjustTemperature(temp)))
      temp  <- actor(ReadTemperature)
      _     <- Console.printLine(s"Final temperature is ${temp}")
    } yield ())
  }
}

object ParallelWebCrawler extends ZIOAppDefault {

  trait Web {
    def getURL(url: URL): IO[Exception, String]
  }
  object Web {

    /**
     * EXERCISE
     *
     * Using `ZIO.serviceWithZIO`, delegate to the `Web` module's `getURL` function.
     */
    def getURL(url: URL): ZIO[Web, Exception, String] = 
      ZIO.serviceWithZIO(_.getURL(url))
  }

  final case class WebLive() extends Web {

    /**
     * EXERCISE
     *
     * Implement this method using the `ZIO.attemptBlockingIO` combinator
     * to safely wrap `Source.fromURL` into a functional effect.
     */
    override def getURL(url: URL): IO[Exception, String] = 
      // acquireRelease creates a zio effect w/o an env, but our return val says no env required so acquireReleaseWith
      ZIO.acquireReleaseWith(
        ZIO.attemptBlockingIO(
          scala.io.Source.fromURL(url.toString())
        )
      ) (source => ZIO.attemptBlockingIO(source.close()).orDie
      ) (source => 
        ZIO.attemptBlockingIO(source.mkString("\n"))
      )

  }
  object WebLive {

    /**
     * EXERCISE
     *
     * Implement a layer for `WebLive`
     */
    val layer: ZLayer[Any, Nothing, Web] /* aka ULayer[Web] */ = ZLayer.succeed(WebLive()) 
  }

  final case class CrawlState[+E](visited: Set[URL], errors: List[E]) {
    final def visitAll(urls: Set[URL]): CrawlState[E] = copy(visited = visited ++ urls)

    final def logError[E1 >: E](e: E1): CrawlState[E1] = copy(errors = e :: errors)
  }

  /**
   * EXERCISE
   *
   * Implement the `crawl` function using the helpers provided in this object.
   *
   * {{{
   * def getURL(url: URL): ZIO[Web, Exception, String]
   * def extractURLs(root: URL, html: String): List[URL]
   * }}}
   */
  def crawl[E](
    seeds: Set[URL],
    router: URL => Set[URL],
    processor: (URL, String) => IO[E, Unit]
  ): ZIO[Web, Nothing, List[E]] = {
    val emptySet = ZIO.succeed(Set.empty[URL])

    def loop(seeds: Set[URL], ref: Ref[CrawlState[E]]): ZIO[Web, Nothing, Unit] =
      if (seeds.isEmpty) ZIO.unit
      else {
        val newSeeds = ZIO.foreachPar(seeds.flatMap(router)) { seed =>
          for {
            contents <- Web.getURL(seed) // this can return w/ exception
            _ <- processor(seed, contents).catchAll(e => ref.update(_.logError(e))) // this can fail w/ type E, so catchall to handle it
          } yield extractURLs(seed, contents)
        }.map(_.flatten) // [Web, Exception, Set[URL]]
        .orElse(emptySet) //&& ZIO.aspect

        // newSeeds.flatMap { seeds => 
        //   ref.update(_.visitAll(seeds))
        // }

        for {
          seeds <- newSeeds
          //_ <-  ref.update(_.visitAll(seeds))
          dedupedSeeds <- ref.modify(state => (seeds -- state.visited, state.visitAll(seeds)))
          _ <- loop(dedupedSeeds, ref)
        } yield ()
      }

    for {
      ref   <- Ref.make[CrawlState[E]](CrawlState(seeds, Nil))
      _     <- loop(seeds, ref)
      state <- ref.get
    } yield state.errors
  }

  /**
   * A data structure representing a structured URL, with a smart constructor.
   */
  final case class URL private (parsed: io.lemonlabs.uri.Url) {
    import io.lemonlabs.uri._

    final def relative(page: String): Option[URL] =
      scala.util.Try {
        val parts = parsed.path.parts

        val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

        parsed.withPath(UrlPath(whole))
      }.toOption
        .map(new URL(_))

    def url: String = parsed.toString

    override def equals(a: Any): Boolean = a match {
      case that: URL => this.url == that.url
      case _         => false
    }

    override def hashCode: Int = url.hashCode
  }

  object URL {
    import io.lemonlabs.uri._

    def make(url: String): Option[URL] =
      scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
        case None         => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  /**
   * A function that extracts URLs from a given web page.
   */
  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util
      .Try({
        val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

        for {
          m   <- matches
          url <- URL.make(m).toList ++ root.relative(m).toList
        } yield url
      })
      .getOrElse(Nil)
  }

  object test {
    val Home          = URL.make("http://zio.dev").get
    val Index         = URL.make("http://zio.dev/index.html").get
    val ScaladocIndex = URL.make("http://zio.dev/scaladoc/index.html").get
    val About         = URL.make("http://zio.dev/about").get

    val SiteIndex =
      Map(
        Home          -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index         -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About         -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )

    final case class WebTest() extends Web {

      /**
       * EXERCISE
       *
       * Implement a test version of this method using the SiteIndex data.
       */
      override def getURL(url: URL): IO[Exception, String] = 
        ZIO.fromOption(SiteIndex.get(url))
          // add this for IO Exception
          //.orElse(ZIO.fail(new Exception("URL not found"))) 
          .orElseFail(new Exception("URL not found")) //orElseFail removes the need for ZIO.fail
    }

    object WebTest {

      /**
       * EXERCISE
       *
       * Implement a layer for `WebTest`
       */
      val layer: ULayer[Web] /*ZLayer[Any, Nothing, Web]*/ = ZLayer.succeed(WebTest())
    }

    val TestRouter: URL => Set[URL] =
      url => if (url.parsed.apexDomain == Some("zio.dev")) Set(url) else Set()

    val Processor: (URL, String) => IO[(URL, String), Unit] = { (url, html) =>
      Random.nextBoolean.flatMap {
        case true  => Console.printLine(s"Processing URL: $url, HTML: $html").orDie
        case false => ZIO.fail((url, html))
      }
    }
  }

  /**
   * EXERCISE
   *
   * Run your test crawler using the test data, supplying it the custom layer
   * it needs.
   */
  val run =
    //Console.printLine("Hello World!")
    crawl(Set(test.Home), test.TestRouter, test.Processor).provide(test.WebTest.layer)
}

object Hangman extends ZIOAppDefault {
  import java.io.IOException

  /**
   * EXERCISE
   *
   * Implement an effect that gets a single, lower-case character from
   * the user.
   */
  lazy val getChoice: ZIO[Any, IOException, Char] = 
    (Console.printLine("Please introduce a character: ") *> Console.readLine).flatMap { input =>
      // check if letter is char
      input.toList match {
        case c :: Nil => ZIO.succeed(c)
        case _ => Console.printLine("Invalid input") *> getChoice // get choice again if it fails
      }
    }

  /**
   * EXERCISE
   *
   * Implement an effect that prompts the user for their name, and
   * returns it.
   */
  lazy val getName: ZIO[Any, IOException, String] = 
    Console.printLine("Give me your name: ") *> Console.readLine

  /**
   * EXERCISE
   *
   * Implement an effect that chooses a random word from the dictionary.
   * The dictionary is `Dictionary.Dictionary`.
   */
  lazy val chooseWord: ZIO[Any, Nothing, String] = 
    // for {
    //   index <- Random.nextIntBounded(Dictionary.Dictionary.length)
    //   word <- ZIO.attempt(Dictionary.Dictionary(index)).orDie // it should never fail but what if someone removed the whole dictionary and it's empty
    // } yield word

    // alternative
    for {
      shuffled <- Random.shuffle(Dictionary.Dictionary)
      word <- ZIO.fromOption(shuffled.headOption).orDieWith(_ => new Exception ("Empty dictionary"))
    } yield word
 
  /**
   * EXERCISE
   *
   * Implement the main game loop, which gets choices from the user until
   * the game is won or lost.
   */
  def gameLoop(oldState: State): ZIO[Any, IOException, Unit] = 
    for {
      choice <- getChoice
      newState = oldState.addChar(choice)
      guessResult = analyzeNewInput(oldState, newState, choice)
      _ <- guessResult match {
        case GuessResult.Won => Console.printLine("You won!")
        case GuessResult.Lost => Console.printLine("You lost!")
        case GuessResult.Correct => Console.printLine("You gusssed a letter!") *> gameLoop(newState)
        case GuessResult.Incorrect => Console.printLine("You failed to guess a letter!") *> gameLoop(newState)
        case GuessResult.Unchanged => Console.printLine("You already guessed this letter!") *> gameLoop(newState)
      }
    } yield ()


  def renderState(state: State): ZIO[Any, IOException, Unit] = {

    /**
     *
     *  f     n  c  t  o
     *  -  -  -  -  -  -  -
     *
     *  Guesses: a, z, y, x
     *
     */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    Console.printLine(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won       extends GuessResult
    case object Lost      extends GuessResult
    case object Correct   extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def analyzeNewInput(
    oldState: State,
    newState: State,
    char: Char
  ): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
   * EXERCISE
   *
   * Execute the main function and verify your program works as intended.
   */
  val run = {
    for {
      name  <- getName
      word  <- chooseWord
      state = State(name, Set(), word)
      _     <- renderState(state)
      _     <- gameLoop(state)
    } yield ()
  }
}

object TicTacToe extends ZIOAppDefault {

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }
    final def render: String = renderChar.toString
  }
  object Mark {
    case object X extends Mark
    case object O extends Mark
  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
     * Retrieves the mark at the specified row/col.
     */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
     * Places a mark on the board at the specified row/col.
     */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
        )
      else None

    /**
     * Renders the board to a string.
     */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
     * Returns which mark won the game, if any.
     */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
      row0: Int,
      col0: Int,
      rowInc: Int,
      colInc: Int,
      mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
      row0: Int,
      col0: Int,
      rowInc: Int,
      colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        i <- 0 to 2
      } yield value(row0 + rowInc * i)(col0 + colInc * i)
  }
  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
      first: Iterable[Char],
      second: Iterable[Char],
      third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(
      List(' ', 'O', 'X'),
      List('O', 'X', 'O'),
      List('X', ' ', ' ')
    )
    .get
    .render

  /**
   * EXERCISE
   *
   * Implement a game of tic-tac-toe, where the player gets to play against a
   * computer opponent.
   */
  val run =
    Console.printLine(TestBoard)
}
