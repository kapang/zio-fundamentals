package net.degoes.zio

import zio._

object Retry {

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes = //: Schedule[Any, Any, Int] = this seems to be broken; he removed it from his code
    Schedule.recurs(5)

  // val z: UIO[String] = ???
  // z.repeat(fiveTimes)

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1 = Console.printLine("Hello world").repeat(fiveTimes).unit // unit to ignore output and make it zio effect
  // zio you can just use .repeatN(5) or just .repeat with a schedule

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  val everySecond = Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  val fiveTimesEverySecond = fiveTimes && everySecond // those 5 times will happen ever second and not forever
  // it returns a tuple; it's a intersection of the 2 schedules
  // if you want it sequential, one after the other, use ++ operator

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action Console.printLine("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  val repeated2 = Console.printLine("Hi hi").repeat(fiveTimesEverySecond).unit

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  val fiveTimesThenEverySecond = fiveTimes ++ everySecond

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1   = ZIO.fail("Uh oh!")//.flip if you add this it will invert the fail/succes in zio
  val retried5 = error1.retry(fiveTimes)
  //retry and repeat both take schedules, but retry only executes on failures

  /**
   * EXERCISE
   *
   * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats the minimum of five times and
   * every second.
   */
  val fiveTimesOrEverySecond = Schedule.dayOfMonth(1) || Schedule.dayOfMonth(20) 
  // && intersection wouldn't make sense here; union operator || makes more sense

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  val exponentialSchedule = Schedule.exponential(10.millis)

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  val jitteredExponential = exponentialSchedule.jittered // adds randomness to it
  // there are other methods for doing random distributions as well

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  val oneHundred = Schedule.forever.whileOutput(_ <= 100)


  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] = Schedule.identity[A]
    //equivalent to forever, but it copies the last input to output

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, List[A]] = Schedule.collectAll[A].map(_.toList)
    // copies all values in the output and collects it to a chunk, transform it to a list to compile

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  val fiveTimesEverySecondR = fiveTimes *> everySecond
      // or * for zipleft and return only input from fiveTimes
      // or zip ? or ++ for both

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import Schedule.{ collectAll, exponential, fixed, recurs }
  def mySchedule[A]: Schedule[Any, A, List[A]] = {
    val exp = Schedule.exponential(10.millis).untilOutput(_ < 60.seconds).unit //.whileOutput(_ < 60.seconds)
    val fixed = (Schedule.spaced(60.seconds) && Schedule.recurs(100)).unit

    ((exp ++ fixed) && Schedule.collectAll[A].map(_.toList)).jittered
  }

// for testing
  //val z = ZIO.succeed(100).repeat(mySchedule)
}
