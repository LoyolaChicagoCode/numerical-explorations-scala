/*
 * The world-famous Monte Carlo method for calculating pi.
 * It is a great example of the power of functional programming for mathematical/scientific codes, but also
 * illustrates some of the challenges to make it work properly without blowing up memory (etc.)
 */

/*
 * This is a reconstruction of the chunk-free version so we have a better way of comparing the time
 * to other versions. Chunking, admittedly, is a workaround for Scala sequences not being able to have
 * more than int values.
 */

/*
 * Determine whether a randomly generated (x, y) coordinate lies within the unit circle.
 */
def sqr(x: Double) = x * x

val inCircle: ((Double, Double)) => Boolean = { case (x, y) => sqr(x) + sqr(y) <= 1.0 }

/*
 * Generate a Stream of an arbitrary number of uniform deviate pairs (x, y).
 * Shows also how to get the first N pairs (for going a specified number of iterations).
 */

val randomPairs = Stream continually (math.random, math.random) iterator

val n = 1000000 // memoization: will run out of heap space for 10^7
val darts = randomPairs take n

/*
 * Finding the number of darts that hit the circle is a matter of finding those that lie within the circle.
 * For example (0.5, 0.5) is in the circle; (0.9, 0.9) is not.
 */
val dartsInCircle = darts count inCircle
val totalDarts = darts length

/*
 * The 4.0 comes from the area of the square that bounds the circle from (-1.0, -1.0) to (1.0, 1.0).
 */
val area = 4.0 * dartsInCircle.toDouble / totalDarts

/*
 * Putting it all together. We provide a core method to compute the number of darts that hit the circle,
 * which works with an Int number of darts. Because Scala's core methods cannot work on a Long number of
 * elements, we drive this method with MonteCarloCircleArea that breaks up a long number of darts into
 * discrete chunks (of chunkSize). This allows us to get the required number of iterations to approximate
 * pi (almost) as closely as desired.
 *
 * FIXME successive invocations of longDartsInCircle need to pull darts from the point where the
 * previous invocation left off
 */
def longDartsInCircle(numDarts: Int): Long = randomPairs take numDarts count inCircle

/* begin-monteCarloCircleArea */
def monteCarloCircleArea(numDarts: Int): Double = {
  val dartsInCircle = longDartsInCircle(numDarts)
  4.0 * dartsInCircle.toDouble / numDarts.toDouble
}
/* end-monteCarloCircleArea */

// Courtesy of this posting on StackOverflow:
// http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala

/* begin-time */
def nanoTime[R](block: => R): (Long, R) = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  ((t1-t0), result)
}

def secondsTime[R](block: => R): (Double, R) = {
  nanoTime(block) match {
    case (runTime, result) => (runTime / 1.0e9, result)
  }
}
/* end-time */

/* begin-performance-study */
val powers = 1 to math.log10(Int.MaxValue).floor.toInt
val sizes = powers map { math.pow(10, _).toInt } 
val problemSizes = sizes drop 5 take 5

val results = for (numDarts <- problemSizes) yield {
  val result = secondsTime { monteCarloCircleArea(numDarts) }
  result match {
    case (runTime, result) => println(s"t = $runTime, pi = $result")
  }
  result
}

println(results)
/* end-performance-study */
