/*
 * The world-famous Monte Carlo method for calculating pi.
 * It is a great example of the power of functional programming for
 * mathematical/scientific codes, but also illustrates some of the
 * challenges to make it work properly without blowing up memory (etc.)
 */

/*
 * Determine whether a randomly generated (x, y) coordinate lies within the unit circle.
 */
def sqr(x: Double) = x * x
val inCircle: ((Double, Double)) => Boolean = { case (x, y) => sqr(x) + sqr(y) <= 1.0 }

/*
 * Create an Iterator of an arbitrary number of uniform deviate
 * pairs (x, y). Shows also how to get the first N pairs (for going
 * a specified number of iterations).
 */
val randomPairs = Iterator continually (math.random, math.random)

val chunkSize = 10000
val numDarts  = 10000000

// An Iterator that produces discrete chunks of darts
val chunkedRandomPairs = randomPairs grouped chunkSize

val numChunks = numDarts / chunkSize
// convert chunkwise counts to Long to avoid overflow during summation
val dartsInCircle = chunkedRandomPairs take numChunks map { _ count inCircle toLong } sum
val pi = 4.0 * dartsInCircle / numDarts
println("pi = " + pi + " with " + numDarts + " darts")

/*
 * Putting it all together. We provide a core method to compute the number
 * of darts that hit the circle, which works with an Int number of darts.
 * Because Scala's core methods cannot work on a Long number of elements,
 * we drive this method with MonteCarloCircleArea that breaks up a long
 * number of darts into discrete chunks (of chunkSize). This allows us to
 * get the required number of iterations to approximate pi (almost) as
 * closely as desired.
 */
def monteCarloCircleArea(numDarts: Long, chunkSize: Int) = {
  val numChunks = (numDarts / chunkSize).toInt
  val dartsInCircle = randomPairs grouped chunkSize take numChunks map { _ count inCircle toLong } sum ;
  4.0 * dartsInCircle / numDarts
}

// Courtesy of this posting on StackOverflow:
// http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala

/* begin-time */
def nanoTime[R](block: => R): (Double, R) = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  (t1 - t0, result)
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

for {
  problemSize <- sizes drop 5 take 5
  chunkSize <- sizes drop 3 take 3
} {
  val (runTime, result) = secondsTime { monteCarloCircleArea(problemSize, chunkSize) }
  println(s"n = $problemSize, c = $chunkSize, t = $runTime, pi = $result")
}
/* end-performance-study */
