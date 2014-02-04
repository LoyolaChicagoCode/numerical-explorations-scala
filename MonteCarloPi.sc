/*
 * The world-famous Monte Carlo method for calculating pi.
 * It is a great example of the power of functional programming for mathematical/scientific codes, but also
 * illustrates some of the challenges to make it work properly without blowing up memory (etc.)
 */

/*
 * Determine whether a randomly generated (x, y) coordinate lies within the unit circle.
 */

def incircle(pair : (Double, Double)): Boolean  =  pair._1 * pair._1 + pair._2 * pair._2 <= 1.0

/*
 * Generate a Stream of an arbitrary number of uniform deviate pairs (x, y).
 * Shows also how to get the first N pairs (for going a specified number of iterations).
 */
val N = 10000
val darts = Stream.continually((math.random, math.random)).take(N)

/*
 * Finding the number of darts that hit the circle is a matter of finding those that lie within the circle.
 * For example (0.5, 0.5) is in the circle; (0.9, 0.9) is not.
 */

val dartsInCircle = darts.filter(incircle)

val totalDarts = darts.length

/*
 * The 4.0 comes from the area of the square that bounds the circle from (-1.0, -1.0) to (1.0, 1.0).
 */
val area = 4.0 * darts.filter(incircle).length.toDouble / darts.length

println("The area is " + area)

/*
 * Putting it all together. We provide a core method to compute the number of darts that hit the circle,
 * which works with an Int number of darts. Because Scala's core methods cannot work on a Long number of
 * elements, we drive this method with MonteCarloCircleArea that breaks up a long number of darts into
 * discrete chunks (of chunkSize). This allows us to get the required number of iterations to approximate
 * pi (almost) as closely as desired.
 */
def IntDartsInCircle(numDarts : Int) : Long = {
  val darts = Stream.continually( (math.random, math.random))
  val dartsInCircle = darts.take(numDarts).filter(incircle)
  dartsInCircle.length
}

def MonteCarloCircleArea(numDarts : Long, chunkSize : Int) : Double = {
  val numChunks = (numDarts / chunkSize).toInt
  val remainder = (numDarts % chunkSize).toInt
  def IntDartsBound() = IntDartsInCircle(chunkSize)
  val dartsInCircle = IntDartsInCircle(remainder) + Stream.continually( IntDartsBound ).take(numChunks).sum
  return 4.0 * dartsInCircle.toDouble / numDarts.toDouble
}

// Courtesy of this posting on StackOverflow:
// http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  println("Elapsed time: " + (t1 - t0).toDouble / 1.0e9 + "s")
  result
}
// Some quick timing experiments, to be generalized.
time { MonteCarloCircleArea(1000L, 1000) }
time { MonteCarloCircleArea(10000L, 1000) }
time { MonteCarloCircleArea(100000L, 1000) }
time { MonteCarloCircleArea(1000000L, 1000) }
time { MonteCarloCircleArea(10000000L, 1000) }
time { MonteCarloCircleArea(10000000L, 10000) }
time { MonteCarloCircleArea(10000000L, 100000) }



