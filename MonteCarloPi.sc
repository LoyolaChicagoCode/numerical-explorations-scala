/*
 * The world-famous Monte Carlo method for calculating pi.
 * It is a great example of the power of functional programming for mathematical/scientific codes, but also
 * illustrates some of the challenges to make it work properly without blowing up memory (etc.)
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
val randomPairs = Stream continually (math.random, math.random)

val n = 10000
val darts = randomPairs take n

/*
 * Finding the number of darts that hit the circle is a matter of finding those that lie within the circle.
 * For example (0.5, 0.5) is in the circle; (0.9, 0.9) is not.
 */
val dartsInCircle = darts filter inCircle
val totalDarts = darts length

/*
 * The 4.0 comes from the area of the square that bounds the circle from (-1.0, -1.0) to (1.0, 1.0).
 */
val area = 4.0 * dartsInCircle.length.toDouble / totalDarts
println("The area is " + area)

/*
 * Putting it all together. We provide a core method to compute the number of darts that hit the circle,
 * which works with an Int number of darts. Because Scala's core methods cannot work on a Long number of
 * elements, we drive this method with MonteCarloCircleArea that breaks up a long number of darts into
 * discrete chunks (of chunkSize). This allows us to get the required number of iterations to approximate
 * pi (almost) as closely as desired.
 */
def intDartsInCircle(numDarts: Int): Long = {
  val dartsInCircle = randomPairs take numDarts filter inCircle
  dartsInCircle.length
}

def monteCarloCircleArea(numDarts: Long, chunkSize: Int): Double = {
  val numChunks = (numDarts / chunkSize).toInt
  val remainder = (numDarts % chunkSize).toInt
  def intDartsBound() = intDartsInCircle(chunkSize)
  val dartsInCircle = intDartsInCircle(remainder) + Stream.continually(intDartsBound).take(numChunks).sum
  4.0 * dartsInCircle.toDouble / numDarts.toDouble
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

// Quick performance study.
val sizes: Stream[Long] = 1L #:: sizes map { _ * 10L }
val problemSizes = sizes drop 5 take 5
val chunkSizes = sizes drop 3 take 3 map { _.toInt }

println("Trying these probem sizes")
problemSizes foreach println

println("Trying these chunk sizes")
chunkSizes foreach println

for (numDarts <- problemSizes)
  for (chunkSize <- chunkSizes) {
    println("numDarts: " + numDarts + " chunkSize: " + chunkSize)
    val area = time { monteCarloCircleArea(numDarts, chunkSize) }
    println("The area is " + area)
  }