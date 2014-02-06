// TODO state monad?

/*
 * Determine whether a randomly generated (x, y) coordinate lies within the unit circle.
 */
def sqr(x: Double) = x * x
val inCircle: ((Double, Double)) => Boolean = { case (x, y) => sqr(x) + sqr(y) <= 1.0 }

// use StreamIterator to avoid memoization
val randomPairs = Stream continually (math.random, math.random) iterator

val chunkSize = 10000
val numDarts  = 100000000L

// use sliding instead of grouped for looking at each chunk only once
val chunkedRandomPairs = randomPairs sliding chunkSize

// convert chunkwise counts to Long to avoid overflow during summation
val numChunks = (numDarts / chunkSize).toInt
val dartsInCircle = chunkedRandomPairs take numChunks map { _ count inCircle toLong } sum
val pi = 4.0 * dartsInCircle / numDarts
println("pi = " + pi)

/*
* Putting it all together. We provide a core method to compute the number of darts that hit the circle,
* which works with an Int number of darts. Because Scala's core methods cannot work on a Long number of
* elements, we drive this method with MonteCarloCircleArea that breaks up a long number of darts into
* discrete chunks (of chunkSize). This allows us to get the required number of iterations to approximate
* pi (almost) as closely as desired.
*/
def monteCarloCircleArea(numDarts: Long, chunkSize: Int) = {
  val numChunks = (numDarts / chunkSize).toInt
  val dartsInCircle = randomPairs sliding chunkSize take numChunks map { _ count inCircle toLong } sum ;
  4.0 * dartsInCircle / numDarts
}

// Courtesy of this posting on StackOverflow:
// http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
//  println("Elapsed time: " + (t1 - t0) + "ns")
  println("Elapsed time: " + (t1 - t0).toDouble / 1.0e9 + "s")
  result
}

// Quick performance study.
val sizes: Stream[Long] = 1L #:: sizes map { _ * 10L }
val problemSizes = sizes drop 5 take 5
val chunkSizes = sizes drop 3 take 3 map { _ toInt }

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