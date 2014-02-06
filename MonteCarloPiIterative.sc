/*
 * Determine whether a randomly generated (x, y) coordinate lies within the unit circle.
 */
def sqr(x: Double) = x * x
val inCircle: ((Double, Double)) => Boolean = { case (x, y) => sqr(x) + sqr(y) <= 1.0 }

def dart = (math.random, math.random)

val numDarts  = 10000000L
var index = 0
var numDartsInCircle = 0
while (index < numDarts) {
  if (inCircle(dart)) numDartsInCircle += 1
  index += 1
}

val pi = 4.0 * numDartsInCircle / numDarts
println("pi = " + pi + " with " + numDarts + " darts")

/*
* Putting it all together. We provide a core method to compute the number of darts that hit the circle,
* which works with an Int number of darts. Because Scala's core methods cannot work on a Long number of
* elements, we drive this method with MonteCarloCircleArea that breaks up a long number of darts into
* discrete chunks (of chunkSize). This allows us to get the required number of iterations to approximate
* pi (almost) as closely as desired.
*/
def monteCarloCircleArea(numDarts: Long) = {
  var index = 0
  var numDartsInCircle = 0
  while (index < numDarts) {
    if (inCircle(dart)) numDartsInCircle += 1
    index += 1
  }
  4.0 * numDartsInCircle / numDarts
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

println("Trying these probem sizes")
problemSizes foreach println

for (numDarts <- problemSizes) {
  println("numDarts: " + numDarts)
  val area = time { monteCarloCircleArea(numDarts) }
  println("The area is " + area)
}