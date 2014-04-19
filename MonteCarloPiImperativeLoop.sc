/*
 * Determine whether a randomly generated (x, y) coordinate lies within the unit circle.
 */
def sqr(x: Double) = x * x
val inCircle: ((Double, Double)) => Boolean = { case (x, y) => sqr(x) + sqr(y) <= 1.0 }

def dart = (math.random, math.random)

val numDarts  = 10000000
var index = 0
var numDartsInCircle = 0
while (index < numDarts) {
  if (inCircle(dart)) numDartsInCircle += 1
  index += 1
}

val pi = 4.0 * numDartsInCircle / numDarts
println("pi = " + pi + " with " + numDarts + " darts")

/*
 * Putting it all together. We provide a core method to compute the number
 * of darts that hit the circle, which works with an Long number of darts.
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

for { problemSize <- sizes drop 5 take 5 } {
  val (runTime, result) = secondsTime { monteCarloCircleArea(problemSize) }
  println(s"n = $problemSize, t = $runTime, pi = $result")
}
/* end-performance-study */
