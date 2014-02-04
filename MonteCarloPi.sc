import scala.math.Random

// Thanks to a great pairing session with Konstantin, we have finally been able to overcome some problems
// by using Stream. 
 
def incircle(pair : (Double, Double)): Boolean  =  pair._1 * pair._1 + pair._2 * pair._2 <= 1.0;
 
val N = 10000
val darts = Stream.continually((math.random, math.random)).take(N)
// How many darts fall within the circle?
 
val dartsInCircle = darts.filter(incircle)
 
// How many darts are there in total?
 
val totalDarts = darts.length // same as N, of course.
 
// What's the approximate area of the circle?

val area = 4.0 * darts.filter(incircle).length.toDouble / darts.length

// Better in Scala to use toDouble
val area = 4.0 * (darts.filter(incircle).length.toDouble) / darts.length

// Putting it all together

// This computes the number of darts in a circle (returns <= numDarts)
def IntDartsInCircle(numDarts : Int) : Long = {
  val darts = Stream.continually( (math.random, math.random))
  val dartsInCircle = darts.take(numDarts).filter(incircle)
  dartsInCircle.length
}

// This computes the area for a long number of darts

def MonteCarloCircleArea(numDarts : Long, chunkSize : Int) : Double = {
  val numChunks = (numDarts / chunkSize).toInt
  val remainder = (numDarts % chunkSize).toInt
  def IntDartsBound() = IntDartsInCircle(chunkSize)
  val dartsInCircle = IntDartsInCircle(remainder) + Stream.continually( IntDartsBound ).take(numChunks).sum
  return 4.0 * dartsInCircle.toDouble / numDarts.toDouble
}

// Now to get a Long number of darts, just say how many darts you want and the chunkSize to use


