/*
 * Another classic: Newton's method for finding the root of an equation f(x) = 0.
 */

/** The desired accuracy. */
val eps = 1e-10

/** Functionally iterates until a given predicate is satisfied. */
def iterateUntil[A](x0: A, f: A => A, p: (A, A) => Boolean): A = Iterator.iterate {
  (f(x0), x0) // starting point
} { case (xPrev, _) =>
  val xCurr = f(xPrev) // calculate the new value from the old one
  println(xCurr)
  (xCurr, xPrev)
}.dropWhile { case (xCurr, xPrev) =>
  ! p(xCurr, xPrev)
}.next()._1 // return the result of the last iteration

/** Newton's method as an application of `iterateUntil`. */
def newton(x0: Double, f: Double => Double, fPrime: Double => Double): Double =
  iterateUntil(x0, x => x - f(x) / fPrime(x), (x1, x0) => math.abs(x1 - x0) < eps)

/** A sample function and its derivative. */
def f(x: Double) = 3 * math.pow(x, 2) + 5 * x - 7
def fPrime(x: Double) = 6 * x + 5

assert { f(newton(0, f, fPrime)) < eps }

