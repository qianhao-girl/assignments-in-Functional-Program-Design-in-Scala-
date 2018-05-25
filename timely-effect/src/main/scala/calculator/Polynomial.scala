package calculator

import Math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val dVal = delta()
      if (dVal < 0) Set()
      else {
        val aVal = a()
        val bVal = b()
        Set(
          (-bVal + sqrt(dVal)) / (2 * aVal),
          (-bVal - sqrt(dVal)) / (2 * aVal)
        )
      }
    }
  }


// Why computeSolutions2 didn't work like computerSolutions ?
      def computeSolutions2(a: Signal[Double], b: Signal[Double],
                            c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
        if(delta() < 0) Signal(Set())
        else {
          val x1: Double = (-1 * b() + sqrt(delta())) / (2 * a())
          val x2: Double = (-1 * b() + sqrt(delta())) / (2 * a())
          Signal(Set(x1,x2))
        }
      }

}
