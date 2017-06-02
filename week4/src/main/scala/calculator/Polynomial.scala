package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal {
      val a1 = a()
      val b1 = b()
      val c1 = c()
      b1 * b1 - 4 * a1 * c1
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val a1 = a()
      val b1 = b()
      val delta1 = delta()
      if (a1 == 0)
        if (b1 == 0) Set()
        else Set(-c() / b1)
      else
      if (delta1 < 0) Set()
      else if (delta1 > 0)
        Set((-b1 - math.sqrt(delta1)) / (2 * a1), (-b1 + math.sqrt(delta1)) / (2 * a1))
      else Set(-b1 / (2 * a1))
    }
  }
}
