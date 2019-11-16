package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Var {
      val aVal = a()
      val bVal = b()
      val cVal = c()
      bVal * bVal - 4.0 * aVal * cVal
    }
  }

  def computeSolutions(a: Signal[Double],
                       b: Signal[Double],
                       c: Signal[Double],
                       delta: Signal[Double]): Signal[Set[Double]] = Var {
    val aVal = a()
    val bVal = b()
    val cVal = c()
    val deltaVal = delta()

    if (deltaVal < 0) Set.empty[Double]
    else if (deltaVal == 0) Set(-bVal / (2.0 * aVal))
    else
      Set(
        (-bVal - Math.sqrt(deltaVal)) / (2.0 * aVal),
        (-bVal + Math.sqrt(deltaVal)) / (2.0 * aVal)
      )
  }
}
