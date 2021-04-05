package chapter4

object E002Variance {

  private val mean: Seq[Double] => Option[Double] = seq =>
    if (seq.isEmpty)
      None
    else
      Some(seq.foldRight(0d)((acc, cur) => acc + cur) / seq.length)

  private val square: Double => Double = x => math.pow(x, 2)

  private val differentials: (Double, Seq[Double]) => Seq[Double] = {
    case (mean, seq) => seq.map(_ - mean)
  }

  private val allSquared: Seq[Double] => Seq[Double] = _.map(square)

  private val sum: Seq[Double] => Double = _.sum

  private val divideByN: (Int, Double) => Double = {
    case (n, sum) => sum / n
  }

  private def differentialsSquaredAndDivided(
      mean: Double,
      seq: Seq[Double],
      count: Int
  ): Double =
    differentials
      .curried(mean)
      .andThen(allSquared)
      .andThen(sum)
      .andThen(divideByN.curried(count))
      .apply(seq)

  private val validate: Seq[Double] => Boolean = seq => seq.length > 1

  private val variance = (seq: Seq[Double]) =>
    Some(seq)
      .filter(validate)
      .flatMap(mean)
      .map((differentialsSquaredAndDivided _).curried)
      .map(_(seq))
      .fold[Int => Option[Double]](_ => None)(f => x => Some(f(x)))

  def sampleVariance(seq: Seq[Double]): Option[Double] =
    variance(seq)(seq.length)

  def populationVariance(seq: Seq[Double]): Option[Double] =
    variance(seq)(seq.length - 1)
}
