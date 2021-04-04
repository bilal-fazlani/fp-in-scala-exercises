package chapter4

import munit.FunSuite

class E002VarianceTest extends FunSuite {
  test("variance should return None for empty sequence") {
    val seq = Seq.empty[Double]
    assertEquals(E002Variance.sampleVariance(seq), None)
  }

  test("variance should return 0.5 for [3,4]") {
    val seq: Seq[Double] = Seq(3, 4)
    assertEquals(E002Variance.sampleVariance(seq), Some(0.25))
  }

  test("variance should return 7 for [3,4,8]") {
    val seq: Seq[Double] = Seq(3, 4, 8)
    assertEquals(E002Variance.sampleVariance(seq), Some(4.666666666666667))
  }

  test("variance should return None for single item") {
    val seq: Seq[Double] = Seq(3)
    assertEquals(E002Variance.sampleVariance(seq), None)
  }
}
