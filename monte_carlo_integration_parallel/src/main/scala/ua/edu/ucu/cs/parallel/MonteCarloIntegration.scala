package ua.edu.ucu.cs

import org.scalameter
import scala.util.Random
import org.scalameter._
import Math.{abs, PI, cos}

object MonteCarloIntegration {

  def CountPointsUnderFunction(f: Double => Double, a: Double, b:Double, totalNumberOfPoints: Int) = {
    val rndX = new Random

    def simulate(acc: Double, pointsGenerated: Int): Double = {
      if (pointsGenerated >= totalNumberOfPoints) acc
      else {
        // pick up random x in range [a, b]
        val x = a + (b - a) * rndX.nextDouble()
        simulate(acc + (b - a) * f(x), pointsGenerated + 1)
      }
    }
    simulate(0, 0)
  }

  def integralSeq(f: Double => Double, a: Double, b:Double, totalNumberOfPoints: Int): Double =
    CountPointsUnderFunction(f, a, b,totalNumberOfPoints) / totalNumberOfPoints

  def integralPar(f: Double => Double, a: Double, b:Double, totalNumberOfPoints: Int): Double = {
        val (p1, p2) = parallel.parallel(
              CountPointsUnderFunction(f, a, b, totalNumberOfPoints / 2),
              CountPointsUnderFunction(f, a, b,  totalNumberOfPoints / 2)
            )
        (p1 + p2) / totalNumberOfPoints
//    val (p1, p2, p3, p4) = parallel.parallel(
//      CountPointsUnderFunction(f, a, b, totalNumberOfPoints / 4),
//      CountPointsUnderFunction(f, a, b,  totalNumberOfPoints / 4),
//      CountPointsUnderFunction(f, a, b,  totalNumberOfPoints / 4),
//      CountPointsUnderFunction(f, a, b,  totalNumberOfPoints / 4)
//    )
//    (p1 + p2 + p3 + p4) / totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000
    // testing
//    def f(x: Double): Double = cos(x)
//    val (a, b) = (0, PI / 2)

//    def f(x: Double): Double = x * x * x
//    val (a, b) = (0, 1)

    def f(x: Double): Double = x
    val (a, b) = (-2, 4)

    println(integralSeq(f, a, b, totalNumberOfPoints))

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer new scalameter.Warmer.Default


    val seqtime = standartConfig.measure {
      integralSeq(f, a, b, totalNumberOfPoints)
    }

    val partime = standartConfig.measure {
      integralPar(f, a, b, totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}