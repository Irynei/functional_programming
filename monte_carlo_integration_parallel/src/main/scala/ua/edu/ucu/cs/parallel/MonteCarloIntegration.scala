package ua.edu.ucu.cs

import org.scalameter
import scala.util.Random
import org.scalameter._
import Math.{abs, PI, cos}

object MonteCarloIntegration {

  def CountPointsUnderFunction(f: Double => Double, a: Double, b:Double, min:Double, max:Double, totalNumberOfPoints: Int) = {
    val rndX = new Random
    def rndY = new Random

    def simulate(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoints) hits
      else {
        // pick up random x in range [a, b]
        val x = a + (b - a) * rndX.nextDouble()
        val y = min + (max - min) * rndY.nextDouble()
        if (abs(f(x)) < abs(y)) simulate(hits, pointsGenerated + 1)
        else
          {
            val inc =
              if(f(x) < 0 & y < 0) -1
              else if (f(x) > 0 & y > 0) 1
              else 0
            simulate(hits + inc, pointsGenerated + 1)
          }
      }
    }
    simulate(0, 0)
  }

  def integralSeq(f: Double => Double, a: Double, b:Double, min:Double, max:Double, totalNumberOfPoints: Int): Double =
    (b - a) * (max - min) * CountPointsUnderFunction(f, a, b, min, max, totalNumberOfPoints) / totalNumberOfPoints

  def integralPar(f: Double => Double, a: Double, b:Double, min:Double, max:Double, totalNumberOfPoints: Int): Double = {
    val (p1, p2) = parallel.parallel(
          CountPointsUnderFunction(f, a, b, min, max, totalNumberOfPoints / 2),
          CountPointsUnderFunction(f, a, b, min, max, totalNumberOfPoints / 2)
        )
    (b - a) * (max - min) * (p1 + p2) / totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000
    // testing
    def f(x: Double): Double = cos(x)
    val (a, b) = (0, PI / 2)
    val(min, max) = (0, 1)

//    def f(x: Double): Double = x * x * x
//    val (a, b) = (0, 1)
//    val(min, max) = (0, 1)

//    def f(x: Double): Double = x
//    val (a, b) = (-2, 4)
//    val(min, max) = (-2, 4)

    println(integralSeq(f, a, b, min, max, totalNumberOfPoints))

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ) withWarmer new scalameter.Warmer.Default


    val seqtime = standartConfig.measure {
      integralSeq(f, a, b, min, max, totalNumberOfPoints)
    }

    val partime = standartConfig.measure {
      integralPar(f, a, b, min, max, totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}