package taller4
//La paralelización en NewtonParalela se logra utilizando Future para ejecutar evaluaciones de funciones de manera concurrente. Esta estrategia mejora la eficiencia y el rendimiento del algoritmo de Newton, especialmente en problemas donde las evaluaciones de funciones son costosas en términos de tiempo de cómputo.

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object NewtonParalela {
  def mostrar(e: Expr): String = Newton.mostrar(e)

  def derivar(f: Expr, a: Atomo): Expr = Newton.derivar(f, a)

  def evaluar(f: Expr, a: Atomo, v: Double): Double = {
    val futureResult = Future {
      Newton.evaluar(f, a, v)
    }
    Await.result(futureResult, Duration.Inf)
  }

  def limpiar(f: Expr): Expr = Newton.limpiar(f)

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    val maxIterations = 1000
    val tolerance = 1e-10

    def iterate(xi: Double, iterations: Int): Double = {
      if (iterations >= maxIterations) xi
      else {
        val futureFx = Future {
          Newton.evaluar(f, a, xi)
        }
        val futureDfx = Future {
          Newton.evaluar(Newton.derivar(f, a), a, xi)
        }
        val fx = Await.result(futureFx, Duration.Inf)
        val dfx = Await.result(futureDfx, Duration.Inf)
        val xi1 = xi - fx / dfx
        if (ba(f, a, xi1)) xi1
        else iterate(xi1, iterations + 1)
      }
    }

    iterate(x0, 0)
  }

}

