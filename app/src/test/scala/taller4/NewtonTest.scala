/**
 * Plantilla para pruebas
 * @author Carlos Delgado
 * @version 1.0
 * @note 22 de Noviembre de 2023
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner


import org.scalatest.funsuite.AnyFunSuite

import taller4.Newton.{derivar, evaluar, limpiar, mostrar, raizNewton}


@RunWith(classOf[JUnitRunner])
class NewtonTest extends AnyFunSuite {


    test("mostrar expr3") {
      val expr1 = Suma(Atomo('x'), Numero(2.0))
      val expr2 = Prod(Atomo('x'), Atomo('x'))
      val expr3 = Suma(expr1, Expo(expr2, Numero(5.0)))
      assert(Newton.mostrar(expr3) == "((x + 2.0) + ((x * x) ^ 5.0))")
    }


  test("derivar expr2 respecto a x") {
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    assert(mostrar(derivar(expr2, Atomo('x'))) == "((1.0 * x) + (x * 1.0))")
  }

  test("evaluar Suma(expr1, expr2) con x=5.0") {
    val expr1 = Suma(Atomo('x'), Numero(2.0))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    assert(evaluar(Suma(expr1, expr2), Atomo('x'), 5.0) == 32.0)
  }


  test("limpiar debería simplificar una expresión correctamente") {
    assert(Newton.limpiar(Suma(Numero(0.0), Atomo('x'))) == Atomo('x'))
    assert(Newton.limpiar(Prod(Numero(1.0), Atomo('x'))) == Atomo('x'))
    assert(Newton.limpiar(Prod(Numero(0.0), Atomo('x'))) == Numero(0.0))
  }

  test("raizNewton para e1") {
    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
    def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
      evaluar(f, a, d) < 0.001
    }
    assert(math.abs(raizNewton(e1, Atomo('x'), 2.0, buenaAprox) - math.sqrt(2)) < 0.001)
  }

}