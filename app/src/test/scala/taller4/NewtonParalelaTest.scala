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
import taller4.Newton.{buenaAprox, derivar, evaluar, limpiar, mostrar}
import taller4.NewtonParalela.raizNewton
@RunWith(classOf[JUnitRunner])
class NewtonParalelaTest extends AnyFunSuite {

  test("mostrar expr1") {
    val expr1 = Suma(Atomo('x'), Numero(2.0))
    assert(NewtonParalela.mostrar(expr1) == "(x + 2.0)")
  }
  test("derivar should handle constant functions correctly") {
    val expr = Numero(5.0)
    val derivada = NewtonParalela.derivar(expr, Atomo('x'))
    assert(NewtonParalela.mostrar(derivada) == "0.0") // d/dx (5) -> 0
  }


  test("evaluar should correctly evaluate expressions") {
      val expr1 = Numero(5.0)
      val expr2 = Atomo('x')
      val expr3 = Suma(Atomo('x'), Numero(2.0))
      val expr4 = Prod(Atomo('x'), Atomo('x'))
      val expr5 = Resta(Atomo('x'), Numero(2.0))
      val expr6 = Div(Atomo('x'), Numero(2.0))
      val expr7 = Expo(Atomo('x'), Numero(2.0))
      val expr8 = Logaritmo(Numero(2.0))

      assert(evaluar(expr1, Atomo('x'), 1.0) == 5.0)
      assert(evaluar(expr2, Atomo('x'), 5.0) == 5.0)
      assert(evaluar(expr3, Atomo('x'), 5.0) == 7.0)
      assert(evaluar(expr4, Atomo('x'), 5.0) == 25.0)
      assert(evaluar(expr5, Atomo('x'), 5.0) == 3.0)
      assert(evaluar(expr6, Atomo('x'), 4.0) == 2.0)
      assert(evaluar(expr7, Atomo('x'), 3.0) == 9.0)
      assert(evaluar(expr8, Atomo('x'), 0.0) == math.log(2.0))
    }
  test("limpiar should simplify expressions with multiplication by one") {
    val expr = Prod(Atomo('x'), Numero(1.0)) // x * 1
    val simplifiedExpr = NewtonParalela.limpiar(expr)
    assert(NewtonParalela.mostrar(simplifiedExpr) == "x")
  }

  test("raizNewton should correctly find roots") {
    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
    val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
    val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

    assert(math.abs(raizNewton(e1, Atomo('x'), 2.0, buenaAprox) - 1.41421356237) < 0.001)
    assert(math.abs(raizNewton(e2, Atomo('x'), 2.0, buenaAprox) - 2.0) < 0.001)
    assert(math.abs(raizNewton(e3, Atomo('x'), 2.0, buenaAprox) - 1.0) < 0.001)
  }




}
