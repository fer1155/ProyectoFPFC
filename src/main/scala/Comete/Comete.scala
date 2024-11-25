import Comete.{DistributionValues, MedidaPol}

import scala.annotation.tailrec
import scala.math.BigDecimal.double2bigDecimal

// Medida de polarizacion
package object Comete {

  type DistributionValues = Vector[Double]
  // Representa los valores posibles de opinion
  // Tipo para los valores reales de una distribucion (Ordenado) [0,0 - 1,0]

  type Frequency = Vector[Double]
  // Representa probabilidades
  // Pi_k es una frecuencia de longitud k
  // Si Pi_k.lenght=k, 0 <= Pi_k(i) <= 1, 0 <= i <= k-1
  // Pi_k.sum == 1
  // Toman valores reales entre 0 y 1 (No hay orden)

  type Distribution = (Frequency, DistributionValues)
  // (π,y) Probabilidades, valores de distribucion
  // (Pi,dv) es una distribucion si Pi es una frecuencia y dv son los valores de distribucion
  // Son de la misma longitud

  type MedidaPol = Distribution => Double
  // Funcion xd

  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double =
    if (max - min < prec) (min + max) / 2
    else {
      val step = (max - min) / 10
      val points = (0 to 10).map(i => min + i * step)
      val minIndex = points.indices.minBy(i => f(points(i)))
      val (left, right) = (if (minIndex > 0) points(minIndex - 1) else min,
        if (minIndex < points.size - 1) points(minIndex + 1) else max)

      if (math.abs((min + max) / 2 - points(minIndex)) < prec) points(minIndex)
      else min_p(f, left, right, prec)
    }


  //Funcion Auxiliar
  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
    def rhoAux(dist: Distribution, p: Double): Double = {
      val (frecuencias, valores) = dist
      frecuencias.zip(valores)
        .map { case (pi, yi) =>
          math.pow(pi, alpha) * math.pow(math.abs(yi - p), beta)
        }.sum
    }

    def medidaComete(dist: Distribution): Double = {
      // Función auxiliar que será minimizada
      val f = (p: Double) => rhoAux(dist, p)
      // Buscamos el mínimo en el intervalo [0,1] con precisión 0.001
      val p_min = min_p(f, 0.0, 1.0, 0.001)

      // Devolvemos el valor mínimo encontrado
      val resultado = rhoAux(dist, p_min)
      math.round(resultado * 1000) / 1000.0
    }

    medidaComete
  }


  def normalizar(m: MedidaPol): MedidaPol = {
    // Define el peor caso de polarización (0.5 en los extremos 0 y 1, y 0 en los otros valores)
    val peorCaso: Frequency = Vector(0.5, 0.0, 0.0, 0.0, 0.5)
    val valoresDistribucion: DistributionValues = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
    // Calcula la polarización máxima
    val maxPolarizacion = m(peorCaso, valoresDistribucion)
    distribucion => {
      val resultado = m(distribucion) / maxPolarizacion
      math.round(resultado * 1000) / 1000.0
    }
  }



}
//
