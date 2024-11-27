import Comete.{DistributionValues, PolMeasure}

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

  type PolMeasure = Distribution => Double


  //Funcion min_p
  @tailrec
  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    if (max - min < prec) (min + max) / 2
    else {
      val tam = (max - min) / 10
      val puntos = (0 to 10).map(i => min + i * tam)
      val indMin = puntos.indices.minBy(i => f(puntos(i)))
      val (izq, der) = (if (indMin > 0) puntos(indMin - 1) else min,
        if (indMin < puntos.size - 1) puntos(indMin + 1) else max)

      if (math.abs((min + max) / 2 - puntos(indMin)) < prec) puntos(indMin)
      else min_p(f, izq, der, prec)
    }
  }

  //Funcion rhoCMT_Gen

  def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {
    (distribucion: (Frequency, DistributionValues))=> {
      val (frecuencias, valores) = distribucion

      def calPaux(p: Double): Double = {
        frecuencias.indices.map { i =>
          val pi = frecuencias(i)
          val yi = valores(i)
          math.pow(pi, alpha) * math.pow(math.abs(yi - p), beta)
        }.sum
      }

      // valor p_min que minimiza rho
      val p_min = min_p(calPaux, 0.0, 1.0, 0.001)
      val resultado = calPaux(p_min)
      math.round(resultado * 1000) / 1000.0
    }
  }

  //Funcion normalizar
  def normalizar(m: PolMeasure): PolMeasure = {
    // Define el peor caso de polarización
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