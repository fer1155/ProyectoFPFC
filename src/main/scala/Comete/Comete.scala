import scala.annotation.tailrec
import scala.math.BigDecimal.double2bigDecimal

package object Comete {

  type DistributionValues = Vector[Double]

  type Frequency = Vector[Double]

  type Distribution = (Frequency, DistributionValues)

  type MedidaPol = Distribution => Double


////  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
////    if (max - min < prec) (min + max) / 2
////    else {
////      // Divide el intervalo en 10 partes iguales
////      val division = (max - min) / 10
////      val puntosDivision = (0 to 10).map(i => min + i * division).toList
////
////      // Encuentra el índice del punto donde f es mínimo
////      val puntoMinIndex = puntosDivision.indices.minBy(i => f(puntosDivision(i)))
////
////      // Identifica el predecesor y el sucesor del punto mínimo
////      val predecesor = if (puntoMinIndex > 0) puntosDivision(puntoMinIndex - 1) else min
////      val sucesor = if (puntoMinIndex < puntosDivision.size - 1) puntosDivision(puntoMinIndex + 1) else max
////
////      // Si la diferencia entre el mínimo y el punto medio es suficientemente pequeña, retorna el mínimo
////      if (math.abs((min + max) / 2 - puntosDivision(puntoMinIndex)) < prec) puntosDivision(puntoMinIndex)
////      else min_p(f, predecesor, sucesor, prec) // Llamada recursiva
////    }
////  }
/////*
//
//  //Funcion Auxiliar
//  def rhoAux(p: Double, freq: Frequency, values: DistributionValues, alpha: Double, beta: Double): Double = {
//    (freq zip values).map { case (pi, yi) =>
//      math.pow(pi, alpha) * math.pow(math.abs(yi - p), beta)
//    }.sum
//  }
//  // Función principal para calcular la medida de polarización comete
//  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
//    (distribution: Distribution) => {
//      val (freq, values) = distribution
//      // Función de rhoAux que toma p como variable
//      val f = (p: Double) => rhoAux(p, freq, values, alpha, beta)
//      //Intervalo [0, 1] con precisión 0.01
//      val pmin = min_p(f, 0.0, 1.0, 0.01)
//      f(pmin)
//    }
//  }
//
    def normalizar(m: MedidaPol): MedidaPol = {
//
//
    }
 }
//
