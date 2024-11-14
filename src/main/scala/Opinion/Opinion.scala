import Comete._
import common._
import scala.collection.parallel.CollectionConverters._

package object Opinion {
  // Si n es el número de agentes, estos se identifican
  // con los enteros entre 0 y n−1.
  // O sea, el conjunto de Agentes A es implícitamente el conjunto {0,1,2,...,n−1}

  // Si b: BeliefConf, para cada i en Int, b[i] es un número
  // entre 0 y 1 que indica cuánto cree el agente i en
  // la veracidad de la proposición p.
  // Si existe i : b(i) < 0 o b(i) > 1, b está mal definida.

  type SpecificBelief = Vector[Double]

  // Si b: SpecificBelief, para cada i en Int, b[i] es un número
  // entre 0 y 1 que indica cuánto cree el
  // agente i en la veracidad de la proposición p.
  // El número de agentes es b.length.
  // Si existe i : b(i) < 0 o b(i) > 1, b está mal definida.
  // Para i en Int\A, b(i) no tiene sentido.

  type GenericBeliefConf = Int => SpecificBelief

  // Si gb: GenericBelief, entonces gb(n) = b tal que
  // b: SpecificBelief.

  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double

  // Si rho: AgentsPolMeasure y sb: SpecificBelief
  // y d: DistributionValues,
  // rho(sb, d) es la polarización de los agentes
  // de acuerdo a esa medida.

  // Funcion Rho

//  def rho(alpha:Double, beta:Double):AgentsPolMeasure = {
//    def contarCreenciasEnIntervalos(creencias:SpecificBelief, posiblesValores:DistributionValues):SpecificBelief = {
//      //Genera los intervalos basados en posiblesValores
//      val k = posiblesValores.length
//      val intervalos = (0 until k).map {
//        case 0 => (0.0, (posiblesValores(1)/2))
//        case i if i == k - 1 => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, 1.0)
//        case i => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, (posiblesValores(i) + posiblesValores(i + 1)) / 2)
//      }.toVector
//
//      //Cuenta cuántas creencias caen en cada intervalo
//      intervalos.zipWithIndex.map { case ((limInf, limSup), indx) =>
//        if (indx == k - 1)
//          creencias.count(x => x >= limInf && x <= limSup)  //Último intervalo incluye 1.0
//        else
//          creencias.count(x => x >= limInf && x < limSup)
//      }
//    }
//    def construirDistribucion(creencias:SpecificBelief, values:DistributionValues):Frequency = {
//      //Se cuentan las creencias en intervalos
//      val creenciasCategorizadas = contarCreenciasEnIntervalos(creencias, values)
//      //Obtenemos el total de creencias (n agentes)
//      val totalCreencias = creencias.length.toDouble
//      //Calculamos la frecuencia dividiendo cada elemento de creenciasCategorizadas entre el total
//      val frecuencia = creenciasCategorizadas.map(_ / totalCreencias)
//      frecuencia
//    }
//    (creencias:SpecificBelief, values:DistributionValues) => {
//      val cmt = rhoCMT_Gen(alpha, beta)
//      val distribucion = construirDistribucion(creencias, values)
//      val cmt1norm = normalizar(cmt)
//      cmt1norm(distribucion, values)
//    }
//  }


  // Tipos para modelar la evolución de la opinión en una red
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph = Int => SpecificWeightedGraph

  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
  ...
  }

  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
  ...
  }

  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {
  ...
  }

  // Versiones paralelas
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarización de agentes basada en comete
  ...
  }

  def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
  ...
  }
}
