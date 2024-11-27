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

  type GenericBelief = Int => SpecificBelief
  // Si gb: GenericBelief, entonces gb(n) = b tal que
  // b: SpecificBelief.

  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double
  // Si rho: AgentsPolMeasure y sb: SpecificBelief
  // y d: DistributionValues,
  // rho(sb, d) es la polarización de los agentes
  // de acuerdo a esa medida.

  // Funcion Rho

  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    def contarCreenciasEnIntervalos(creencias: SpecificBelief, posiblesValores: DistributionValues): SpecificBelief = {
      //Genera los intervalos basados en posiblesValores
      val k = posiblesValores.length
      val intervalos = (0 until k).map {
        case 0 => (0.0, (posiblesValores(1) / 2))
        case i if i == k - 1 => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, 1.0)
        case i => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, (posiblesValores(i) + posiblesValores(i + 1)) / 2)
      }.toVector

      //Cuenta cuántas creencias caen en cada intervalo
      intervalos.zipWithIndex.map { case ((limInf, limSup), indx) =>
        if (indx == k - 1)
          creencias.count(x => x >= limInf && x <= limSup) //Último intervalo incluye 1.0
        else
          creencias.count(x => x >= limInf && x < limSup)
      }
    }

    def construirDistribucion(creencias: SpecificBelief, values: DistributionValues):
    Frequency = {
      //Se cuentan las creencias en intervalos
      val creenciasCategorizadas = contarCreenciasEnIntervalos(creencias, values)
      //Obtenemos el total de creencias (n agentes)
      val totalCreencias = creencias.length.toDouble
      //Calculamos la frecuencia dividiendo cada elemento de creenciasCategorizadas entre el total
      val frecuencia = creenciasCategorizadas.map(_ / totalCreencias)
      frecuencia
    }

    (creencias: SpecificBelief, values: DistributionValues) => {
      val cmt = rhoCMT_Gen(alpha, beta)
      val distribucion = construirDistribucion(creencias, values)
      val cmt1norm = normalizar(cmt)
      val resultado = cmt1norm(distribucion, values)
      math.round(resultado * 1000) / 1000.0
    }
  }


  // Tipos para modelar la evolución de la opinión en una red
  // Para implementar la funcion de influencia I
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph = Int => SpecificWeightedGraph

  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

  //Funcion confBiasUpdate
  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val n = sb.length
    val (weightFunction, _) = swg // Extrae la función de peso de la tupla
    // Actualiza la creencia de cada agente aplicando el sesgo de confirmación
    Vector.tabulate(n) { i =>
      // Calcula el valor de ajuste para cada agente j que influye sobre el agente i
      val influencias = for {
        j <- 0 until n if weightFunction(j, i) > 0
      } yield {
        val beta_ij = 1 - math.abs(sb(j) - sb(i))
        beta_ij * weightFunction(j, i) * (sb(j) - sb(i))
      }
      // Verifica si hay influencias para evitar divisiones por cero
      if (influencias.isEmpty) sb(i) // Sin influencias, conserva la creencia original
      else sb(i) + influencias.sum / influencias.size
    }
  }

  //Funcion showWeightedGraph
  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    //Extrae la función de influencia y el número de agentes del SpecificWeightedGraph
    val (graphFunc, nags) = swg
    //Crea una matriz de influencias usando dos bucles for, donde:
    // - El bucle externo itera sobre el índice i, que representa el agente de origen
    // - El bucle interno itera sobre el índice j, que representa el agente de destino
    // Para cada par (i, j), calcula la influencia usando graphFunc(i, j) y almacena el resultado en la matriz.
    for (i <- 0 until nags) yield {
      for (j <- 0 until nags) yield graphFunc(i, j)
    }
  }



  // Funcion simulate
  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {
    //Genera una secuencia de creencias usando iteración sobre los pasos de tiempo
    (0 until t).foldLeft(IndexedSeq(b0)) { (creencias, _) =>
      //Accede a la ultima creencia calculada en la secuencia
      val ultimaCreencia = creencias.last
      //Aplica la función de actualización (fu) a la última creencia para obtener la nueva creencia
      //demas se crea una nueva secuencia añadiendo la nueva creencia al final de la secuencia existente
      creencias :+ fu(ultimaCreencia, swg)
    }
  }

  // Versiones paralelas

  // Funcion rhoPar
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {

    def contarCreenciasEnIntervalos(creencias: SpecificBelief, posiblesValores: DistributionValues): SpecificBelief = {
      val k = posiblesValores.length

      // Se divide para generar intervalos en dos tareas
      val mitad = k / 2
      val (intervalosIzq, intervalosDer) = parallel(
        // Tarea 1: Primera mitad de intervalos
        (0 until mitad).map {
          case 0 => (0.0, (posiblesValores(1) / 2))
          case i => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, (posiblesValores(i) + posiblesValores(i + 1)) / 2)
        },
        // Tarea 2: Segunda mitad de intervalos
        (mitad until k).map {
          case i if i == k - 1 => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, 1.0)
          case i => ((posiblesValores(i - 1) + posiblesValores(i)) / 2, (posiblesValores(i) + posiblesValores(i + 1)) / 2)
        }
      )

      val intervalos = (intervalosIzq ++ intervalosDer).toVector

      intervalos.zipWithIndex.map { case ((limInf, limSup), indx) =>
        if (indx == k - 1)
          creencias.count(x => x >= limInf && x <= limSup) // Último intervalo incluye 1.0
        else
          creencias.count(x => x >= limInf && x < limSup)
      }
    }
    // Función para construir la distribución
    def construirDistribucion(creencias: SpecificBelief, values: DistributionValues): Frequency = {
      val creenciasCategorizadas = contarCreenciasEnIntervalos(creencias, values)
      val totalCreencias = creencias.length.toDouble

      // Dividimos el cálculo de frecuencias en dos tareas paralelas
      val mitad = creenciasCategorizadas.length / 2
      val (freqIzq, freqDer) = parallel(
        creenciasCategorizadas.slice(0, mitad).map(_ / totalCreencias),
        creenciasCategorizadas.slice(mitad, creenciasCategorizadas.length).map(_ / totalCreencias)
      )

      freqIzq ++ freqDer
    }

    // Función principal con paralelización de tareas
    (creencias: SpecificBelief, values: DistributionValues) => {

      val (cmt, distribucion) = parallel(
        task { rhoCMT_Gen(alpha, beta) }, // Cálculo de la medida Comete
        task { construirDistribucion(creencias, values) } // Cálculo de la distribución
      )

      val cmt1norm = normalizar(cmt.join())
      val resultado = cmt1norm(distribucion.join(), values)

      math.round(resultado * 1000) / 1000.0
    }
  }

  //Funcion confBiasUpdatePar
  def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    // Extrae la función de peso del SpecificWeightedGraph
    val (weightFunction, _) = swg

    // Define una función para calcular la nueva creencia de un agente
    def actualizarCreenciaParaAgente(i: Int): Double = {
      val influencias = for {
        j <- sb.indices if weightFunction(j, i) > 0
      } yield {
        val beta_ij = 1 - math.abs(sb(j) - sb(i))
        beta_ij * weightFunction(j, i) * (sb(j) - sb(i))
      }
      // Calcula la nueva creencia
      if (influencias.isEmpty) sb(i) else sb(i) + influencias.sum / influencias.size
    }
    // Dividimos las tareas en dos mitades y ejecutamos en paralelo
    val (parte1, parte2) = parallel(
      // Primera mitad con paralelización de datos
      sb.indices.take(sb.length / 2).par.map(actualizarCreenciaParaAgente).toVector,
      // Segunda mitad con paralelización de datos
      sb.indices.drop(sb.length / 2).par.map(actualizarCreenciaParaAgente).toVector
    )
    // Combina los resultados de las partes
    parte1 ++ parte2
  }

}
