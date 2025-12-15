import scala.annotation.tailrec

object p08 {
  val testData = """162,817,812
                   |57,618,57
                   |906,360,560
                   |592,479,940
                   |352,342,300
                   |466,668,158
                   |542,29,236
                   |431,825,988
                   |739,650,466
                   |52,470,668
                   |216,146,977
                   |819,987,18
                   |117,168,530
                   |805,96,715
                   |346,949,466
                   |970,615,88
                   |941,993,340
                   |862,61,35
                   |984,92,344
                   |425,690,689""".stripMargin

  //val lines = testData.split("\n")
  val lines = scala.io.Source.fromFile("data/p08.txt").getLines().toSeq
  val limit = 1000
  val numbers = lines.map { line =>
    line.split(",").map(_.toLong)
  }
  val distanceMatrix: Seq[((Int, Int), Long)] = numbers.indices.flatMap { i =>
    numbers.indices.drop(i + 1).map { j =>
      val dx = numbers(i)(0) - numbers(j)(0)
      val dy = numbers(i)(1) - numbers(j)(1)
      val dz = numbers(i)(2) - numbers(j)(2)
      val d2 = dx * dx + dy * dy + dz * dz
      (i, j) -> d2
    }
  }.sortBy(_._2)

  @tailrec
  def processCircuits(idx: Int, circuits: Seq[Set[Int]]): (Int, Int) = {
    println(s"Processing idx $idx with ${circuits.size} circuits")
    //if (idx > limit) return circuits

    val ((i, j), _) = distanceMatrix(idx)

    val circuitI = circuits.indexWhere(_.contains(i))
    val circuitJ = circuits.indexWhere(_.contains(j))
    val (nextCircuits, updatedCircuit) = (circuitI, circuitJ) match {
      case (ci, cj) if ci != cj && ci >= 0 && cj >= 0 =>
        // merge circuits
        val merged = circuits(ci) union circuits(cj)
        (circuits.indices.filterNot(idx => idx == ci || idx == cj).map(circuits) :+ merged, merged)

      case (ci, cj) if ci == cj && ci >= 0 =>
        // both already in same circuit, do nothing
        (circuits, circuits(ci))
      case (ci, -1) if ci >= 0 =>
        val updatedCircuit = circuits(ci) + j
        (circuits.indices.map { idx => if (idx == ci) updatedCircuit else circuits(idx) }, updatedCircuit)

      case (-1, cj) if cj >= 0 =>
        val updatedCircuit = circuits(cj) + i
        (circuits.indices.map { idx => if (idx == cj) updatedCircuit else circuits(idx) }, updatedCircuit)

      case (-1, -1) =>
        val newCircuit = Set(i, j)
        (circuits :+ newCircuit, newCircuit)
    }

    if (updatedCircuit.size >= numbers.length) {
      (i, j)
    } else {
      processCircuits(idx + 1, nextCircuits)
    }
  }

  def main(args: Array[String]): Unit = {

    //println(distanceMatrix)

    val (i, j) = processCircuits(0, Seq.empty) // initialize

    /*
    // now we're done
    val largest3 = circuits.map(_.size).sorted.takeRight(3)
    println(largest3)
    println(largest3.product)
     */

    println(numbers(i)(0) * numbers(j)(0))
  }
}
