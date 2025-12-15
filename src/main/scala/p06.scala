object p06 {
  def main(args: Array[String]): Unit = {
    val testData = """123 328  51 64
                     | 45 64  387 23
                     |  6 98  215 314
                     |*   +   *   +  """.stripMargin

    //val lines = testData.split("\n")
    val lines = scala.io.Source.fromFile("data/p06.txt").getLines().toSeq

    val longestLine = lines.map(_.length).max

    val columns = (0 until longestLine).reverse.map {
      col => lines.map(row => if (col < row.length) row(col) else " ").mkString
    }

    val results = columns.foldLeft((List.empty[Long], List.empty[Long])) { case ((results, usedCols), currCol) =>
      if (currCol.matches("^\\s+$")) {
        (results, usedCols)
      } else {
        val op = currCol.last
        val num = currCol.dropRight(1).trim.toLong
        println(num, op)
        val nextUsedCols = num :: usedCols
        op match {
          case '*' => (nextUsedCols.product :: results, List.empty[Long])
          case '+' => (nextUsedCols.sum :: results, List.empty[Long])
          case _ => (results, nextUsedCols)
        }
      }
    }

    results._1.foreach(println)

    println(results._1.sum)
  }

  //10695785245070 too low
}
