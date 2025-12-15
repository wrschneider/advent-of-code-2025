object p05 {

  def main(args: Array[String]): Unit = {
    val testData = """3-5
                     |10-14
                     |16-20
                     |12-18
                     |
                     |1
                     |5
                     |8
                     |11
                     |17
                     |32""".stripMargin

    //val data = testData
    val data = scala.io.Source.fromFile("data/p05.txt").getLines().mkString("\n")
    val ranges = data.split("\n\n")(0)
    val ingredients = data.split("\n\n")(1)

    val rangeList = ranges.split("\n").map { line =>
      val parts = line.split("-")
      (parts(0).toLong, parts(1).toLong)
    }.toList

    val ingredientList = ingredients.split("\n").map(_.toLong).toList

    val result = ingredientList.filter { ingredient =>
      rangeList.exists { case (start, end) =>
        ingredient >= start && ingredient <= end
      }
    }

    result.foreach(println)
    println(result.size)

    // part 2
    val sortedRanges = rangeList.sortBy(_._1)
    val condensed = sortedRanges.foldLeft(List.empty[(Long, Long)]) { (processedRanges, currentRange) =>
      val (start, end) = currentRange
      val mergedRanges = processedRanges match {
        case (headStart, headEnd) :: tail if start <= headEnd + 1 => (headStart, Math.max(headEnd, end)) :: tail
        // this handles both Nil case and non-overlapping case
        case _ => currentRange :: processedRanges
      }
      mergedRanges
    }
    println(condensed.map(r => r._2 - r._1 + 1).sum)
  }

}
