import scala.annotation.tailrec

object p07 {
  @tailrec
  def processBeams(lines: Seq[String], beams: Set[Int], row: Int = 1, splitCount: Int = 0): Int = {
    // based on word "beam" and example
    // copilot was able to figure out that that I was trying to split something based on ^
    // and generated the core splitting logic.
    // I just had to add the split counting: turned flatMap into foldLeft
    if (row >= lines.length) {
      println("All beams processed.")
      return splitCount
    }
    val line = lines(row)
    val (newBeams, newSplitCount) = beams.foldLeft(Set.empty[Int], 0) { case ((cols, splits), col) =>
      if (line(col) == '^') {
        // Beam hits a mirror, split into two new beams
        val newCols = cols ++ Seq(col - 1, col + 1).filter(c => c >= 0 && c < line.length)
        (newCols, splits + 1)
      } else {
        // Beam continues straight down
        (cols + col, splits)
      }
    }

    println(s"Row $row: Beams at columns ${newBeams.mkString(", ")}; $newSplitCount")

    processBeams(lines, newBeams, row + 1, splitCount + newSplitCount)
  }

  def countTimelines(lines: Seq[String], beam: Int, row: Int = 1, memo: Map[(Int, Int), Long] = Map.empty): (Long, Map[(Int, Int), Long]) = {
    // Copilot got the whole initial recursive structure, except 'beams' shouldn't be a set
    // no deduping here - same column from two different paths counts as two timelines
    // copilot agent could have added memoization myself, but I did it myself to be pedantic about avoiding mutable state
    println(s"Processing column $beam row $row of ${lines.size} beams")

    if (row >= lines.length) {
      return (1L, memo)
    }
    if (memo.contains((beam, row))) {
      return (memo((beam, row)), memo)
    }
    val line = lines(row)
    val totalTimelines = if (line(beam) == '^') {
      // Beam hits a mirror, split into two new beams
      val (leftTimelines, nextMemoL) = if (beam - 1 >= 0) {
        countTimelines(lines, beam - 1, row + 1, memo)
      } else (0L, memo)
      val (rightTimelines, nextMemoR) = if (beam + 1 < line.length) {
        countTimelines(lines, beam + 1, row + 1, nextMemoL)
      } else (0L, nextMemoL)

      val nextCount = leftTimelines + rightTimelines
      (nextCount, nextMemoR + ((beam, row) -> nextCount))
    } else {
      // Beam continues straight down
      countTimelines(lines, beam, row + 1, memo)
    }
    totalTimelines
  }

  def main(args: Array[String]): Unit = {
    val testData = """.......S.......
                     |...............
                     |.......^.......
                     |...............
                     |......^.^......
                     |...............
                     |.....^.^.^.....
                     |...............
                     |....^.^...^....
                     |...............
                     |...^.^...^.^...
                     |...............
                     |..^...^.....^..
                     |...............
                     |.^.^.^.^.^...^.
                     |...............
                     |""".stripMargin

    //val lines = testData.split("\n")
    val lines = scala.io.Source.fromFile("data/p07.txt").getLines().toSeq

    val initialBeam = lines.head.indexOf("S")
    val beams = Set(initialBeam)

    val totalSplits = processBeams(lines, beams, row = 1, splitCount = 0)

    println(totalSplits)

    // part 2
    val totalTimelines = countTimelines(lines, initialBeam, row = 1)
    println(totalTimelines._1)
  }
}
