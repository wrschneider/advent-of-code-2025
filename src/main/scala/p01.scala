object p01 {

  val testData = """L68
                   |L30
                   |R48
                   |L5
                   |R60
                   |L55
                   |L1
                   |L99
                   |R14
                   |L82""".stripMargin

  def main(args: Array[String]): Unit = {
    //val lines = testData.split("\n")
    val lines = scala.io.Source.fromFile("data/p01.txt").getLines().toSeq
    val result = lines.foldLeft((50, 0)) { case ((pos, zeroCount), line) =>
      val nextPosRaw = line.drop(1).toInt match {
        case value if line.head == 'L' => pos - value
        case value if line.head == 'R' => pos + value
      }
      val zeroCrossing = if (line.head == 'L' && nextPosRaw <= 0) {
        (if (pos == 0) 0 else 1) + (nextPosRaw / -100)
      } else {
        nextPosRaw / 100
      }
      val nextPos = if (nextPosRaw < 0 && nextPosRaw % 100 != 0) {
        100 + (nextPosRaw % 100)
      } else {
        nextPosRaw % 100
      }

      println(s"Instruction: $line, Current Position: $pos, Next Position: $nextPos, Zero Crossings This Step: $zeroCrossing")
      (nextPos, zeroCrossing + zeroCount)
    }
    // 6794 too high
    println(s"Final Position: ${result._1}, Zero Crossings: ${result._2}")
  }
}
