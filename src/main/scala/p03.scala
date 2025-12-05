object p03 {

  def maxJoltage(s: String, remainingDigits: Int): String = {
    if (remainingDigits == 0) return ""
    // TODO: rewrite with tail recursion

    // find max digit and position in s ensuring there are at least N-1 digits remaining
    val maxDigit: Char = s.dropRight(remainingDigits - 1).max
    // find first position of that digit
    val pos = s.indexOf(maxDigit)
    // now find largest remaining N-1 digits to right of pos
    val nextDigits = maxJoltage(s.drop(pos + 1), remainingDigits - 1)
    val result: String = maxDigit + nextDigits
    result
  }

  def main(args: Array[String]): Unit = {
    val testData = """987654321111111
                     |811111111111119
                     |234234234234278
                     |818181911112111""".stripMargin

    // val lines = testData.split("\n")
    val lines = scala.io.Source.fromFile("data/p03.txt").getLines().toSeq
    val results = lines.map(maxJoltage(_, 12).mkString.toLong)
    results.foreach(println)
    println(results.sum)
  }

}
