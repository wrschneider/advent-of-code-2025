object p02 {
  def invalid(num: Long): Boolean = {
    val str = num.toString
    val parts = (2 to str.length).filter(str.length % _ == 0)
    parts.exists(p => (1 until p).forall(i => {
      val partLength = str.length / p
      val part = str.substring(i * partLength, (i + 1) * partLength)
      part == str.substring(0, partLength)
    }))
  }
  def invalidInRange(rangeStr: String): Seq[Long] = {
    val parts = rangeStr.split("-").map(_.trim)
    val (start, end) = (parts(0).toLong, parts(1).toLong)
    (start to end).filter(invalid)
  }

  def main(args: Array[String]): Unit = {
    val testData =
      """
        |11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
        |1698522-1698528,446443-446449,38593856-38593862,565653-565659,
        |824824821-824824827,2121212118-2121212124
        |""".stripMargin

    val input = scala.io.Source.fromFile("data/p02.txt").getLines().mkString.trim
    // val input = testData
    val data = input.split(",").map(_.trim)
    val result = data.flatMap(rangeStr => invalidInRange(rangeStr)).sum
    print(result)

  }
}
