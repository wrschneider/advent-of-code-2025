import scala.annotation.tailrec

object p11 {
/*

  val testData = """aaa: you hhh
                   |you: bbb ccc
                   |bbb: ddd eee
                   |ccc: ddd eee fff
                   |ddd: ggg
                   |eee: out
                   |fff: out
                   |ggg: out
                   |hhh: ccc fff iii
                   |iii: out""".stripMargin
*/
  val testData = """svr: aaa bbb
                    |aaa: fft
                    |fft: ccc
                    |bbb: tty
                    |tty: ccc
                    |ccc: ddd eee
                    |ddd: hub
                    |hub: fff
                    |eee: dac
                    |dac: fff
                    |fff: ggg hhh
                    |ggg: out
                    |hhh: out""".stripMargin

  def main(args: Array[String]): Unit = {
    //val lines = testData.split("\n").toList
    val lines = scala.io.Source.fromFile("data/p11.txt").getLines().toList

    val graph = lines.map { line =>
      val parts = line.split(":").map(_.trim)
      val node = parts(0)
      val edges = if (parts.length > 1) parts(1).split(" ").map(_.trim).toList else List.empty[String]
      node -> edges
    }.toMap

    // count ALL paths from "you" to "out" - do not need the path, just the count
    def countPaths(current: String, target: String, memo: Map[String, Int] = Map.empty): (Int, Map[String, Int]) = {
      // DAG so don't need to track visited
      //print(s"Visiting $current\n")
      if (current == target) {
        println("Found path to target")
        (1, memo)
      } else if (memo.contains(current)) {
        //println(s"Using cache for $current")
        (memo(current), memo)
      } else {
        val neighbors = graph.getOrElse(current, List.empty)
        val tmp = neighbors.foldLeft((0, memo)) { case ((ct, currMemo), neighbor) =>
          val (result, nextMemo) = countPaths(neighbor, target, currMemo)
          (ct + result, nextMemo + (neighbor -> result))
        }
        tmp
      }

    }

    val p1 = countPaths("svr", "fft")._1.toLong
    println(p1)

    val p2 = countPaths("fft", "dac")._1.toLong
    println(p2)

    val p3 = countPaths("dac", "out")._1.toLong
    println(p3)

    println(p1 * p2 * p3)

  }

}
