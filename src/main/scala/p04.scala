object p04 {

  val directions = List((0,1), (1,0), (0,-1), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1))

  def adjacentCount(grid: Array[Array[Char]], row: Int, col: Int): Int = {
    directions.map { case (dr, dc) =>
      val newRow = row + dr
      val newCol = col + dc
      if (newRow >= 0 && newRow < grid.length && newCol >= 0 && newCol < grid(0).length && grid(newRow)(newCol) == '@') 1 else 0
    }.sum
  }

  def remove(grid: Array[Array[Char]], adjacentCountGrid: IndexedSeq[Array[Int]]): Seq[(Int ,Int)] = {
    val removable = for {
      row <- grid.indices
      col <- grid(0).indices
      canRemove <- if (grid(row)(col) == '@' && adjacentCountGrid(row)(col) < 4) Some((row, col)) else None
    } yield {
      canRemove
    }

    if (removable.isEmpty) {
      Seq.empty
    } else {
      removable.foreach { case (r, c) =>
        // TODO fix this cheating w/mutable state
        grid(r)(c) = '.'
        directions.foreach { case (dr, dc) =>
          val newRow = r + dr
          val newCol = c + dc
          if (newRow >= 0 && newRow < grid.length && newCol >= 0 && newCol < grid(0).length) {
            adjacentCountGrid(newRow)(newCol) -= 1
          }
        }
      }
      // TODO: tail recursion
      // (somehow stack didn't blow up and still sub-second though, so idk)
      removable ++ remove(grid, adjacentCountGrid)
    }
  }

  def main(args: Array[String]): Unit = {
    val testData = """..@@.@@@@.
                     |@@@.@.@.@@
                     |@@@@@.@.@@
                     |@.@@@@..@.
                     |@@.@@@@.@@
                     |.@@@@@@@.@
                     |.@.@.@.@@@
                     |@.@@@.@@@@
                     |.@@@@@@@@.
                     |@.@.@@@.@.""".stripMargin

    //val grid = testData.split("\n").map(_.toCharArray)
    val grid = scala.io.Source.fromFile("data/p04.txt").getLines().map(_.toCharArray).toArray
    val rowCount = grid.length
    val colCount = grid(0).length

    val adjacentCountGrid: IndexedSeq[Array[Int]] = (0 until rowCount).map { r =>
      (0 until colCount).map { c => adjacentCount(grid, r, c) }.toArray
    }

    val removable = remove(grid, adjacentCountGrid)

    println(removable.length)
  }
}
