object p12 {
  val testData = """0:
                   |###
                   |##.
                   |##.
                   |
                   |1:
                   |###
                   |##.
                   |.##
                   |
                   |2:
                   |.##
                   |###
                   |##.
                   |
                   |3:
                   |##.
                   |###
                   |##.
                   |
                   |4:
                   |###
                   |#..
                   |###
                   |
                   |5:
                   |###
                   |.#.
                   |###
                   |
                   |4x4: 0 0 0 0 2 0
                   |12x5: 1 0 1 0 2 2
                   |12x5: 1 0 1 0 3 2""".stripMargin

  def tryPlacement(width: Int, height: Int, shapeCounts: Seq[Int], shapesWithVariants: Seq[Seq[Seq[String]]]): Option[String] = {
    // brute-force try to place the shapes in the grid of width x height
    // shapeCounts gives how many of each shape to place
    // shapeVariants gives all the shapes and their rotated/flipped variants
    // shapeCounts and shapeVariants are aligned by index
    val (shapeHeight, shapeWidth) = (3, 3) // hard-coded

    def canPlaceShapeAt(grid: Array[Array[Char]], shape: Seq[String], top: Int, left: Int): Boolean = {
      if (top + shapeHeight > height || left + shapeWidth > width) return false
      for (r <- 0 until shapeHeight; c <- 0 until shapeWidth) {
        if (shape(r)(c) == '#' && grid(top + r)(left + c) != '.') {
          return false
        }
      }
      true
    }

    def placeShapeAt(grid: Array[Array[Char]], shape: Seq[String], top: Int, left: Int, char: Char): Array[Array[Char]] = {
      val newGrid = grid.map(_.clone())
      for (r <- 0 until shapeHeight; c <- 0 until shapeWidth) {
        if (shape(r)(c) == '#') {
          newGrid(top + r)(left + c) = char
        }
      }
      newGrid
    }

    // brute force!!
    def backtrack(grid: Array[Array[Char]], shapeIdx: Int, remainingCounts: Seq[Int]): Option[Array[Array[Char]]] = {
      if (shapeIdx >= shapeCounts.length) {
        // all shapes placed
        return Some(grid)
      }
      if (remainingCounts(shapeIdx) <= 0) {
        // move to next shape
        return backtrack(grid, shapeIdx + 1, remainingCounts)
      }

      val shapeVariants = shapesWithVariants(shapeIdx)
      for (variant <- shapeVariants) {
        for (r <- 0 to height - shapeHeight) {
          for (c <- 0 to width - shapeWidth) {
            if (canPlaceShapeAt(grid, variant, r, c)) {
              val newGrid = placeShapeAt(grid, variant, r, c, ('A' + shapeIdx).toChar)
              val updatedCounts = remainingCounts.updated(shapeIdx, remainingCounts(shapeIdx) - 1)
              val result = backtrack(newGrid, shapeIdx, updatedCounts)
              if (result.isDefined) {
                return result
              }
            }
          }
        }
      }
      None
    }
    val initialGrid = Array.fill(height, width)('.')
    backtrack(initialGrid, 0, shapeCounts).map { finalGrid =>
      finalGrid.map(_.mkString).mkString("\n")
    }
  }
  def main(args: Array[String]): Unit = {
    //val lines = testData.split("\n").toList
    val lines = scala.io.Source.fromFile("data/p12.txt").getLines().toList

    // parse input lines
    // first section a numeric index followed by colon, then a 3x3 grid of # and . (representing a shape)

    // the sample data and the real data both have 6 shapes numbered 0..5 so we can hard-code
    val (shapesReversed, rls) = (0 to 5).foldLeft((List.empty[Seq[String]], lines)) { case ((shapes, remainingLines), i) =>
      remainingLines match {
        case header :: grid1 :: grid2 :: grid3 :: rest if header.startsWith(s"$i:") =>
          val shape = Seq(grid1, grid2, grid3)
          (shape :: shapes, rest.tail)
        case _ =>
          (shapes, remainingLines.tail) // consume blank line before next shape
      }
    }
    val placements = rls.map { line =>
        val parts = line.split(":").map(_.trim)
        val sizePart = parts(0)
        val shapeIndicesPart = parts(1)
        val sizeParts = sizePart.split("x").map(_.trim)
        val width = sizeParts(0).toInt
        val height = sizeParts(1).toInt
        val shapeIndices = shapeIndicesPart.split(" ").map(_.trim).map(_.toInt).toSeq
        (width, height, shapeIndices)
    }

    val shapesWithVariants = shapesReversed.reverse.map { shape =>
      // generate all 4 rotations
      def rotate90(s: Seq[String]): Seq[String] = {
        val rows = s.length
        val cols = s(0).length
        (0 until cols).map { c =>
          (0 until rows).reverse.map { r =>
            s(r)(c)
          }.mkString
        }
      }
      def flip(s: Seq[String]): Seq[String] = {
        s.map(_.reverse)
      }
      val r0 = shape
      val r1 = rotate90(r0)
      val r2 = rotate90(r1)
      val r3 = rotate90(r2)
      // also account for flipped versions?
      val r0f = flip(r0)
      val r1f = rotate90(r0f)
      val r2f = rotate90(r1f)
      val r3f = rotate90(r2f)
      Seq(r0, r1, r2, r3, r0f, r1f, r2f, r3f).distinct
    }

    shapesWithVariants.zipWithIndex.foreach { case (variants, shapeIdx) =>
      println(s"Shape $shapeIdx has ${variants.size} variants:")
      variants.zipWithIndex.foreach { case (variant, varIdx) =>
        println(s" Variant $varIdx:")
        variant.foreach { row =>
          println(s"  $row")
        }
      }
    }

    val results = placements.map { case(width, height, shapeCounts) =>
      // fail early check - impossible because more spaces occupied than available on grid even with perfect packing
      // got lucky - brute force worked WITH this fail-early check but would have failed otherwise
      val totalSpacesOccupied = shapeCounts.zipWithIndex.map { case (count, shapeIdx) =>
        val nonEmptySpaces = shapesWithVariants(shapeIdx).head.map(_.count(_ == '#')).sum
        count * nonEmptySpaces // each shape is 3x3
      }.sum
      if (totalSpacesOccupied > width * height) {
        println(s"skipping placement ${width}x${height} with shapes ${shapeCounts.mkString(" ")} due to early failure check")
        None
      } else {
        println(s"trying placement ${width}x${height} with shapes ${shapeCounts.mkString(" ")}")
        tryPlacement(width, height, shapeCounts, shapesWithVariants)
      }
    }

    println(results.count(_.isDefined))
  }
}
