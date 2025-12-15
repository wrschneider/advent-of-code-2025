import java.awt.Graphics2D

object p09 {
  val testData = """7,1
                   |11,1
                   |11,7
                   |9,7
                   |9,5
                   |2,5
                   |2,3
                   |7,3""".stripMargin

  //val lines = testData.split("\n").toSeq
  val lines = scala.io.Source.fromFile("data/p09.txt").getLines().toSeq
  val points = lines.map { line =>
    line.split(",").map(_.toInt)
  }

  // flipping typical row/col convention b/c input is (x,y) = (col,row)
  val maxC = points.map(_(0)).max
  val maxR = points.map(_(1)).max

  private val edges: Seq[Seq[Array[Int]]] = points.sliding(2).toSeq ++ Seq(Seq(points.last, points.head))
  // build lookup of edges by row and by column for convenience
  // first split edges by vertical/horizontal
  val (v, h) = edges.partition { edge =>
    val c1 = edge(0)(0)
    val c2 = edge(1)(0)
    (c1 == c2)
  }

  private val verticalByCol = v.groupBy(_(0)(0)).mapValues(_.map { edge =>
      val r1 = edge(0)(1)
      val r2 = edge(1)(1)
      (Math.min(r1, r2), Math.max(r1, r2))
    })

  private val horizontalByRow = h.groupBy(_(0)(1)).mapValues(_.map { edge =>
      val c1 = edge(0)(0)
      val c2 = edge(1)(0)
      (Math.min(c1, c2), Math.max(c1, c2))
  })

  verticalByCol.foreach { case (col, edges) =>
    println(s"Vertical edges at col $col: ${edges.map { case (r1, r2) => s"($r1,$r2)" }.mkString(", ")}")
  }
  horizontalByRow.foreach { case (row, edges) =>
    println(s"Horizontal edges at row $row: ${edges.map { case (c1, c2) => s"($c1,$c2)" }.mkString(", ")}")
  }

  def resultQualified(p1: Array[Int], p2: Array[Int]): Boolean = {
    // check if all four corners are inside the shape
    val left = Math.min(p1(0), p2(0))
    val right = Math.max(p1(0), p2(0))
    val top = Math.min(p1(1), p2(1))
    val bottom = Math.max(p1(1), p2(1))

    // see if any edges (line segments) intersect the rectangle defined by p1 and p2
    // check the horizontal ones first
    val horizontalCollision = (top + 1 until bottom).flatMap { row =>
      for {
        horizontalEdgesOnRow <- horizontalByRow.get(row)
      } yield {
        horizontalEdgesOnRow.exists { case (c1, c2) =>
          // check if line segment intersects the rectangle
          (c1 > right && c1 < left) || (c2 > right && c2 < left) || (c1 <= left && c2 >= right)
        }
      }
    }.foldLeft(false)(_ || _)
    val verticalCollision = (left+ 1 until right).flatMap { col =>
      for {
        verticalEdgesOnCol <- verticalByCol.get(col)
      } yield {
        verticalEdgesOnCol.exists { case (r1, r2) =>
          // check if line segment intersects the rectangle
          (r1 > top && r1 < bottom) || (r2 > top && r2 < bottom) || (r1 <= top && r2 >= bottom)
        }
      }
    }.foldLeft(false)(_ || _)
    !horizontalCollision && !verticalCollision
  }

  def main(args: Array[String]): Unit = {

    println(maxR, maxC)

    // Brute force worked for part 1....
    //  take all pairs of points and find max area
    val pairs = points.indices.flatMap { i =>
      points.indices.drop(i + 1).map { j =>
        (points(i), points(j))
      }
    }
    val candidates = pairs.map { case (p1, p2) =>
      val width = Math.abs(p1(0) - p2(0)) + 1
      val height = Math.abs(p1(1) - p2(1)) + 1
      (p1, p2, width.toLong * height.toLong)
    }.sortBy(-_._3)
    val result = candidates.head
    result match {
      case (p1, p2, p3) =>
        println(s"Max area between points (${p1(0)},${p1(1)}) and (${p2(0)},${p2(1)}) = $p3")
    }

    // part 2 - check candidates qualified

    val qualified = candidates.find { case (p1, p2, _) =>
      // special case for part 2 to narrow search space
      // longest horizontal edges at 48450, 50319 - result must either be entirely above or below those lines
      val q =
        (Math.min(p1(1), p2(1)) >= 50319 || Math.max(p1(1), p2(1)) <= 48450) &&
        resultQualified(p1, p2)
      if (!q) {
        println(s"Disqualified area between points (${p1(0)},${p1(1)}) and (${p2(0)},${p2(1)})")
      }
      q
    }

    qualified match {
      case Some((p1, p2, p3)) =>
        println(s"Part 2 - Max qualified area between points (${p1(0)},${p1(1)}) and (${p2(0)},${p2(1)}) = $p3")
      case None =>
        println("No qualified area found")
    }

    // create an SVG file to visualize the shape
    val f = new java.io.File("data/p09_shape.svg")
    // open file for writing
    val pw = new java.io.PrintWriter(f)
    val scale = 50
    pw.println(s"""<svg xmlns="http://www.w3.org/2000/svg" width="${maxC / scale + 2}" height="${maxR / scale + 2}" viewBox="0 0 ${maxC / scale + 2} ${maxR / scale+ 2}">""")
    pw.println(s"""<rect width="100%" height="100%" fill="white"/>""")
    edges.foreach { edge =>
      val x1 = edge(0)(0) / scale
      val y1 = edge(0)(1) / scale
      val x2 = edge(1)(0) / scale
      val y2 = edge(1)(1) / scale
      pw.println(s"""<line x1="${x1 + 1}" y1="${y1 + 1}" x2="${x2 + 1}" y2="${y2 + 1}" stroke="blue" stroke-width="0.5"/>""")
    }
    // add extra rectangle where p1, p2 defines opposite corners
    val (p1, p2, _) = result
    val x1 = p1(0) / scale
    val y1 = p1(1) / scale
    val x2 = p2(0) / scale
    val y2 = p2(1) / scale
    val rectX = Math.min(x1, x2) + 1
    val rectY = Math.min(y1, y2) + 1
    val rectWidth = Math.abs(x1 - x2)
    val rectHeight = Math.abs(y1 - y2)
    pw.println(s"""<rect x="${rectX}" y="${rectY}" width="${rectWidth}" height="${rectHeight}" fill="none" stroke="red" stroke-width="0.5"/>""")

    // add final rectangle for part 2 result if different
    qualified match {
      case Some((qp1, qp2, _)) =>
        val qx1 = qp1(0) / scale
        val qy1 = qp1(1) / scale
        val qx2 = qp2(0) / scale
        val qy2 = qp2(1) / scale
        val qrectX = Math.min(qx1, qx2) + 1
        val qrectY = Math.min(qy1, qy2) + 1
        val qrectWidth = Math.abs(qx1 - qx2)
        val qrectHeight = Math.abs(qy1 - qy2)
        pw.println(s"""<rect x="${qrectX}" y="${qrectY}" width="${qrectWidth}" height="${qrectHeight}" fill="none" stroke="green" stroke-width="0.5"/>""")
      case _ =>
    }
    pw.println("</svg>")
    pw.close()
  }
}
