import scala.annotation.tailrec

object p10 {
  val testData =
    """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin

  def minPresses(finalState: Seq[Boolean], buttons: Seq[Seq[Int]]): Int = {
    // brute force all combinations of button presses
    val n = buttons.length
    val totalCombinations = 1 << n // 2^n combinations
    val pressCounts = (0 until totalCombinations).flatMap { combination =>
      // TODO make this immutable
      val currentState = Array.fill(finalState.length)(false)
      var presses = 0

      for (btnIdx <- 0 until n) {
        if (((combination >> btnIdx) & 1) == 1) {
          // press this button
          presses += 1
          for (lightIdx <- buttons(btnIdx)) {
            currentState(lightIdx) = !currentState(lightIdx)
          }
        }
      }
      if (currentState.zip(finalState).forall { case (a, b) => a == b }) {
        Some(presses)
      } else None
    }
    pressCounts.min
  }

  def minPressesForTotal(buttons: Seq[Seq[Int]], target: Seq[Int]): Option[Int] = {
    // this is an ops research linear optimization problem with Integer programming
    // minimize sum(presses(i)) for i in buttons.length
    // subject to sum (presses(j) * (buttons[j] contains k ? 1 : 0))  == target(k) for all k in target.length
    // and presses(i) >= 0 and integer

    // BFS works on smaller scale but blows up when values of target(k) > 10 or so
    // try greedy DFS (longest buttons and most constrained buttons, most presses)

    def dfs(buttons: List[Seq[Int]], remainingTarget: Seq[Int], currentPresses: Int, bestSoFar: Option[Int]): Option[Int] = {

      if (remainingTarget.forall(_ == 0)) {
        println(s"found result at $currentPresses")
        return Some(currentPresses)
      }
      if (buttons.isEmpty || remainingTarget.exists(_ < 0)) {
        // no buttons left to press but target not met
        return None
      }
      if (bestSoFar.isDefined && currentPresses + remainingTarget.max > bestSoFar.get) {
        // println(s"pruning: currentPresses $currentPresses + ${remainingTarget.max} > bestSoFar ${bestSoFar.get}")
        return None
      }

      val remainingButtons = buttons

      // other dead-end checks: make sure that for each remaining target, there is at least one button that can press it
      val nonZeroTargets = remainingTarget.indices.filter { idx => remainingTarget(idx) > 0 }
      val buttonsForTarget = nonZeroTargets.map { tIdx =>
        (remainingButtons.indices.filter { btn => remainingButtons(btn).contains(tIdx) }, remainingTarget(tIdx))
      }
      if (buttonsForTarget.exists(_._1.isEmpty)) {
        // at least one target cannot be reached with remaining buttons
        // println(s"dead end: at least one target cannot be reached with remaining buttons")
        return None
      }
      // now find forced moves: targets that can only be reached by one button, and press all such buttons
      // exact number of times
      // one button might have been a singleton for multiple targets, though so don't double-count
      val singletons = buttonsForTarget.filter(_._1.length == 1).map(t => (t._1.head, t._2))
        .groupBy(_._1).mapValues(entries => entries.map(_._2).min) // button idx -> presses needed

      if (singletons.nonEmpty) {
        val nextPresses = currentPresses + singletons.values.sum
        if (bestSoFar.isDefined && nextPresses > bestSoFar.get) {
          return None
        }
        //println(s"found forced moves: $singletons")
        val nextState = singletons.foldLeft(remainingTarget) { case (currTarget, (btnIdx, pressesNeeded)) =>
          currTarget.zipWithIndex.map { case (value, idx) =>
            if (remainingButtons(btnIdx).contains(idx)) value - pressesNeeded else value
          }.toList
        }
        // remove used buttons
        val nextButtons = remainingButtons.zipWithIndex.filterNot { case (_, idx) =>
          singletons.exists(_._1 == idx)
        }.map(_._1)
        return dfs(nextButtons, nextState, nextPresses, bestSoFar)
      }

      // no singletons, so take the button that is closest to yielding in a singleton

      val buttonIndex = buttonsForTarget.minBy(_._1.length)._1.head
      val currentButton = remainingButtons(buttonIndex)
      val restButtons = remainingButtons.zipWithIndex.filterNot(_._2 == buttonIndex).map(_._1)
      val maxPossiblePresses = currentButton.map(remainingTarget).min // pressing more than this overshoots at least one target

      val range = (0 to maxPossiblePresses).reverse

      val possibleResults = range.foldLeft(List.empty[Int]) { case (acc, n) =>
        // pressing the button n times
        val newTarget = remainingTarget.zipWithIndex.map { case (value, idx) =>
          if (currentButton.contains(idx)) value - n else value
        }
        if (newTarget.exists(_ < 0)) {
          acc
        } else {
          // best result so far can update during iteration
          val nextBest = if (acc.isEmpty) bestSoFar
            else if (bestSoFar.isEmpty) Some(acc.min)
            else Some(Math.min(acc.min, bestSoFar.get))
          dfs(restButtons, newTarget, currentPresses + n, nextBest) match {
            case Some(result) => result :: acc
            case None => acc
          }
        }
      }
      if (possibleResults.isEmpty)
        None
      else {
        Some(possibleResults.min)
      }
    }
    println(s"*** starting $target")
    dfs(buttons.toList, target, 0, None)
  }

  def main(args: Array[String]): Unit = {
    //val lines = testData.split("\n")
    val lines = scala.io.Source.fromFile("data/p10.txt").getLines().toSeq

    // format:
    // [final state] is in square braces (# = on, . = off)
    // all (comma separated) lists in parens are "buttons"
    // the number of buttons is variable - can be any number of buttons per line
    // the curly braces is a single list of "joltage requirements"

    val parsed = lines.map { line =>
      val pattern = """\[(.*?)\](.*)\{(.*)\}""".r
      val pattern(finalStateStr, buttonsStr, joltageStr) = line

      val finalState = finalStateStr.map {
        case '#' => true
        case '.' => false
      }

      val buttons = buttonsStr.trim.split("\\)").filter(_.nonEmpty).map { btnStr =>
        val cleaned = btnStr.trim.stripPrefix("(").trim
        cleaned.split(",").map(_.toInt).toSeq
      }.toSeq

      val joltageReqs = joltageStr.trim.split(",").map(_.toInt).toSeq

      (finalState, buttons, joltageReqs)
    }
    val t1 = System.currentTimeMillis()
    //val presses = parsed.map(problem => minPresses(problem._1, problem._2))
    val presses = parsed.flatMap { case (finalState, buttons, joltageReqs) =>
      val result = minPressesForTotal(buttons, joltageReqs)
      println(result)
      result
    }.toList
    presses.foreach(println)
    println(presses.sum)
    val t2 = System.currentTimeMillis()
    println(s"Time taken: ${(t2 - t1) / 1000.0} seconds") // 22 mins, not great, but not sure what I missed
  }
}
