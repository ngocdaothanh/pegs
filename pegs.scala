object Pegs {
  def main(args: Array[String]) {
    val x = args(0).toInt
    val y = args(1).toInt
    play2(x, y)
  }

  def isIn(x: Int, y: Int) = (x >= 1 && y >= 1 && (x + y) <= 6)

  // x, y: hole
  def makeMap(x: Int, y: Int) = {
    var ret = Map[(Int, Int), Boolean]()
    for (i <- 1 to 5; j <- 1 to 5) {
      if (isIn(i, j)) {
        val isHole = (i == x && j == y)
        ret += ((i, j) -> isHole)
      }
    }
    ret
  }
  
  // x, y: hole
  def findMovablePegs(map: Map[(Int, Int), Boolean], x: Int, y: Int): Array[(Int, Int)] = {
    var ret = Array[(Int, Int)]()
  
    if (isIn(x - 2, y) && isIn(x - 1, y) && !map(x - 2, y) && !map(x - 1, y))
      ret ++= Array((x - 2, y))
    if (isIn(x + 2, y) && isIn(x + 1, y) && !map(x + 2, y) && !map(x + 1, y))
      ret ++= Array((x + 2, y))
  
    if (isIn(x, y - 2) && isIn(x, y - 1) && !map(x, y - 2) && !map(x, y - 1))
      ret ++= Array((x, y - 2))
    if (isIn(x, y + 2) && isIn(x, y + 1) && !map(x, y + 2) && !map(x, y + 1))
      ret ++= Array((x, y + 2))
  
    if (isIn(x - 2, y + 2) && isIn(x - 1, y + 1) && !map(x - 2, y + 2) && !map(x - 1, y + 1))
      ret ++= Array((x - 2, y + 2))
    if (isIn(x + 2, y - 2) && isIn(x + 1, y - 1) && !map(x + 2, y - 2) && !map(x + 1, y - 1))
      ret ++= Array((x + 2, y - 2))
    ret
  }
  
  def findHoles(map: Map[(Int, Int), Boolean]) = {
    map.foldLeft(Array[(Int, Int)]()) { (acc, kv) =>
      val (position, isHole) = kv
      if (isHole) acc ++ Array(position) else acc
    }
  }
  
  def hasWon(map: Map[(Int, Int), Boolean]) = {
    val numPegs = map.count { case (position, isHole) => !isHole }
    numPegs == 1
  }
  
  // x, y: hole
  // px, py: peg
  def move(map: Map[(Int, Int), Boolean], x: Int, y: Int, px: Int, py: Int) = {
    val m2 = map.updated((px, py), true)
    val m3 = m2.updated(((x + px)/2, (y + py)/2), true)
    val m4 = m3.updated((x, y), false)
    m4
  }
  
  def printSteps(steps: Array[((Int, Int), (Int, Int))]) {
    for (s <- steps) {
      val (from, to) = s
      println(from + " -> " + to)
    }
  }
  
  def play1(map: Map[(Int, Int), Boolean], x: Int, y: Int, steps: Array[((Int, Int), (Int, Int))]) {
    val pegs = findMovablePegs(map, x, y)
    if (pegs.size > 0) {
      for ((px, py) <- pegs) {
        val m2 = move(map, x, y, px, py)
        if (hasWon(m2)) {
          printSteps(steps)
          exit
        } else {
          val holes = findHoles(m2)
          for ((i, j) <- holes) {
            val step = ((px, py), (x, y))
            play1(m2, i, j, steps ++ Array(step))
          }
        }
      }
    }
  }
  
  def play2(x: Int, y: Int) {
    val m = makeMap(x, y)
    play1(m, x, y, Array())
  }
}
