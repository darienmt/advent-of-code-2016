val input = "R3, L5, R2, L2, R1, L3, R1, R3, L4, R3, L1, L1, R1, L3, R2, L3, L2, R1, R1, L1, R4, L1, L4, R3, L2, L2, R1, L1, R5, R4, R2, L5, L2, R5, R5, L2, R3, R1, R1, L3, R1, L4, L4, L190, L5, L2, R4, L5, R4, R5, L4, R1, R2, L5, R50, L2, R1, R73, R1, L2, R191, R2, L4, R1, L5, L5, R5, L3, L5, L4, R4, R5, L4, R4, R4, R5, L2, L5, R3, L4, L4, L5, R2, R2, R2, R4, L3, R4, R5, L3, R5, L2, R3, L1, R2, R2, L3, L1, R5, L3, L5, R2, R4, R1, L1, L5, R3, R2, L3, L4, L5, L1, R3, L5, L2, R2, L3, L4, L1, R1, R4, R2, R2, R4, R2, R2, L3, L3, L4, R4, L4, L4, R1, L4, L4, R1, L2, R5, R2, R3, R3, L2, L5, R3, L3, R5, L2, R3, R2, L4, L3, L1, R2, L2, L3, L5, R3, L1, L3, L4, L3"

// First puzzle.
val directions = input
                    .split(", ")
                    .toList

def nextMove(x: Int, y: Int, dX: Int, dY: Int, direction: Char, step: Int): (Int, Int, Int, Int) =
  (dX, dY, direction) match {
    case (0,  1, 'R') => (x + step, y, 1, 0)
    case (0,  1, 'L') => (x - step, y, -1, 0)
    case (0, -1, 'R') => (x - step, y, -1, 0)
    case (0, -1, 'L') => (x + step, y, 1, 0)
    case (1,  0, 'R') => (x, y - step, 0, -1)
    case (1,  0, 'L') => (x, y + step, 0, 1)
    case (-1, 0, 'R') => (x, y + step, 0, 1)
    case (-1, 0, 'L') => (x, y - step, 0, -1)
  }

def calculate( moves: List[String]): Int =
  moves
    .map( v => (v.charAt(0), v.substring(1).toInt))
    .foldLeft[(Int,Int,Int,Int)]((0,0,0,1))( (a, d) => {
      val (x, y, dX, dY) = a
      val (direction, step) = d
      nextMove(x,y, dX, dY, direction, step)
    }) match {
    case (x, y, _, _) => Math.abs(x) + Math.abs(y)
  }

val solution = calculate(directions)  // 291

// Second puzzle.

def nextLine(x: Int, y: Int, dX: Int, dY: Int, direction: Char, step: Int): Seq[(Int, Int, Int, Int)] =
  (1 to step)
      .map(nextMove(x,y, dX, dY, direction, _))

def move(
          moves: List[String],
          passPoints: Vector[(Int,Int)] = Vector((0,0)),
          p: (Int, Int, Int, Int) = (0,0,0,1)): (Int, Int, Int, Int) = {
  moves match {
    case Nil => p
    case h :: t => {
      val direction = h.charAt(0)
      val step = h.substring(1).toInt
      val (x, y, dX, dY) = p
      val nextPath = nextLine(x,y, dX, dY, direction, step)
      val nextPs = nextPath.map {
        case (x, y, _, _) => (x, y)
      }
      val revisited = nextPs.filter(passPoints.contains(_))
      if (revisited.isEmpty) {
        move(t, passPoints ++ nextPs, nextPath.last)
      } else {
        val (foundX, foundY) = revisited.head
        (foundX, foundY, 0,0)
      }
    }
  }
}


val solution2 = move(directions) match {              // 159
  case (x, y, _, _) => Math.abs(x) + Math.abs(y)
}
