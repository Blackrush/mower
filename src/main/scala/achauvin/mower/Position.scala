package achauvin.mower

case class Position(x: Int, y: Int)

object Position {
  def of(x: Int, y: Int)(implicit bound: Position) =
    if (x < 0 || x > bound.x || y < 0 || x > bound.y)
      throw new IndexOutOfBoundsException(s"($x, $y) is out of bounds")
    else
      Position(x, y)
}


