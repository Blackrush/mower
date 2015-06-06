package achauvin.mower

object Cardinal extends Enumeration {
  type Cardinal = Value

  val North, East, South, West = Value

  @scala.annotation.tailrec
  def of(id: Int): Cardinal =
    if (id < 0) of(4 + id)
    else if (id >= 4) of(id - 4)
    else Cardinal(id)

  def forSymbol(str: String): Option[Cardinal] =
    str match {
      case "N" => Some(North)
      case "E" => Some(East)
      case "S" => Some(South)
      case "W" => Some(West)
      case _ => None
    }
}

