package achauvin.mower

import Cardinal._

case class Mower(pos: Position, card: Cardinal)(implicit val bound: Position) {
  def execute(cmd: Command): Mower =
    cmd match {
      case Forward =>
        card match {
          case North => Mower(Position.of(pos.x, pos.y + 1), card)
          case East => Mower(Position.of(pos.x + 1, pos.y), card)
          case South => Mower(Position.of(pos.x, pos.y - 1), card)
          case West => Mower(Position.of(pos.x - 1, pos.y), card)
        }
      case Left => Mower(pos, Cardinal.of(card.id - 1))
      case Right => Mower(pos, Cardinal.of(card.id + 1))
    }

  def executeAll(cmds: Seq[Command]): Mower =
    cmds.foldLeft(this) { _.execute(_) }

  def |>(cmds: Seq[Command]) = executeAll(cmds)
}

object Mower {
  private def toInt(str: String): Option[Int] =
    try {
      Some(Integer.parseInt(str, 10))
    } catch {
      case e: NumberFormatException => None
    }

  def apply(str: String)(implicit bound: Position): Option[Mower] = {
    val args = str.split(" ", 3)

    for {
      x <- toInt(args(0))
      y <- toInt(args(1))
      card <- Cardinal.forSymbol(args(2))
    } yield Mower(Position(x, y), card)
  }
}

