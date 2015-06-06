package achauvin.mower

sealed trait Command

object Command {
  def parseOne(ch: Char): Command =
    ch match {
      case 'A' => Forward
      case 'G' => Left
      case 'D' => Right
      case _ => throw new NoSuchElementException
    }

  def parse(str: String): Seq[Command] =
    str map parseOne

  def apply(str: String) = parse(str)
}

case object Forward extends Command
case object Left extends Command
case object Right extends Command

