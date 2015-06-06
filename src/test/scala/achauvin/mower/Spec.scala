package achauvin.mower

import org.scalatest.WordSpec
import org.scalatest.Assertions

import Cardinal._

class Spec extends WordSpec {
  import Assertions._

  implicit val bound = Position(5, 5)

  "A Command" when {
    "parsed" should {
      "return a command" in {
        assert(Command.parseOne('A') == Forward)
        assert(Command.parseOne('G') == Left)
        assert(Command.parseOne('D') == Right)

        intercept[NoSuchElementException] {
          Command.parseOne('Z')
        }
      }
      "return a sequence of command" in {
        val cmds = Command("DAG")
        assert(cmds == Seq(Right, Forward, Left))
      }
    }
  }

  "A Position" when {
    "queried" should {
      "be in bounds" in {
        intercept[IndexOutOfBoundsException] {
          Position.of(42, 6)
        }

        intercept[IndexOutOfBoundsException] {
          Position.of(-1, -1)
        }

        Position.of(5, 5)
        Position.of(0, 0)
      }
    }
  }

  "A Cardinal" when {
    "queried" should {
      "accept id" in {
        assert(Cardinal(0) == North)
        assert(Cardinal(1) == East)
        assert(Cardinal(2) == South)
        assert(Cardinal(3) == West)
      }

      "accept negative ids" in {
        assert(Cardinal.of(-1) == West)
        assert(Cardinal.of(-5) == West)
      }
    }
  }

  "A Mower" when {
    "initialized" should {
      "move" in {
        val m = Mower(Position(1, 2), North)
        val mm = m |> Command("GAGAGAGAA")

        assert(mm.pos == Position(1, 3))
        assert(mm.card == North)
      }

      "move2" in {
        val m = Mower(Position(3, 3), East)
        val mm = m |> Command("AADAADADDA")

        assert(mm.pos == Position(5, 1))
        assert(mm.card == East)
      }
    }

    "queried" should {
      "be parsable" in {
        val Some(m) = Mower("1 2 N")
        assert(m.pos == Position(1, 2))
        assert(m.card == North)
      }
    }
  }
}
