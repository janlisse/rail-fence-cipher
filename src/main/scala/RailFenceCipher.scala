import scala.annotation.tailrec

object RailFenceCipher {

  def encode(plainText: String, numberOfRails: Int): String = {
    emptyRails(numberOfRails, plainText.length).fillZigZag(plainText).mkString
  }

  def decode(cipherText: String, numberOfRails: Int): String = {
    val placeholders = List.fill(cipherText.length)("?").mkString
    val railsWithPlaceholders = emptyRails(numberOfRails, cipherText.length).fillZigZag(placeholders)
    val filledRails = railsWithPlaceholders.fillRailByRail(cipherText)
    filledRails.readZigZag()
  }

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction

  case class Rail(values: Array[Option[Char]]) {
    def add(char: Char, index: Int): Rail = copy(values = values.updated(index, Some(char)))

    def fill(text: String): (Rail, String) = {
      val indexes = values.zipWithIndex.collect {
        case (Some(_), idx) => idx
      }
      val textForRail = text.take(indexes.length)
      val textWithIndex = textForRail.zip(indexes)
      val updatedRail = textWithIndex.foldLeft(this)((rail, textWithIndex) => {
        rail.copy(values = rail.values.updated(textWithIndex._2, Some(textWithIndex._1)))
      })
      (updatedRail, text.diff(textForRail))
    }
  }

  private def emptyRails(numberOfRails: Int, textLength: Int) =
    Array.fill(numberOfRails)(Rail(Array.fill(textLength)(None)))

  implicit class RailsOps(val self: Array[Rail]) {

    @tailrec
    final def fillZigZag(text: String, index: Int = 0, currentRailAndDirection: (Int, Direction) = (1, Down), rails: Array[Rail] = self): Array[Rail] = {
      val currentRail = currentRailAndDirection._1
      val char = text.charAt(index)
      val rail = rails(currentRail - 1)
      val updatedRails = rails.updated(currentRail - 1, rail.add(char, index))
      if (index == text.length - 1) {
        updatedRails
      } else {
        fillZigZag(text, index + 1, nextRailAndDirection(currentRailAndDirection), updatedRails)
      }
    }

    @tailrec
    final def fillRailByRail(text: String, activeRailNumber: Int = 1, markedRails: Array[Rail] = self): Array[Rail] = {
      val activeRail = markedRails(activeRailNumber - 1)
      val (updatedRail, newRemainingText) = activeRail.fill(text)
      val updatedRails = markedRails.updated(activeRailNumber - 1, updatedRail)
      if (activeRailNumber == self.length) {
        updatedRails
      } else {
        fillRailByRail(newRemainingText, activeRailNumber + 1, updatedRails)
      }
    }

    @tailrec
    final def readZigZag(acc: String = "", index: Int = 0, currentRailAndDirection: (Int, Direction) = (1, Down), rails: Array[Rail] = self): String = {
      val currentRail = currentRailAndDirection._1
      val rail = rails(currentRail - 1)
      val char = rail.values(index).get
      val newAcc = acc + char
      if (index >= rail.values.length - 1) {
        newAcc
      } else {
        readZigZag(newAcc, index + 1, nextRailAndDirection(currentRailAndDirection), rails)
      }
    }

    def mkString: String =
      self.foldLeft("")((cipher, rail) => cipher ++ rail.values.flatten.foldLeft("")(_ ++ _.toString))

    private def nextRailAndDirection(currentRailAndDirection: (Int, Direction)): (Int, Direction) = currentRailAndDirection match {
      case (rail, Down) if rail == self.length => (rail - 1, Up)
      case (rail, Up) if rail == 1 => (rail + 1, Down)
      case (rail, Down) => (rail + 1, Down)
      case (rail, Up) => (rail - 1, Up)
    }
  }
}




