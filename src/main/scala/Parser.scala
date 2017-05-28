package parserforsmila

object Parser {

  def main(args: Array[String]): Unit = {
    args.foreach(str => println(str: Boolean))
  }

  sealed trait Brackets
  case object Square extends Brackets
  case object Round extends Brackets
  case object Undefined extends Brackets

  private implicit val run: String => Boolean = (phrase: String) => checkPhrase(phrase, Undefined)

  private def checkPhrase(phrase: String, brType: Brackets): Boolean = phrase match {
    case "A" if brType != Square => true
    case "B" if brType != Round => true
    case str if str.startsWith("[[") && str.endsWith(")]") && brType != Round =>
      val s = str.dropRight(1).drop(1)
      checkPhrase(head(s), Square) && checkPhrase(tail(s), Round)
    case str if str.startsWith("((") && str.endsWith("])") && brType != Square =>
      val s = str.dropRight(1).drop(1)
      checkPhrase(head(s), Round) && checkPhrase(tail(s), Square)
    case _ => false
  }

  private def head(p: String): String = {
    var (sqCount, rCount, phrase) = (0, 0, p)
    do {
      if (sqCount < 0 || rCount < 0) return "E"
      phrase.takeRight(1) match {
        case ")" => rCount += 1
        case "]" => sqCount += 1
        case "(" => rCount -= 1
        case "[" => sqCount -= 1
        case "A" | "B" =>
        case _ => return "E"
      }
      phrase = phrase.dropRight(1)
    } while (sqCount + rCount != 0)
    if ((phrase.startsWith("[") && phrase.endsWith("]")) || (phrase.startsWith("(") && phrase.endsWith(")")))
      phrase.dropRight(1).drop(1)
    else "E"
  }

  private def tail(p: String): String = {
    var (sqCount, rCount, phrase) = (0, 0, p)
    do {
      if (sqCount < 0 || rCount < 0) return "E"
      phrase.take(1) match {
        case ")" => rCount -= 1
        case "]" => sqCount -= 1
        case "(" => rCount += 1
        case "[" => sqCount += 1
        case "A" | "B" =>
        case _ => return "E"
      }
      phrase = phrase.drop(1)
    } while (sqCount + rCount != 0)
    if ((phrase.startsWith("[") && phrase.endsWith("]")) || (phrase.startsWith("(") && phrase.endsWith(")")))
      phrase.dropRight(1).drop(1)
    else "E"
  }
}
