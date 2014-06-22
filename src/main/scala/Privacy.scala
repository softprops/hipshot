package hipshot

sealed trait Privacy {
  def value: String
}

object Privacy {
  abstract class Value(val value: String) extends Privacy
  case object Public extends Value("public")
  case object Private extends Value("private")
  def fromStr(str: String) =
    str match {
      case "public" => Public
      case "private" => Private
    }
}
