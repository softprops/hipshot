package hipshot

sealed trait Privacy {
  def value: String =
    getClass.getSimpleName.toLowerCase.replace("""$""", "")
}

object Privacy {
  case object Public extends Privacy
  case object Private extends Privacy
  def fromStr(str: String) =
    str match {
      case "public" => Public
      case "private" => Private
    }
}
