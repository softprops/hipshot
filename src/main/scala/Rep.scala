package hipshot

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

case class Results[Of](items: Seq[Of], startIndex: Int, maxResults: Int, links: Map[String, String])

case class Emoticon(id: Int, shortcut: String, url: String, links: Map[String, String])

case class EmoticonDetails(id: Int, shortcut: String, width: Int, height: Int, audio: Option[String])

case class User(id: Int, name: String, mentionName: String, links: Map[String, String])

case class Room(id: Int, name: String, links: Map[String, String])

sealed trait Rep[T] {
  def map: Response => T
}

sealed trait Parse[T] {
  def map: List[JValue] => List[T]
}

object Rep {
  implicit object Identity extends Rep[Response] {
    def map = identity(_)
  }

  implicit object Nada extends Rep[Unit] {
    def map = _ => ()
  }

  /** convenience for parsing result set-like responses */
  trait ResultRep[T] extends Rep[Results[T]] {
    def parse: List[JValue] => List[T]
    def map = { r => (for {
        JObject(results) <- as.json4s.Json(r)
        ("startIndex", JInt(start)) <- results
        ("maxResults", JInt(max))   <- results
        ("items", JArray(items))    <- results
        ("links", JObject(links))   <- results
      } yield Results(
        parse(items), start.toInt, max.toInt, (for {
          (key, JString(link)) <- links
        } yield (key, link)).toMap)).head
    }
  }

  implicit object EmoticonDetailsRep extends Rep[EmoticonDetails] {
    def map = { r => (for {
      JObject(dets) <- as.json4s.Json(r)
      ("id", JInt(id)) <- dets
      ("shortcut", JString(shortcut)) <- dets
      ("width", JInt(width)) <- dets
      ("height", JInt(height)) <- dets
    } yield EmoticonDetails(
      id.toInt, shortcut, width.toInt, height.toInt,
      (for {
        ("audio_path", JString(audio)) <- dets
      } yield audio).headOption
    )).head }
  }

  implicit object RoomResults extends ResultRep[Room] {
    def parse = (for {
      JObject(room) <- _
      ("id", JInt(id))          <- room
      ("name", JString(name))   <- room
      ("links", JObject(links)) <- room
    } yield Room(
      id.toInt,
      name,
     (for {
        (key, JString(link)) <- links
      } yield (key, link)).toMap))
  }

  implicit object EmoticonResults extends ResultRep[Emoticon] {
    def parse = (for {
      JObject(room)                   <- _
      ("id", JInt(id))                <- room
      ("shortcut", JString(shortcut)) <- room
      ("url", JString(url))           <- room
      ("links", JObject(links))       <- room
    } yield Emoticon(
      id.toInt,
      shortcut,
      url,
     (for {
        (key, JString(link)) <- links
      } yield (key, link)).toMap))
  }

  implicit object UserResults extends ResultRep[User] {
    def parse = (for {
      JObject(member) <- _
      ("mention_name", JString(mention)) <-  member
      ("id", JInt(id))          <- member
      ("name", JString(name))   <- member
      ("links", JObject(links)) <- member
    } yield User(
      id.toInt,
      name,
      mention, (for {
        (key, JString(link)) <- links
      } yield (key, link)).toMap))
  }
}
