package hipshot

import dispatch.Req
import org.json4s.JsonDSL._
import org.json4s.JValue
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact

// https://www.hipchat.com/docs/apiv2

trait Methods { self: Requests =>

  private object json {
    private[this] val Typ = "application/json"
    private[this] val Encoding = "UTF-8"
    def content(r: Req) = r.setContentType(Typ, Encoding)
    def str(jv: JValue) = compact(render(jv))
  }

  // view_group scope
  object emoticons {
    private[this] def emoticonBase = apiBase / "emoticon"

    def apply[T]
      (start: Int = 0, max: Int = 100, kind: String = "all")
      (handler: Client.Handler[T]) =
      request(emoticonBase <<? Map(
        "start-index" -> start.toString,
        "max-results" -> max.toString,
        "type" -> kind))(handler)

    def get(emo: String) =
      complete(emoticonBase / emo)
  }

  object rooms {
    private[this] def roomBase = apiBase / "room"

    // manage_rooms scope
    /** https://www.hipchat.com/docs/apiv2/method/delete_room */
    def delete(id: String) =
      complete(roomBase.DELETE /  id)

    case class CreateRoom(
      _name: String,
      _ownerId: Option[String] = None,
      _privacy: Privacy = Privacy.Public,
      _guests: Boolean = false) extends Client.Completion {
      def name(name: String) = copy(_name = name)
      def owner(id: String) = copy(_ownerId = Some(id))
      def guests(enable: Boolean) = copy(_guests = enable)
      def privacy(priv: Privacy) = copy(_privacy = priv)
      def apply[T](handler: Client.Handler[T]) =
        request(json.content(roomBase.POST)
             << json.str(
               ("name" -> _name) ~
               ("owner_user_id" -> _ownerId) ~
               ("privacy" -> _privacy.value) ~
               ("guest_access" -> _guests)))(handler)
    }

    // manage_rooms scope
    /** https://www.hipchat.com/docs/apiv2/method/create_room */
    def create(name: String) = CreateRoom(name)

    /** https://www.hipchat.com/docs/apiv2/method/update_room */
    // admin_room scope
    def update(
      roomId: String,
      name: String,
      privacy: Privacy,
      archived: Boolean,
      guests: Boolean,
      topic: String,
      owner: String) =
      complete(json.content(roomBase.PUT / roomId)
               << json.str(
                 ("name" -> name) ~
                 ("privacy" -> privacy.value) ~
                 ("is_archived" -> archived) ~
                 ("is_guest_accessible" -> guests) ~
                 ("topic" -> topic) ~
                 ("owner" -> ("id" -> owner))))

    // view_group scope
    def apply(start: Int = 0, max: Int = 100) =
      complete(roomBase <<? Map("start-index" -> start.toString,
                                "max-results" -> max.toString))

    case class Notify(
      _room: String,
      _message: (String, String),
      _color: String = "yellow",
      _notify: Boolean = false) extends Client.Completion {
      def room(r: String) = copy(_room = r)
      def text(msg: String) = copy(_message = (msg, "text"))
      def html(msg: String) = copy(_message = (msg, "html"))
      def apply[T](handler: Client.Handler[T]) =
        request(json.content(roomBase.POST) / _room / "notification"
             << json.str(
               ("color" -> _color) ~
               ("message" -> _message._1) ~
               ("message_format" -> _message._2) ~
               ("notify" -> _notify)))(handler)
    }

    // send_notification scope
    /** https://www.hipchat.com/docs/apiv2/method/send_room_notification */
    def notify(room: String, message: String) =
      Notify(room, (message, "text"))

    /** https://www.hipchat.com/docs/apiv2/method/get_room */
    def get(room: String) =
      complete(roomBase / room)

    // admin_room scope
    case class Webhooks(room: String) {
      private[this] def webhookBase = apiBase / "room" / room / "webhook"

      /** https://www.hipchat.com/docs/apiv2/method/get_all_webhooks */
      def apply[T]
        (start: Int = 0, max: Int = 100)
        (handler: Client.Handler[T]) =
        request(webhookBase <<? Map(
          "start-index" -> start.toString,
          "max-results" -> max.toString))(handler)

      /** https://www.hipchat.com/docs/apiv2/method/get_webhook */
      def get(hook: String) =
        complete(webhookBase / hook)

      /** https://www.hipchat.com/docs/apiv2/method/create_webhook */
      def create(
        url: String,
        pattern: String,
        event: String,
        name: String) =
        complete(json.content(webhookBase.POST) << json.str(
          ("url" -> url) ~
          ("pattern" -> pattern) ~
          ("event" -> event) ~
          ("name" -> name)))

      /** https://www.hipchat.com/docs/apiv2/method/delete_webhook */
      def delete(hook: String) =
        complete(webhookBase.DELETE / hook)
    }

    def webhooks(room: String) = Webhooks(room)

    // admin_room scope
    case class Members(room: String) {
      private[this] def memberBase = apiBase / "room" / room / "member"

      /** https://www.hipchat.com/docs/apiv2/method/get_all_members */
      def apply[T]
        (start: Int = 0, max: Int = 100)
        (handler: Client.Handler[T]) = {
        println(memberBase.toRequest.getUrl)
        request(memberBase.GET <<? Map(
          "start-index" -> start.toString,
          "max-results" -> max.toString))(handler)
        }

      /** https://www.hipchat.com/docs/apiv2/method/add_member */
      def add(user: String) =
        complete(memberBase.PUT / user)

      /** https://www.hipchat.com/docs/apiv2/method/remove_member */
      def remove(user: String) =
        complete(memberBase.DELETE / user)

      /** https://www.hipchat.com/docs/apiv2/method/invite_user */
      def invite(user: String) =
        complete(apiBase.POST / "room" / room / "invite" / user)
    }

    def members(room: String) = Members(room)

    def topic(room: String, name: String) =
      complete(json.content(apiBase.PUT) / room / "topic"
               << json.str("topic" -> name))
  }

  object users {
    private[this] def userBase = apiBase / "user"

    case class User(user: String) {
      private[this] def thisUser = userBase / user

      /** https://www.hipchat.com/docs/apiv2/method/view_user */
      def apply[T]
        (handler: Client.Handler[T]) =
        request(thisUser)(handler)

      def message(msg: String) =
        complete(json.content(thisUser.POST) << json.str(
          "message" -> msg))

      /** https://www.hipchat.com/docs/apiv2/method/delete_user */
      def delete =
        complete(thisUser.DELETE)

      /** https://www.hipchat.com/docs/apiv2/method/update_user */
      //def update()

      /** https://www.hipchat.com/docs/apiv2/method/create_user */
      // def create()
    }

    def get(user: String) = User(user)

    case class Editor(
      userId: Option[String]    = None,
      _email: Option[String]    = None,
      _name: Option[String]     = None,
      _mention: Option[String]  = None,
      _title: Option[String]    = None,
      _admin: Option[Boolean]   = None,
      _password: Option[String] = None,
      _timezone: Option[String] = None) {

      def email(addr: String) = copy(_email = Some(addr))
      def name(n: String) = copy(_name = Some(n))
      def mention(m: String) = copy(_mention = Some(m))
      def title(t: String) = copy(_title = Some(t))
      def admin(is: Boolean) = copy(_admin = Some(is))
      def password(pass: String) = copy(_password = Some(pass))
      /** https://www.hipchat.com/docs/api/timezones */
      def timezone(tz: String) = copy(_timezone = Some(tz))

      def apply[T]
        (handler: Client.Handler[T]) =
        request(json.content(endpoint) << json.str(
              ("user_id"     -> userId) ~
              ("email"       -> _email) ~
              ("name"        -> _name) ~
              ("mention_name" -> _mention) ~
              ("title"       -> _title) ~
              ("is_group_admin" -> _admin) ~
              ("password"    -> _password) ~
              ("timezone"    -> _timezone)))(handler)

      private def endpoint =
        userId.map(userBase.PUT / _).getOrElse(userBase.POST)
    }

    /** https://www.hipchat.com/docs/apiv2/method/create_user */
    def create(
      email: String,
      name: String) = Editor().email(email).name(name)

    def update(userId: String) = Editor(Some(userId))

    case class Search(
      _start: Int       = 0,
      _max: Int         = 100,
      _guests: Boolean  = false,
      _deleted: Boolean = false) extends Client.Completion {
      def startIndex(start: Int) = copy(_start = start)
      def max(lim: Int) = copy(_max = lim)
      def guests(incl: Boolean) = copy(_guests = incl)
      def deleted(incl: Boolean) = copy(_deleted = incl)
      def apply[T](handler: Client.Handler[T]) =
        request(userBase <<? Map(
          "start-index" -> _start.toString,
          "max-results" -> _max.toString,
          "include-guests" -> _guests.toString,
          "include-deleted" -> _deleted.toString))(handler)
    }

    /** https://www.hipchat.com/docs/apiv2/method/get_all_users */
    def apply = Search()
  }
}
