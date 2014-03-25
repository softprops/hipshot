package hipshot

import scala.concurrent.ExecutionContext
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render
import org.json4s.JsonDSL._

// https://www.hipchat.com/docs/apiv2

trait Methods { self: Requests =>

  object rooms {
    private[this] def roomBase = apiBase / "room"
    private[this] def roomsBase = apiBase / "rooms"
    
    def delete(id: String) =
      complete(roomsBase.POST / "delete" << Map("room_id" -> id))

    // manage_rooms scope
    def create(
      name: String,
      ownerId: Option[String] = None,
      public: Boolean = true,
      guests: Boolean = false) =
      complete(roomBase.POST
             << compact(render(
               ("name" -> name) ~
               ("owner_user_id" -> ownerId) ~
               ("privacy" -> Privacy(public)) ~
               ("guest_access" -> guests))))

    // view_group scope
    def list(start: Int = 0, max: Int = 100) =
      complete(roomBase <<? Map("start-index" -> start.toString,
                                "max-results" -> max.toString))

    // send_notification scope
    def notify(
      room: String,
      message: String,
      color: String = "yellow",
      notify: Boolean = false,
      format: String = "text") =
      complete(roomBase.POST / room / "notification"
             << compact(render(
               ("color" -> color) ~
               ("message" -> message) ~
               ("notify" -> notify) ~
               ("message_format" -> format))))


    def topic(
      room: String,
      topic: String,
      from: String = "API") =
      complete(roomsBase.POST / "topic"
             << Map(
               "room_id" -> room,
               "topic" -> topic,
               "from" -> from))

    def show(room: String) =
      complete(roomsBase / "show" <<? Map("room_id" -> room))
  }

  object users {
    private[this] def usersBase = apiBase / "users"
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
        (handler: Client.Handler[T])
        (implicit ec: ExecutionContext) =
        request(endpoint << Map.empty[String, String] ++
              userId.map("user_id" -> _) ++
              _email.map("email" -> _) ++
              _name.map("name" -> _) ++
              _mention.map("mention_name" -> _) ++
              _title.map("title" -> _) ++
              _admin.map("is_group_admin" -> Bool(_)) ++
              _password.map("password" -> _) ++
              _timezone.map("timezone" -> _))(handler)

      private def endpoint =
        usersBase.POST / userId.map(
          Function.const("updated")).getOrElse("create")
    }

    def create(
      email: String,
      name: String) = Editor().email(email).name(name)

    def update(userId: String) = Editor(Some(userId))

    def delete(userId: String) =
      complete(usersBase.POST / "delete" << Map("user_id" -> userId))

    def undelete(userId: String) =
      complete(usersBase.POST / "undelete" << Map("user_id" -> userId))

    def users(withDeleted: Boolean = false) =
      complete(usersBase / "list" <<? Map(
        "include_deleted" -> Bool(withDeleted)))

    def show(userId: String) =
      complete(usersBase / "show" <<? Map("user_id" -> userId))
  }
}
