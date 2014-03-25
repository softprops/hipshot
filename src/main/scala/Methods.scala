package hipshot

import scala.concurrent.ExecutionContext

trait Methods { self: Requests =>

  object rooms {
    private[this] def roomsBase = apiBase / "rooms"

    def delete(id: String) =
      complete(roomsBase.POST / "delete" << Map("room_id" -> id))

    def create(
      name: String,
      ownerId: String,
      public: Boolean = true,
      topic: Option[String] = None,
      guests: Boolean = false) =
      complete(roomsBase.POST / "create"
               << Map(
                 "name"          -> name,
                 "owner_user_id" -> ownerId,
                 "privacy"       -> Some("public").filter(
                   Function.const(public)).getOrElse("private"),
                 "guest_access"  -> Some("1").filter(
                   Function.const(guests)).getOrElse("0")) ++
                 topic.map("topic" -> _))

    def list = complete(roomsBase / "list")

    def message(
      room: String,
      from: String,
      message: String,
      format: String  = "text",
      notify: Boolean = false,
      color: String   = "yellow") =
      complete(roomsBase.POST / "message"
               << Map(
                 "room_id" -> room,
                 "from" -> from,
                 "message" -> message,
                 "format" -> format,
                 "notify" -> Some("1").filter(
                   Function.const(notify)).getOrElse("0"),
                 "color" -> color))

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
              _admin.filter(identity).map(_ => ("is_group_admin" -> "1")) ++
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
      complete(usersBase / "list" <<? Map.empty[String, String] ++
        Some("1").filter(Function.const(withDeleted))
          .map("included_deleted" -> _))

    def show(userId: String) =
      complete(usersBase / "show" <<? Map("user_id" -> userId))
  }
}
