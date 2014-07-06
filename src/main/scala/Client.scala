package hipshot

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ FunctionHandler, Http, Req, :/ }
import scala.concurrent.{ ExecutionContext, Future }

object Client {
  type Handler[T] = AsyncHandler[T]
  abstract class Completion[T: Rep] {
    def apply(): Future[T] =
      apply(implicitly[Rep[T]].map)
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new FunctionHandler(f))
    def apply[T]
      (handler: Client.Handler[T]): Future[T]
  }
}

trait DefaultHosts {
  def apiBase = :/("api.hipchat.com").secure / "v2" <:< Map("Content-Type" -> "application/json")
}

abstract class Requests(
  token: String, http: Http)
  (implicit ec: ExecutionContext)
  extends DefaultHosts
  with Methods {

  def request[T]
    (req: Req)
    (handler: Client.Handler[T]): Future[T] =
    http(req <<? Map("auth_token" -> token) > handler)

  def complete[T: Rep](req: Req): Client.Completion[T] =
    new Client.Completion[T] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}

case class Client(
  token: String, private val http: Http = Http)
  (implicit ec: ExecutionContext)
  extends Requests(token, http)
