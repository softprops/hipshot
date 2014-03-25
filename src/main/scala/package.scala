import com.ning.http.client.Response
import dispatch.FunctionHandler

package object hipshot {
  object Bool {
    def apply(bool: Boolean) = if (bool) "1" else "0"
  }

  implicit def r2h[T](f: Response => T): Client.Handler[T] =
    new FunctionHandler(f)
}
