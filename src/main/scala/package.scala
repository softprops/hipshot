import com.ning.http.client.Response
import dispatch.FunctionHandler

package object hipshot {
  implicit def r2h[T](f: Response => T): Client.Handler[T] =
    new FunctionHandler(f)
}
