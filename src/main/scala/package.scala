import com.ning.http.client.Response
import dispatch.FunctionHandler

package object hipshot {
  object Bool {
    def apply(bool: Boolean) = if (bool) "1" else "0"
  }
}
