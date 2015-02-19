import scala.concurrent.ExecutionContext.Implicits.global
import scala.async.Async._
import scala.concurrent.Await
import scala.concurrent.duration._

object PlayingWithAsync extends App {
	val a = async {
		42
	}

	val b = Await.result(a, 10 seconds)
	println(b)
}
