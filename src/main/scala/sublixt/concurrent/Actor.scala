package sublixt.concurrent

import scala.concurrent.{ Future, Promise, ExecutionContext }
import scala.util.Try

object Actor {
	def apply[A, B](react: A => B)(implicit ec: ExecutionContext): Actor[A, B] =
		new ActorImpl(react)(ec)
}

sealed trait Actor[A, B] {
	protected val mailbox = new AtomicArrayQueue[(A, Promise[B])](10)
	val ec: ExecutionContext

	final def ![C <: A](message: C) = {
		val empty = mailbox.isEmpty

		val promise = Promise[B]
		mailbox += (message, Promise[B])

		if (empty)
			ec.execute(new Runnable {
				def run() {
					schedule()
				}
			})

		promise.future
	}

	protected def schedule()
}

private[concurrent] final class ActorImpl[A, B] private[concurrent] (
		val react: A => B)(implicit val ec: ExecutionContext) extends Actor[A, B] {
	protected def schedule() {
		while (!mailbox.isEmpty) {
			val (message, response) = mailbox()
			response.complete(Try(react(message)))
		}
	}
}

object Router {
	def apply[A, B](actors: Actor[A, B]*)(implicit ec: ExecutionContext): Router[A, B] = {
		require(actors.length > 0)
		new Router(actors.toArray)
	}

	def apply[A, B](num: Int)(react: A => B)(implicit ec: ExecutionContext): Router[A, B] = {
		require(num > 0)
		new Router(Array.fill(num)(Actor(react)))
	}
}

final class Router[A, B] private[concurrent] (
		private val actors: Array[Actor[A, B]])(implicit val ec: ExecutionContext) extends Actor[A, B] {
	private var num = 0

	protected def schedule() {
		while (!mailbox.isEmpty) {
			val (message, response) = mailbox()
			response.completeWith(actors(num) ! message)
			num = nextInt(num)
		}
	}

	@inline private def nextInt(x: Int) = {
		val x1 = x + 1
		if (x1 == actors.length) 0
		else x1
	}
}
