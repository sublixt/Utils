package sublixt.concurrent

import java.util.concurrent.atomic.AtomicReferenceArray

//For the most part this is thread safe...
class AtomicArrayQueue[T] private[concurrent] (
		private var buffer: AtomicReferenceArray[T],
		@volatile private var h: Int,
		@volatile private var p: Int) {

	def this(initialLength: Int) {
		this(new AtomicReferenceArray[T](initialLength), 0, 0)
	}

	def this() {
		this(64)
	}

	@inline def isEmpty = h == p
	@inline def capacity = buffer.length
	@inline def length = sublixt.math.abs(h - p)

	def +=(t: T) {
		buffer.set(p, t)
		p = nextIndex(p)
		if (p == h) grow()
	}

	def apply() =
		if (isEmpty) throw new NoSuchElementException("Mailbox is empty")
		else {
			val temp = buffer.getAndSet(h, null.asInstanceOf[T])
			h = nextIndex(h)
			if (isEmpty) {
				h = 0
				p = 0
			}
			temp
		}

	def peek() =
		if (isEmpty) throw new NoSuchElementException("Mailbox is empty")
		else buffer.get(h)

	@inline private def nextIndex(index: Int) = {
		val next = index + 1
		if (next == buffer.length) 0
		else next
	}

	//this method is a little thread unsafe
	private def grow() {
		val newLength = (buffer.length * 3) / 2 + 2
		val newBuffer = new AtomicReferenceArray[T](newLength)
		var c = 0
		var i = p

		do {
			newBuffer.set(c, buffer.get(i))
			i = nextIndex(i)
			c += 1
		} while (i != h)

		h = 0
		p = c
		buffer = newBuffer
	}
}
