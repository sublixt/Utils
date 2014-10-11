package sublixt.collection.mutable

import scala.reflect.ClassTag

class ArrayQueue[@specialized(Int, Float, Short) T] private[mutable] (private var buffer: Array[T], private var h: Int, private var p: Int)(private implicit val tag: ClassTag[T]) {
	def this(initialLength: Int)(implicit tag: ClassTag[T]) {
		this(new Array[T](initialLength), 0, 0)
	}

	def this()(implicit tag: ClassTag[T]) {
		this(64)
	}

	@inline def isEmpty = h == p
	@inline def capacity = buffer.length
	@inline def length = scala.math.abs(h - p) // eventually change this to my math library

	def +=(t: T) {
		buffer(p) = t
		p = nextIndex(p)
		if (p == h) grow()
	}
	def add(t: T) = +=(t)

	def apply() =
		if (isEmpty) throw new NoSuchElementException("Mailbox is empty")
		else {
			val temp = buffer(h)
			buffer(h) = null.asInstanceOf[T]
			h = nextIndex(h)
			if (h == p) {
				h = 0
				p = 0
			}
			temp
		}
	def remove() = apply()

	def peek() =
		if (isEmpty) throw new NoSuchElementException("Mailbox is empty")
		else buffer(h)

	@inline private def nextIndex(index: Int) = {
		val next = index + 1
		if (next == buffer.length) 0
		else next
	}

	private def grow() {
		val newLength = (buffer.length * 3) / 2 + 2
		val newBuffer = new Array[T](newLength)
		var c = 0
		var i = p

		do {
			newBuffer(c) = buffer(i)
			i = nextIndex(i)
			c += 1
		} while (i != h)

		h = 0
		p = c
		buffer = newBuffer
	}

	override def toString() =
		buffer.mkString(", ")
}