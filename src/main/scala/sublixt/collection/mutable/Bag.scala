package sublixt.collection.mutable

import scala.reflect.ClassTag

class Bag[@specialized(Int, Float, Short) T] private (private var buffer: Array[T])(implicit private val tag: ClassTag[T]) {
	private var p = 0

	def this(initialLength: Int)(implicit tag: ClassTag[T]) {
		this(new Array[T](initialLength))
	}

	def this()(implicit tag: ClassTag[T]) {
		this(64)
	}

	def length = p
	def capacity = buffer.length
	def apply(index: Int) =
		if (index < 0) throw new IndexOutOfBoundsException("Negative Index")
		else if (index >= p) throw new IndexOutOfBoundsException("Index too big")
		else buffer(index)

	def update(i: Int, t: T) = {
		if (i < 0) throw new IndexOutOfBoundsException("Negative Index")
		else if (i > p) throw new IndexOutOfBoundsException("Index too big")
		else {
			val removedValue = buffer(i)
			buffer(i) = t
			removedValue
		}
	}

	def contains(elem: T): Boolean = {
		var i = 0
		while (i < p) {
			if (buffer(i) == elem) return true
			i += 1
		}

		false
	}

	def +=(t: T) {
		if (p == buffer.length) {
			grow()
			+=(t)
		} else {
			buffer(p) = t
			p += 1
		}
	}

	def insert(index: Int, t: T) {
		if (p == buffer.length) {
			grow()
			insert(index, t)
		} else if (index < 0) throw new IndexOutOfBoundsException("Negative Index")
		else if (index > p) throw new IndexOutOfBoundsException("Index too big")
		else {
			val temp = buffer(index)
			buffer(index) = t
			buffer(p) = temp
			p += 1
		}
	}

	def -=(t: T) {
		var i = 0
		while (i < p) {
			if (buffer(i) == t) {
				p -= 1
				buffer(i) = buffer(p)
				buffer(p) = null.asInstanceOf[T]
				return
			}
			i += 1
		}
	}

	def removeAll(t: T) {
		var i = 0
		while (i < p) {
			if (buffer(i) == t) {
				p -= 1
				buffer(i) = buffer(p)
				buffer(p) = null.asInstanceOf[T]
			}
			i += 1
		}
	}

	def remove(index: Int) = {
		if (index < 0) throw new IndexOutOfBoundsException("Negative Index")
		else if (index >= p) throw new IndexOutOfBoundsException("Index too big")
		else {
			p -= 1
			val removedValue = buffer(index)
			buffer(index) = buffer(p)
			buffer(p) = null.asInstanceOf[T]
			removedValue
		}
	}

	def removeLast() = {
		p -= 1
		val removedValue = buffer(p)
		buffer(p) = null.asInstanceOf[T]
		removedValue
	}

	private def grow() {
		val newLength = (buffer.length * 3) / 2 + 2
		val newBuffer = new Array[T](newLength)
		Array.copy(buffer, 0, newBuffer, 0, buffer.length)
	}
}