package sublixt.bufferutils

import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.ByteOrder

trait BufferGen[A] {
	def create(capacity: Int): A
}

object ByteBufferGen extends BufferGen[ByteBuffer] {
	def create(capacity: Int) =
		ByteBuffer.allocateDirect(capacity).order(ByteOrder.nativeOrder())
}

object FloatBufferGen extends BufferGen[FloatBuffer] {
	def create(capacity: Int) =
		ByteBufferGen.create(capacity << 2).asFloatBuffer()
}

object IntBufferGen extends BufferGen[IntBuffer] {
	def create(capacity: Int) =
		ByteBufferGen.create(capacity << 2).asIntBuffer()
}
