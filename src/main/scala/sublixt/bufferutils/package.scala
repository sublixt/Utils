package sublixt

import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.ByteOrder
package object bufferutils {
	def create[A](capacity: Int)(implicit gen: BufferGen[A]) = gen.create(capacity)
	implicit def toByteBufferUtils(buffer: ByteBuffer) = new ByteBufferUtils(buffer)
	implicit def toFloatBuferUtils(buffer: FloatBuffer) = new FloatBufferUtils(buffer)

	implicit val byteBufferImplicit = ByteBufferGen
	implicit val floatBufferImplicit = FloatBufferGen
	implicit val intBufferImplicit = IntBufferGen
}
