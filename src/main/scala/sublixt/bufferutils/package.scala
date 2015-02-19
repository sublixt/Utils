package sublixt

import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.ByteOrder
package object bufferutils {
	implicit def toByteBufferUtils(buffer: ByteBuffer) = new ByteBufferUtils(buffer)
	implicit def toFloatBuferUtils(buffer: FloatBuffer) = new FloatBufferUtils(buffer)
}
