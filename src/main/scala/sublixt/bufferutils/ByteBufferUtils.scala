package sublixt.bufferutils

import java.awt.image.BufferedImage
import java.nio.ByteBuffer

class ByteBufferUtils(val buffer: ByteBuffer) extends AnyVal {
	def put(image: BufferedImage) = {
		val width = image.getWidth
		val height = image.getHeight

		val pixels = new Array[Int](width * height)
		image.getRGB(0, 0, width, height, pixels, 0, width)

		for {
			y <- 0 until height
			x <- 0 until width
		} {
			val pixel = pixels(y * width + x)
			buffer.put(((pixel >>> 16) & 0xFF).toByte)
			buffer.put(((pixel >>> 8) & 0xFF).toByte)
			buffer.put((pixel & 0xFF).toByte)
			buffer.put((pixel >>> 24).toByte)
		}

		buffer
	}
}
