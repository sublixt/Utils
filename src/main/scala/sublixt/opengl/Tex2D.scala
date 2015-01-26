/*package sublixt.opengl

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._
import java.nio.ByteBuffer

object Tex2DFunctions {
	def image(width: Int, height: Int, format: Int, pixels: ByteBuffer) {
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, format, GL_UNSIGNED_BYTE, pixels)
	}
	
	def image(width: Int, height: Int) {
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, null.asInstanceOf[ByteBuffer])
	}
	
	def subImage(x: Int, y: Int, width: Int, height: Int, format: Int, pixels: ByteBuffer) {
		glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, width, height, format, GL_UNSIGNED_BYTE, pixels)
	}

	def parameter(pname: Int, param: Int) {
		glTexParameteri(GL_TEXTURE_2D, pname, param)
	}
	
	def gemMipmap() =
		glGenerateMipmap(GL_TEXTURE_2D)
}

class Tex2D private[opengl] (val id: Int) {
	type F = Tex2DFunctions.type
	def fn = Tex2DFunctions
}*/