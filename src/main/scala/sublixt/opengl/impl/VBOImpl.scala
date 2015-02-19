package sublixt.opengl.impl

import java.nio.FloatBuffer
import org.lwjgl.opengl.GL15
import sublixt.opengl._

object VBOImpl
		extends GLGen[VBO] with GLDelete[VBO]
		with GLBind[VBO] with GLBufferData[FloatBuffer] {

	def gen() = new VBO(GL15.glGenBuffers())
	def delete(obj: VBO) = GL15.glDeleteBuffers(obj.id)
	def unbind() = GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, 0)
	def bufferData(buffer: FloatBuffer, usage: Int) =
		GL15.glBufferData(GL15.GL_ARRAY_BUFFER, buffer, usage)
	def subData(buffer: FloatBuffer, offset: Int) =
		GL15.glBufferSubData(GL15.GL_ARRAY_BUFFER, offset * 4, buffer)

	def bind(obj: VBO): GLBind[VBO] = {
		GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, obj.id)
		this
	}
}
