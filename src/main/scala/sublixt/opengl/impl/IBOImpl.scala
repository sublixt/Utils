package sublixt.opengl.impl

import java.nio.IntBuffer
import org.lwjgl.opengl.GL15
import sublixt.opengl._

object IBOImpl
		extends GLGen[IBO] with GLDelete[IBO]
		with GLBind[IBO] with GLBufferData[IntBuffer] {

	def gen() = new IBO(GL15.glGenBuffers())
	def delete(obj: IBO) = GL15.glDeleteBuffers(obj.id)
	def unbind() = GL15.glBindBuffer(GL15.GL_ELEMENT_ARRAY_BUFFER, 0)
	def bufferData(buffer: IntBuffer, usage: Int) =
		GL15.glBufferData(GL15.GL_ELEMENT_ARRAY_BUFFER, buffer, usage)
	def subData(buffer: IntBuffer, offset: Int) =
		GL15.glBufferSubData(GL15.GL_ELEMENT_ARRAY_BUFFER, offset * 4, buffer)

	def bind(obj: IBO): GLBind[IBO] = {
		GL15.glBindBuffer(GL15.GL_ELEMENT_ARRAY_BUFFER, obj.id)
		this
	}
}
