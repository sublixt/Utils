package sublixt.opengl.impl

import org.lwjgl.opengl.GL30
import sublixt.opengl._

object VAOImpl
		extends GLGen[VAO] with GLDelete[VAO]
		with GLBind[VAO] {

	def gen() = new VAO(GL30.glGenVertexArrays())
	def delete(obj: VAO) = GL30.glDeleteVertexArrays(obj.id)
	def unbind() = GL30.glBindVertexArray(0)

	def bind(obj: VAO): GLBind[VAO] = {
		GL30.glBindVertexArray(obj.id)
		this
	}
}
