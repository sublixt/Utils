package sublixt.opengl.impl

import sublixt.opengl._
import org.lwjgl.opengl.GL11

object Tex2DImpl
		extends GLGen[Tex2D] with GLDelete[Tex2D]
		with GLBind[Tex2D] {

	def gen() = new Tex2D(GL11.glGenTextures())
	def delete(obj: Tex2D) = GL11.glDeleteTextures(obj.id)
	def unbind() = GL11.glBindTexture(GL11.GL_TEXTURE_2D, 0)

	def bind(obj: Tex2D) = {
		GL11.glBindTexture(GL11.GL_TEXTURE_2D, obj.id)
		this
	}
}
