package sublixt.opengl.impl

import sublixt.opengl._
import org.lwjgl.opengl.GL30
import org.lwjgl.opengl.GL11

object FBOImpl
		extends GLGen[FBO] with GLDelete[FBO]
		with GLBind[FBO] {

	def gen() = new FBO(GL30.glGenFramebuffers())
	def delete(obj: FBO) = GL30.glDeleteFramebuffers(obj.id)
	def unbind() = GL30.glBindFramebuffer(GL30.GL_FRAMEBUFFER, 0)

	def bind(obj: FBO) = {
		GL30.glBindFramebuffer(GL30.GL_FRAMEBUFFER, obj.id)
		this
	}
}

object FBOTex2DAttach extends GLAttach[FBOAttachment, Tex2D] {
	def attach(a: FBOAttachment, b: Tex2D) = GL30.glFramebufferTexture2D(GL30.GL_FRAMEBUFFER, a.id, GL11.GL_TEXTURE_2D, b.id, 0)
}
