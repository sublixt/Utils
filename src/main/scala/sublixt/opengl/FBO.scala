package sublixt.opengl

import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11._

/*class FBO {
	val id = glGenFramebuffers()

	def attach(tex: Tex2D, attachment: Int) {
		glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + attachment, GL_TEXTURE_2D, tex.id, 0)
	}

	def bind() {
		glBindFramebuffer(GL_FRAMEBUFFER, id)
	}

	def unbind() {
		glBindFramebuffer(GL_FRAMEBUFFER, 0)
	}
}*/