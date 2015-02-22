package sublixt.opengl

import org.lwjgl.opengl.GL15
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL20
import org.lwjgl.opengl.GL30

trait ConstantValues {
	final val GL_FRONT = GL11.GL_FRONT
	final val GL_BACK = GL11.GL_BACK
	final val GL_FRONT_AND_BACK = GL11.GL_FRONT_AND_BACK
	final val GL_CCW = GL11.GL_CCW
	final val GL_CW = GL11.GL_CW
	final val GL_FALSE = GL11.GL_FALSE
	final val GL_TRUE = GL11.GL_TRUE
	final val GL_COLOR_BUFFER_BIT = GL11.GL_COLOR_BUFFER_BIT
	final val GL_DEPTH_BUFFER_BIT = GL11.GL_DEPTH_BUFFER_BIT
	final val GL_STENCIL_BUFFER_BIT = GL11.GL_STENCIL_BUFFER_BIT
	final val GL_RGBA8 = GL11.GL_RGBA8
	final val GL_RGBA = GL11.GL_RGBA
	final val GL_TEXTURE_MAG_FILTER = GL11.GL_TEXTURE_MAG_FILTER
	final val GL_TEXTURE_MIN_FILTER = GL11.GL_TEXTURE_MIN_FILTER
	final val GL_LINEAR = GL11.GL_LINEAR
	final val GL_TEXTURE_2D = new TexTarget(GL11.GL_TEXTURE_2D)
	final val GL_TRIANGLES = GL11.GL_TRIANGLES
	final val GL_DYNAMIC_DRAW = GL15.GL_DYNAMIC_DRAW
	final val GL_STATIC_DRAW = GL15.GL_STATIC_DRAW
	final val GL_COMPILE_STATUS = new TFStatus(GL20.GL_COMPILE_STATUS)
	final val GL_LINK_STATUS = new TFStatus(GL20.GL_LINK_STATUS)
	final val GL_VALIDATE_STATUS = new TFStatus(GL20.GL_VALIDATE_STATUS)
	final val GL_COLOR_ATTACHMENT0 = new FBOAttachment(GL30.GL_COLOR_ATTACHMENT0)
}
