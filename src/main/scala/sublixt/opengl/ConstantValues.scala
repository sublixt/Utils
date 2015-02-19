package sublixt.opengl

import org.lwjgl.opengl.GL15
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL20

trait ConstantValues {
	final val GL_TRIANGLES = GL11.GL_TRIANGLES
	final val GL_DYNAMIC_DRAW = GL15.GL_DYNAMIC_DRAW
	final val GL_STATIC_DRAW = GL15.GL_STATIC_DRAW
	final val GL_COMPILE_STATUS = new TFStatus(GL20.GL_COMPILE_STATUS)
	final val GL_LINK_STATUS = new TFStatus(GL20.GL_LINK_STATUS)
	final val GL_VALIDATE_STATUS = new TFStatus(GL20.GL_VALIDATE_STATUS)
}
