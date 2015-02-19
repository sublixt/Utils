package sublixt.opengl.impl

import sublixt.opengl._
import org.lwjgl.opengl.GL11

object TexParameterImpl extends GLParameter[TexTarget, Int, Int] {
	def parameter(target: TexTarget, pname: Int, param: Int) = GL11.glTexParameteri(target.id, pname, param)
}
