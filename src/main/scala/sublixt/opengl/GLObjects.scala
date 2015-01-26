package sublixt.opengl

class FBO private[opengl] (val id: Int)
class DBO private[opengl] (val id: Int)
class IBO private[opengl] (val id: Int)
class VBO private[opengl] (val id: Int)
class Tex2D private[opengl] (val id: Int)
class VertexShader private[opengl] (val id: Int)
class FragmentShader private[opengl] (val id: Int)
class ShaderProgram private[opengl] (val id: Int)

class VAO private[opengl] (val id: Int) {
	private[opengl] var ibo: IBO = null
	private[opengl] var vbo: VBO = null
}

class GLObjectNotBoundException(message: String, cause: Throwable) extends Exception(message, cause) {
	def this(message: String) = this(message, null)
	def this(cause: Throwable) = this("", cause)
	def this() = this("", null)
}
