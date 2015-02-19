package sublixt.opengl.impl

import sublixt.opengl._
import org.lwjgl.opengl.GL20

object VertexShaderImpl
		extends GLGen[VertexShader] with GLDelete[VertexShader]
		with GLInfoLog[VertexShader] with GLCompile[VertexShader]
		with GLAttach[VertexShader, String] with GLGet[VertexShader, TFStatus, Boolean] {

	def gen() = new VertexShader(GL20.glCreateShader(GL20.GL_VERTEX_SHADER))
	def delete(obj: VertexShader) = GL20.glDeleteShader(obj.id)
	def infoLog(obj: VertexShader) = GL20.glGetShaderInfoLog(obj.id)
	def compile(obj: VertexShader) = GL20.glCompileShader(obj.id)
	def attach(shader: VertexShader, source: String) = GL20.glShaderSource(shader.id, source)
	def get(shader: VertexShader, pname: TFStatus): Boolean = GL20.glGetShaderi(shader.id, pname.id) == GL_TRUE
}
