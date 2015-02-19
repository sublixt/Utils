package sublixt.opengl.impl

import sublixt.opengl._
import org.lwjgl.opengl.GL20

object FragmentShaderImpl
		extends GLGen[FragmentShader] with GLDelete[FragmentShader]
		with GLInfoLog[FragmentShader] with GLCompile[FragmentShader]
		with GLAttach[FragmentShader, String] {

	def gen() = new FragmentShader(GL20.glCreateShader(GL20.GL_FRAGMENT_SHADER))
	def delete(obj: FragmentShader) = GL20.glDeleteShader(obj.id)
	def infoLog(obj: FragmentShader) = GL20.glGetShaderInfoLog(obj.id)
	def compile(obj: FragmentShader) = GL20.glCompileShader(obj.id)
	def attach(shader: FragmentShader, source: String) = GL20.glShaderSource(shader.id, source)
}
