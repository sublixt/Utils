package sublixt.opengl.impl

import sublixt.opengl._
import org.lwjgl.opengl.GL20

object ShaderProgramImpl
		extends GLGen[ShaderProgram] with GLDelete[ShaderProgram]
		with GLBind[ShaderProgram] with GLInfoLog[ShaderProgram]
		with GLCompile[ShaderProgram] with GLGet[ShaderProgram, TFStatus, Boolean] {

	def gen() = new ShaderProgram(GL20.glCreateProgram())
	def delete(obj: ShaderProgram) = GL20.glDeleteProgram(obj.id)
	def unbind() = GL20.glUseProgram(0)
	def infoLog(obj: ShaderProgram) = GL20.glGetProgramInfoLog(obj.id)
	def get(program: ShaderProgram, pname: TFStatus): Boolean = GL20.glGetProgrami(program.id, pname.id) == GL_TRUE

	def bind(obj: ShaderProgram): GLBind[ShaderProgram] = {
		GL20.glUseProgram(obj.id)
		this
	}

	def compile(obj: ShaderProgram) {
		GL20.glLinkProgram(obj.id)
		GL20.glValidateProgram(obj.id)
	}
}

object ProgramVertAttach extends GLAttach[ShaderProgram, VertexShader] {
	def attach(program: ShaderProgram, shader: VertexShader) =
		GL20.glAttachShader(program.id, shader.id)
}

object ProgramFragAttach extends GLAttach[ShaderProgram, FragmentShader] {
	def attach(program: ShaderProgram, shader: FragmentShader) =
		GL20.glAttachShader(program.id, shader.id)
}
