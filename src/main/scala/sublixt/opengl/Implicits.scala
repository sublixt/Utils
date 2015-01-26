package sublixt.opengl

import org.lwjgl.opengl.GL15
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._
import java.nio.FloatBuffer
import java.nio.IntBuffer
import sublixt.math._

trait Implicits {
	implicit object VBOImplicits
			extends GLBind[VBO] with GLGen[VBO]
			with GLDelete[VBO] with GLGetParameter[VBO]
			with GLBufferData[FloatBuffer] {
		def bind(obj: VBO) {
			if (VAOImplicits.bound != null)
				VAOImplicits.bound.vbo = obj
			glBindBuffer(GL_ARRAY_BUFFER, obj.id)
		}
		def unbind() = {
			if (VAOImplicits.bound != null)
				VAOImplicits.bound.vbo = null
			glBindBuffer(GL_ARRAY_BUFFER, 0)
		}
		def gen() = new VBO(glGenBuffers())
		def delete(obj: VBO) = glDeleteBuffers(obj.id)
		def getPara(obj: VBO, pname: Int) = {
			val isBound = !(bound eq obj)
			if (isBound) bind(obj)
			val para = glGetBufferParameteri(GL_ARRAY_BUFFER, pname)
			if (isBound) unbind()
			Some(para)
		}
		def data(buffer: FloatBuffer, usage: Int) =
			if (bound != null || VAOImplicits.bound.vbo != null)
				GL15.glBufferData(GL_ARRAY_BUFFER, buffer, usage)
			else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glBufferData) No VBO bound on call."
					)
				)
		def subData(buffer: FloatBuffer, offset: Long) =
			if (bound != null || VAOImplicits.bound.vbo != null)
				GL15.glBufferSubData(GL_ARRAY_BUFFER, offset, buffer)
			else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glBufferSubData) No VBO bound on call."
					)
				)
	}

	implicit object IBOImplicits
			extends GLBind[IBO] with GLGen[IBO]
			with GLDelete[IBO] with GLGetParameter[IBO]
			with GLBufferData[IntBuffer] {
		def bind(obj: IBO) {
			if (VAOImplicits.bound != null)
				VAOImplicits.bound.ibo = obj
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, obj.id)
		}
		def unbind() {
			if (VAOImplicits.bound != null)
				VAOImplicits.bound.ibo = null
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
		}
		def gen() = new IBO(glGenBuffers())
		def delete(obj: IBO) = glDeleteBuffers(obj.id)
		def getPara(obj: IBO, pname: Int) = {
			if (obj eq bound) {
				val para = glGetBufferParameteri(GL_ELEMENT_ARRAY_BUFFER, pname)
				Some(para)
			} else {
				errorStack.push(
					new GLObjectNotBoundException("glGetParameter when no IBO was bound")
				)
				None
			}
		}
		def data(buffer: IntBuffer, usage: Int) =
			if (bound != null || VAOImplicits.bound.ibo != null)
				GL15.glBufferData(GL_ELEMENT_ARRAY_BUFFER, buffer, usage)
			else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glBufferData) No IBO bound on call."
					)
				)
		def subData(buffer: IntBuffer, offset: Long) =
			if (bound != null || VAOImplicits.bound.ibo != null)
				GL15.glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, offset, buffer)
			else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glBufferSubData) No IBO bound on call."
					)
				)
	}

	implicit object ShaderProgramImplicits
			extends GLBind[ShaderProgram] with GLGen[ShaderProgram]
			with GLDelete[ShaderProgram] with GLGetParameter[ShaderProgram]
			with GLInfoLog[ShaderProgram] with GLCompile[ShaderProgram] {
		def bind(obj: ShaderProgram) = glUseProgram(obj.id)
		def unbind() = glUseProgram(0)
		def gen() = new ShaderProgram(glCreateProgram())
		def delete(obj: ShaderProgram) = glDeleteProgram(obj.id)
		def getPara(obj: ShaderProgram, pname: Int) = Some(glGetProgrami(obj.id, pname))
		def infoLog(obj: ShaderProgram) = glGetProgramInfoLog(obj.id)
		def compile(obj: ShaderProgram) {
			glLinkProgram(obj.id)
			glValidateProgram(obj.id)
		}
	}

	implicit object AttachVertShaderToProgram extends GLAttach[ShaderProgram, VertexShader] {
		def attach(to: ShaderProgram, obj: VertexShader) = glAttachShader(to.id, obj.id)
	}

	implicit object AttachFragShaderToProgram extends GLAttach[ShaderProgram, FragmentShader] {
		def attach(to: ShaderProgram, obj: FragmentShader) = glAttachShader(to.id, obj.id)
	}

	implicit object VertexShaderImplicits
			extends GLGen[VertexShader] with GLDelete[VertexShader]
			with GLAttach[VertexShader, String] with GLGetParameter[VertexShader]
			with GLInfoLog[VertexShader] with GLCompile[VertexShader] {
		def gen() = new VertexShader(glCreateShader(GL_VERTEX_SHADER))
		def delete(obj: VertexShader) = glDeleteShader(obj.id)
		def attach(to: VertexShader, source: String) = glShaderSource(to.id, source)
		def getPara(obj: VertexShader, pname: Int) = Some(glGetShaderi(obj.id, pname))
		def infoLog(obj: VertexShader) = glGetShaderInfoLog(obj.id)
		def compile(obj: VertexShader) = glCompileShader(obj.id)
	}

	implicit object FragmentShaderImplicits
			extends GLGen[FragmentShader] with GLDelete[FragmentShader]
			with GLAttach[FragmentShader, String] with GLGetParameter[FragmentShader]
			with GLInfoLog[FragmentShader] with GLCompile[FragmentShader] {
		def gen() = new FragmentShader(glCreateShader(GL_FRAGMENT_SHADER))
		def delete(obj: FragmentShader) = glDeleteShader(obj.id)
		def attach(to: FragmentShader, source: String) = glShaderSource(to.id, source)
		def getPara(obj: FragmentShader, pname: Int) = Some(glGetShaderi(obj.id, pname))
		def infoLog(obj: FragmentShader) = glGetShaderInfoLog(obj.id)
		def compile(obj: FragmentShader) = glCompileShader(obj.id)
	}

	implicit object VAOImplicits
			extends GLGen[VAO] with GLDelete[VAO]
			with GLBind[VAO] {
		def gen() = new VAO(glGenVertexArrays())
		def delete(obj: VAO) = glDeleteVertexArrays(obj.id)
		def bind(obj: VAO) = glBindVertexArray(obj.id)
		def unbind() = glBindVertexArray(0)
	}

	implicit object Tex2DImplicits
			extends GLGen[Tex2D] with GLDelete[Tex2D]
			with GLBind[Tex2D] with GLGetParameter[Tex2D] {
		def gen() = new Tex2D(glGenTextures())
		def delete(obj: Tex2D) = glDeleteTextures(obj.id)
		def bind(obj: Tex2D) = glBindTexture(GL_TEXTURE_2D, obj.id)
		def unbind() = glBindTexture(GL_TEXTURE_2D, 0)
		def getPara(obj: Tex2D, pname: Int) = {
			bind(obj)
			val para = glGetTexParameteri(GL_TEXTURE_2D, pname)
			unbind()
			Some(para)
		}
	}

	implicit object FBOUniforms
			extends GLBind[FBO] with GLGen[FBO]
			with GLDelete[FBO] {
		def gen() = new FBO(glGenFramebuffers())
		def delete(obj: FBO) = glDeleteFramebuffers(obj.id)
		def bind(obj: FBO) = glBindFramebuffer(GL_FRAMEBUFFER, obj.id)
		def unbind() = glBindFramebuffer(GL_FRAMEBUFFER, 0)
	}

	implicit object AttchTex2DFBO extends GLAttach[FBO, Tex2D] {
		def attach(to: FBO, obj: Tex2D) {
			if (FBOUniforms.bound != null)
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, obj.id, 0)
			else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glAttach) No FBO bound on call."
					)
				)
		}
	}

	implicit object AttchDBOFBO extends GLAttach[FBO, DBO] {
		def attach(to: FBO, obj: DBO) {
			if (FBOUniforms.bound != null)
				glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, obj.id)
			else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glAttach) No FBO bound on call."
					)
				)
		}
	}

	implicit object FloatUniform extends GLUniform[Float] {
		def uniform(location: Int, uniform: Float) {
			if (ShaderProgramImplicits.bound != null) {
				floatBuffer.put(uniform)
				floatBuffer.flip()
				glUniform1(location, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}

	implicit object Vec2Uniform extends GLUniform[Vec2] {
		def uniform(location: Int, uniform: Vec2) {
			if (ShaderProgramImplicits.bound != null) {
				uniform.store(floatBuffer)
				floatBuffer.flip()
				glUniform2(location, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}

	implicit object Vec3Uniform extends GLUniform[Vec3] {
		def uniform(location: Int, uniform: Vec3) {
			if (ShaderProgramImplicits.bound != null) {
				uniform.store(floatBuffer)
				floatBuffer.flip()
				glUniform3(location, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}

	implicit object Vec4Uniform extends GLUniform[Vec4] {
		def uniform(location: Int, uniform: Vec4) {
			if (ShaderProgramImplicits.bound != null) {
				uniform.store(floatBuffer)
				floatBuffer.flip()
				glUniform4(location, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}

	implicit object Mat2Uniform extends GLUniform[Mat2] {
		def uniform(location: Int, uniform: Mat2) {
			if (ShaderProgramImplicits.bound != null) {
				uniform.store(floatBuffer)
				floatBuffer.flip()
				glUniformMatrix2(location, false, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}

	implicit object Mat3Uniform extends GLUniform[Mat3] {
		def uniform(location: Int, uniform: Mat3) {
			if (ShaderProgramImplicits.bound != null) {
				uniform.store(floatBuffer)
				floatBuffer.flip()
				glUniformMatrix3(location, false, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}

	implicit object Mat4Uniform extends GLUniform[Mat4] {
		def uniform(location: Int, uniform: Mat4) {
			if (ShaderProgramImplicits.bound != null) {
				uniform.store(floatBuffer)
				floatBuffer.flip()
				glUniformMatrix4(location, false, floatBuffer)
				floatBuffer.clear
			} else
				errorStack.push(
					new GLObjectNotBoundException(
						"(glUniform) No ShaderProgram bound on call."
					)
				)
		}
	}
}
