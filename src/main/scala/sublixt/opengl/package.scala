package sublixt

import sublixt.opengl.impl._
import sublixt.bufferutils._
import sublixt.math._
import org.lwjgl.opengl.GL20
import org.lwjgl.opengl.GL11
import java.nio.ByteBuffer
import java.nio.FloatBuffer

package object opengl extends ConstantValues {
	@inline def glGen[A](implicit gen: GLGen[A]) = gen.gen
	@inline def glDelete[A](obj: A)(implicit delete: GLDelete[A]) = delete.delete(obj)
	@inline def glIsBound[A](implicit bind: GLBind[A]) = ???
	@inline def glBind[A](obj: A)(implicit bind: GLBind[A]) = bind.bind(obj)
	@inline def glUnbind[A](implicit bind: GLBind[A]) = bind.unbind()
	@inline def glBufferData[A](buffer: A, usage: Int)(implicit data: GLBufferData[A]) = data.bufferData(buffer, usage)
	@inline def glBufferSubData[A](buffer: A, offset: Int)(implicit data: GLBufferData[A]) = data.subData(buffer, offset)
	@inline def glInfoLog[A](obj: A)(implicit info: GLInfoLog[A]) = info.infoLog(obj)
	@inline def glCompile[A](obj: A)(implicit compile: GLCompile[A]) = compile.compile(obj)
	@inline def glAttach[A, B](a: A, b: B)(implicit attach: GLAttach[A, B]) = attach.attach(a, b)
	@inline def glGet[A, @specialized(Int, Boolean) B](a: A)(implicit get: GLGet1[A, B]) = get.get(a)
	@inline def glGet[A, @specialized(Int) B, @specialized(Int, Boolean) C](a: A, b: B)(implicit get: GLGet2[A, B, C]) = get.get(a, b)
	@inline def glParameter[A, @specialized(Int) B, @specialized(Int) C](a: A, b: B, c: C)(implicit para: GLParameter[A, B, C]) = para.parameter(a, b, c)

	@inline def glDrawElements(mode: Int, count: Int, offset: Int) = GL11.glDrawElements(mode, count, GL11.GL_UNSIGNED_INT, offset * 4)
	@inline def glDrawArrays(mode: Int, count: Int, offset: Int) = GL11.glDrawArrays(mode, offset, count)

	@inline def glTexImage2D(target: TexTarget, level: Int, internalFormat: Int, width: Int, height: Int, format: Int, data: ByteBuffer) = GL11.glTexImage2D(target.id, level, internalFormat, width, height, 0, format, GL11.GL_UNSIGNED_BYTE, data)
	@inline def glTexImage2D(target: TexTarget, level: Int, internalFormat: Int, width: Int, height: Int, format: Int) = GL11.glTexImage2D(target.id, level, internalFormat, width, height, 0, format, GL11.GL_UNSIGNED_BYTE, null.asInstanceOf[ByteBuffer])

	@inline def glEnableVertexAttribArray(index: Int) = GL20.glEnableVertexAttribArray(index)
	@inline def glDisableVertexAttribArray(index: Int) = GL20.glDisableVertexAttribArray(index)
	@inline def glVertexAttribPointer(index: Int, size: Int, stride: Int, offset: Int) = GL20.glVertexAttribPointer(index, size, GL11.GL_FLOAT, false, stride * 4, offset * 4)

	@inline def glClear(mask: Int) = GL11.glClear(mask)
	@inline def glCullFace(mode: Int) = GL11.glCullFace(mode)
	@inline def glFrontFace(dir: Int) = GL11.glFrontFace(dir)

	private val buffer = create[FloatBuffer](16)

	@inline def glUniform(location: Int, value: Float) {
		buffer.put(value)
		buffer.flip()
		GL20.glUniform1(location, buffer)
		buffer.clear()
	}

	@inline def glUniform(location: Int, vec: Vec2) {
		buffer.put(vec)
		buffer.flip()
		GL20.glUniform2(location, buffer)
		buffer.clear()
	}

	@inline def glUniform(location: Int, vec: Vec3) {
		buffer.put(vec)
		buffer.flip()
		GL20.glUniform3(location, buffer)
		buffer.clear()
	}

	@inline def glUniform(location: Int, vec: Vec4) {
		buffer.put(vec)
		buffer.flip()
		GL20.glUniform4(location, buffer)
		buffer.clear()
	}

	@inline def glUniform(location: Int, mat: Mat2) {
		buffer.put(mat)
		buffer.flip()
		GL20.glUniformMatrix2(location, false, buffer)
		buffer.clear()
	}

	@inline def glUniform(location: Int, mat: Mat3) {
		buffer.put(mat)
		buffer.flip()
		GL20.glUniformMatrix3(location, false, buffer)
		buffer.clear()
	}

	@inline def glUniform(location: Int, mat: Mat4) {
		buffer.put(mat)
		buffer.flip()
		GL20.glUniformMatrix4(location, false, buffer)
		buffer.clear()
	}

	@inline def glGetUniformLocation(program: ShaderProgram, name: String) = GL20.glGetUniformLocation(program.id, name)
	@inline def glGetAttribLocation(program: ShaderProgram, name: String) = GL20.glGetAttribLocation(program.id, name)

	implicit val vboImplicit = VBOImpl
	implicit val iboImplicit = IBOImpl
	implicit val vaoImplicit = VAOImpl
	implicit val shaderProgramImplicit = ShaderProgramImpl
	implicit val programVertImplicit = ProgramVertAttach
	implicit val programFragImplicit = ProgramFragAttach
	implicit val vertexShaderImplicit = VertexShaderImpl
	implicit val fragmentShaderImplicit = FragmentShaderImpl
	implicit val tex2DImplicit = Tex2DImpl
	implicit val texTargetImplicit = TexParameterImpl
	implicit val fboImplicit = FBOImpl
}
