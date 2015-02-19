import org.lwjgl._
import glfw.GLFW._
import opengl._
import GL11._
import GL14._
import GL15._
import GL20._
import GL30._
import sublixt.natives.NativeExtractor
import java.io.File
import sublixt.math._
import sublixt.math.noise.Seed
import sublixt.math.noise.PerlinNoise
import scala.io.Source
import javax.imageio.ImageIO
import sublixt.bufferutils._
import sublixt.opengl._

object Main extends App {
	NativeExtractor.extractToTempFromZip(new File(System.getProperty("user.home") + File.separator + "Desktop" + File.separator + "native.zip")) match {
		case Some(list) =>
			System.setProperty("org.lwjgl.librarypath", NativeExtractor.tempDir)
		case None =>
			System.exit(0)
	}

	val seed = new Seed(System.nanoTime())
	val noise = new PerlinNoise(seed)

	glfwInit()

	glfwDefaultWindowHints()
	glfwWindowHint(GLFW_VISIBLE, GL_TRUE)
	glfwWindowHint(GLFW_RESIZABLE, GL_FALSE)
	val id = glfwCreateWindow(640, 480, "Test", 0, 0)

	glfwMakeContextCurrent(id)
	val context = GLContext.createFromCurrent()
	glfwSwapInterval(1)

	val vbo = glGen[VBO]
	val ibo = glGen[IBO]
	val vao = glGen[VAO]

	glBind(vao)
	glBind(vbo)
	locally {
		val array =
			Array(
				-1.0f, -1.0f, 0.0f, 1.0f, 0.0f, 0.0f,
				1.0f, -1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
				1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1.0f,
				-1.0f, 1.0f, 0.0f, 1.0f, 0.0f, 1.0f
			)

		val buffer = BufferUtils.createFloatBuffer(array.length)
		buffer.put(array)
		buffer.flip()

		glBufferData(GL_ARRAY_BUFFER, buffer, GL_STATIC_DRAW)
	}

	glEnableVertexAttribArray(0)
	glEnableVertexAttribArray(1)
	glVertexAttribPointer(0, 4, GL_FLOAT, false, 24, 0)
	glVertexAttribPointer(1, 2, GL_FLOAT, false, 24, 16)

	glBind(ibo)
	locally {
		val array =
			Array(0, 1, 2, 2, 3, 0)

		val buffer = BufferUtils.createIntBuffer(array.length)
		buffer.put(array)
		buffer.flip()

		glBufferData(GL_ELEMENT_ARRAY_BUFFER, buffer, GL_STATIC_DRAW)
	}
	glUnbind[VAO]

	def readSource(file: String) =
		Source.fromInputStream(getClass.getResourceAsStream(file)).getLines.mkString("\n")

	val frag = glCreateShader(GL_FRAGMENT_SHADER)
	val vert = glCreateShader(GL_VERTEX_SHADER)
	val program = glCreateProgram()

	val fragSource = readSource("frag.frag")
	val vertSource = readSource("vert.vert")
	glShaderSource(frag, fragSource)
	glShaderSource(vert, vertSource)

	println(vertSource)
	println(fragSource)

	glCompileShader(frag)
	glCompileShader(vert)

	println("Compile Status: " + (glGetShaderi(vert, GL_COMPILE_STATUS) == GL_TRUE))
	println("Compile Status: " + (glGetShaderi(frag, GL_COMPILE_STATUS) == GL_TRUE))
	println(glGetShaderInfoLog(vert))
	println(glGetShaderInfoLog(frag))

	glAttachShader(program, vert)
	glAttachShader(program, frag)

	glLinkProgram(program)
	glValidateProgram(program)

	println("Link Status: " + (glGetProgrami(program, GL_LINK_STATUS) == GL_TRUE))
	println("Validate Status: " + (glGetProgrami(program, GL_VALIDATE_STATUS) == GL_TRUE))
	println(glGetProgramInfoLog(program))

	val tex = glGenTextures()
	locally {
		val image = ImageIO.read(getClass.getResourceAsStream("crate.jpg"))
		val buffer = BufferUtils.createByteBuffer(image.getWidth * image.getHeight * 4)
		buffer.put(image)
		buffer.flip()

		glBindTexture(GL_TEXTURE_2D, tex)
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, image.getWidth, image.getHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
		glBindTexture(GL_TEXTURE_2D, 0)
	}

	val mat = Mat4.translate(0f, 0f, -3f)
	val buffer = BufferUtils.createFloatBuffer(16)

	val proj = Mat4.perspective(75f * degToRad, 4f / 3, -1f, -100f)
	val buffer2 = BufferUtils.createFloatBuffer(16)
	buffer2.put(proj)
	buffer2.flip

	val loc = glGetUniformLocation(program, "trans")
	val loc2 = glGetUniformLocation(program, "projection")

	def getTime = System.nanoTime / 1000000f

	var rot = 0f
	var lastTime = getTime

	while (glfwWindowShouldClose(id) != GL_TRUE) {
		glClear(GL_COLOR_BUFFER_BIT)
		val currentTime = getTime
		val delta = lastTime - currentTime
		lastTime = currentTime

		glUseProgram(program)

		rot += delta * 10 * degToRad * 0.005f
		buffer.put(mat.roty(rot).rotx(rot).rotz(rot))
		buffer.flip

		glUniformMatrix4(loc, false, buffer)
		glUniformMatrix4(loc2, false, buffer2)
		glBindTexture(GL_TEXTURE_2D, tex)
		glBind(vao)
		glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
		glUnbind[VAO]
		glBindTexture(GL_TEXTURE_2D, 0)
		glUseProgram(0)

		glfwSwapBuffers(id)
		glfwPollEvents()
	}

	glfwDestroyWindow(id)
	glfwTerminate()
}
