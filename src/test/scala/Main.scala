import org.lwjgl._
import glfw.GLFW._
import opengl._
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

		glBufferData(buffer, GL_STATIC_DRAW)
	}

	glEnableVertexAttribArray(0)
	glEnableVertexAttribArray(1)
	glVertexAttribPointer(0, 4, 6, 0)
	glVertexAttribPointer(1, 2, 6, 4)

	glBind(ibo)
	locally {
		val array =
			Array(0, 1, 2, 2, 3, 0)

		val buffer = BufferUtils.createIntBuffer(array.length)
		buffer.put(array)
		buffer.flip()

		glBufferData(buffer, GL_STATIC_DRAW)
	}
	glUnbind[VAO]

	def readSource(file: String) =
		Source.fromInputStream(getClass.getResourceAsStream(file)).getLines.mkString("\n")

	val frag = glGen[FragmentShader]
	val vert = glGen[VertexShader]
	val program = glGen[ShaderProgram]

	val fragSource = readSource("frag.frag")
	val vertSource = readSource("vert.vert")
	glAttach(frag, fragSource)
	glAttach(vert, vertSource)

	println(vertSource)
	println(fragSource)

	glCompile(frag)
	glCompile(vert)

	println("Compile Status: " + glGet(vert, GL_COMPILE_STATUS))
	println("Compile Status: " + glGet(frag, GL_COMPILE_STATUS))
	println(glInfoLog(vert))
	println(glInfoLog(frag))

	glAttach(program, vert)
	glAttach(program, frag)

	glCompile(program)

	println("Link Status: " + glGet(program, GL_LINK_STATUS))
	println("Validate Status: " + glGet(program, GL_VALIDATE_STATUS))
	println(glInfoLog(program))

	val tex = glGen[Tex2D]
	locally {
		val image = ImageIO.read(getClass.getResourceAsStream("crate.jpg"))
		val buffer = BufferUtils.createByteBuffer(image.getWidth * image.getHeight * 4)
		buffer.put(image)
		buffer.flip()

		glBind(tex)
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, image.getWidth, image.getHeight, GL_RGBA, buffer)

		glParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
		glParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
		glUnbind[Tex2D]
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

		rot += delta * 10 * degToRad * 0.005f
		buffer.put(mat.roty(rot).rotx(rot).rotz(rot))
		buffer.flip

		for {
			_ <- glBind(program)
			_ <- glBind(tex)
			_ <- glBind(vao)
		} {
			glUniformMatrix4(loc, buffer)
			glUniformMatrix4(loc2, buffer2)

			glDrawElements(GL_TRIANGLES, 6, 0)
		}

		glfwSwapBuffers(id)
		glfwPollEvents()
	}

	glfwDestroyWindow(id)
	glfwTerminate()
}
