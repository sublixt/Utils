import sublixt.opengl._
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

object Main extends App {
	NativeExtractor.extractToTempFromZip(new File(System.getProperty("user.home") + File.separator + "Desktop" + File.separator + "native.zip")) match {
		case Some(list) =>
			System.setProperty("org.lwjgl.librarypath", NativeExtractor.tempDir)
		case None =>
			System.exit(0)
	}

	glfwInit()

	glfwDefaultWindowHints()
	glfwWindowHint(GLFW_VISIBLE, GL_TRUE)
	glfwWindowHint(GLFW_RESIZABLE, GL_FALSE)
	val id = glfwCreateWindow(800, 600, "Test", 0, 0)

	glfwMakeContextCurrent(id)
	val context = GLContext.createFromCurrent()
	glfwSwapInterval(1)

	while (glfwWindowShouldClose(id) != GL_TRUE) {

		glfwSwapBuffers(id)
		glfwPollEvents()
	}

	glfwDestroyWindow(id)
	glfwTerminate()
}
