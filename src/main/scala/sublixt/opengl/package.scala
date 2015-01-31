import scala.collection.mutable.ArrayStack
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._
import org.lwjgl.BufferUtils

package sublixt {
	package object opengl extends Implicits {
		implicit final class GLBindOps[A](val obj: A)(implicit val bind: GLBind[A]) {
			def foreach(f: Unit => Unit) {
				bind.bind(obj)
				bind.bound = obj
				f(())
				bind.bound = null.asInstanceOf[A]
				bind.unbind()
			}
		}

		private[opengl] val errorStack = new ArrayStack[Throwable]()
		private[opengl] val floatBuffer = BufferUtils.createFloatBuffer(16)

		def glGen[A](implicit gen: GLGen[A]) = gen.gen()
		def glDelete[A](obj: A)(implicit del: GLDelete[A]) = del.delete(obj)
		def glBind[A](obj: A)(f: => Unit)(implicit bind: GLBind[A]) {
			bind.bind(obj)
			bind.bound = obj
			f
			bind.bound = null.asInstanceOf[A]
			bind.unbind()
		}
		def glBindOnly[A](obj: A)(f: => Unit)(implicit bind: GLBind[A]) {
			bind.bind(obj)
			bind.bound = obj
			f
		}
		def glUnbind[A](implicit bind: GLBind[A]) {
			bind.unbind()
			bind.bound = null.asInstanceOf[A]
		}
		def glAttach[A, B](to: A, obj: B)(implicit att: GLAttach[A, B]) = att.attach(to, obj)
		def glInfoLog[A](obj: A)(implicit il: GLInfoLog[A]) = il.infoLog(obj)
		//TODO rethink this function
		def glGetParameter[A](obj: A, pname: Int)(implicit gp: GLGetParameter[A]) = gp.getPara(obj, pname)
		def glCompile[A](obj: A)(implicit com: GLCompile[A]) = com.compile(obj)
		def glBufferData[B](buffer: B, usage: Int)(implicit bd: GLBufferData[B]) = bd.data(buffer, usage)
		def glBufferSubData[B](buffer: B, offset: Long)(implicit bd: GLBufferData[B]) = bd.subData(buffer, offset)
		def glUniform[A](location: Int, uniform: A)(implicit u: GLUniform[A]) = u.uniform(location, uniform)
		def glGetErrorStack = {
			val error = glGetError()
			if (error != GL_NO_ERROR) {
				errorStack.push(
					new Exception(
						error match {
							case GL_INVALID_ENUM =>
								"Invalid Enum: An unacceptable value is specified for an enumerated argument."
							case GL_INVALID_OPERATION =>
								"Invalid Operation: A numeric argument is out of range."
							case GL_OUT_OF_MEMORY =>
								"Out of Memory: There is not enough memory left to execute the command."
							case GL_STACK_UNDERFLOW =>
								"Stack Underflow: An attempt has been made to perform an operation that would cause an internal stack to underflow."
							case GL_STACK_OVERFLOW =>
								"Stack Overflow: An attempt has been made to perform an operation that would cause an internal stack to overflow."
							case GL_INVALID_FRAMEBUFFER_OPERATION =>
								"Invalid Framebuffer Operation: The framebuffer object is not complete."
							case t => t.toString
						}
					)
				)
			}
			if (errorStack.isEmpty) None
			else Some(errorStack.toList)
		}
	}

	package opengl {
		trait GLBind[A] {
			protected[opengl] var bound: A = null.asInstanceOf[A]
			def bind(obj: A)
			def unbind()
		}

		trait GLGen[A] {
			def gen(): A
		}

		trait GLDelete[A] {
			def delete(obj: A)
		}

		trait GLAttach[A, B] {
			def attach(to: A, obj: B)
		}

		trait GLInfoLog[A] {
			def infoLog(obj: A): String
		}

		trait GLGetParameter[A] {
			def getPara(obj: A, pname: Int): Option[Int]
		}

		trait GLCompile[A] {
			def compile(obj: A)
		}

		trait GLBufferData[B] {
			def data(buffer: B, usage: Int)
			def subData(buffer: B, offset: Long)
		}

		trait GLUniform[A] {
			def uniform(location: Int, uniform: A)
		}
	}
}
