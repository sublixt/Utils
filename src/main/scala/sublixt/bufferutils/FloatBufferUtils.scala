package sublixt.bufferutils

import sublixt.math._
import java.nio.FloatBuffer

class FloatBufferUtils(val buffer: FloatBuffer) extends AnyVal {
	def put(quat: Quat): FloatBuffer = {
		buffer.put(quat.x)
		buffer.put(quat.y)
		buffer.put(quat.z)
		buffer.put(quat.w)
		buffer
	}

	def put(vec: Vec2): FloatBuffer = {
		buffer.put(vec.x)
		buffer.put(vec.y)
		buffer
	}

	def put(vec: Vec3): FloatBuffer = {
		buffer.put(vec.x)
		buffer.put(vec.y)
		buffer.put(vec.z)
		buffer
	}

	def put(vec: Vec4): FloatBuffer = {
		buffer.put(vec.x)
		buffer.put(vec.y)
		buffer.put(vec.z)
		buffer.put(vec.w)
		buffer
	}

	def put(mat: Mat2): FloatBuffer = {
		put(mat.c0)
		put(mat.c1)
	}

	def put(mat: Mat3): FloatBuffer = {
		put(mat.c0)
		put(mat.c1)
		put(mat.c2)
	}

	def put(mat: Mat4): FloatBuffer = {
		put(mat.c0)
		put(mat.c1)
		put(mat.c2)
		put(mat.c3)
	}
}
