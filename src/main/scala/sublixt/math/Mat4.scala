package sublixt.math

import java.nio.FloatBuffer

object Mat4 {
	val identity =
		Mat4(
			Vec4(1, 0, 0, 0),
			Vec4(0, 1, 0, 0),
			Vec4(0, 0, 1, 0),
			Vec4(0, 0, 0, 1))

	val zero = Mat4(Vec4(), Vec4(), Vec4(), Vec4())

	def apply(mat: Mat2): Mat4 = Mat4(Vec4(mat.c0, 0, 0), Vec4(mat.c1, 0, 0), Vec4(0, 0, 1, 0), Vec4(0, 0, 0, 1))
	def apply(mat: Mat3): Mat4 = Mat4(Vec4(mat.c0, 0), Vec4(mat.c1, 0), Vec4(mat.c2, 0), Vec4(0, 0, 0, 1))
	def apply(): Mat4 = identity
	def apply(euler: Vec3): Mat4 = rot(euler)
	def apply(quat: Quat): Mat4 = quat.toRotationMatrix

	def ortho(left: Int, right: Int, bottom: Int, top: Int, near: Int, far: Int): Mat4 = {
		val tmb = top - bottom
		val rml = right - left
		val fmn = far - near
		Mat4(
			Vec4(2f / rml, 0, 0, 0),
			Vec4(0, 2f / tmb, 0, 0),
			Vec4(0, 0, 2f / fmn, 0),
			Vec4(-((right + left) / (rml)), -((top + bottom) / (tmb)), -((far + near) / fmn), 1))
	}

	def frustum(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): Mat4 = {
		val nearX2 = near * 2
		val rml = right - left
		val tmb = top - bottom
		val fmn = far - near

		Mat4(
			Vec4(nearX2 / rml, 0, 0, 0),
			Vec4(0, nearX2 / tmb, 0, 0),
			Vec4((right + left) / rml, (top + bottom) / tmb, (-far - near) / fmn, -1),
			Vec4(0, 0, (-nearX2 * far) / fmn, 0))
	}

	def perspective(fov: Float, aspectRatio: Float, near: Float, far: Float): Mat4 = {
		val ymax = near * tan(fov / 2)
		val xmax = ymax * aspectRatio
		frustum(-xmax, xmax, -ymax, ymax, near, far)
	}

	def translate(x: Float, y: Float, z: Float): Mat4 =
		Mat4(Vec4(1, 0, 0, x), Vec4(0, 1, 0, y), Vec4(0, 0, 1, z), Vec4(0, 0, 0, 1))
	def translate(vec: Vec3): Mat4 =
		translate(vec.x, vec.y, vec.z)

	def rotx(angle: Float): Mat4 = {
		val c = cos(angle)
		val s = sin(angle)

		Mat4(
			Vec4(1, 0, 0, 0),
			Vec4(0, c, s, 0),
			Vec4(0, -s, c, 0),
			Vec4(0, 0, 0, 1))
	}

	def roty(angle: Float): Mat4 = {
		val c = cos(angle)
		val s = sin(angle)

		Mat4(
			Vec4(c, 0, -s, 0),
			Vec4(0, 1, 0, 0),
			Vec4(s, 0, c, 0),
			Vec4(0, 0, 0, 1))
	}

	def rotz(angle: Float): Mat4 = {
		val c = cos(angle)
		val s = sin(angle)

		Mat4(
			Vec4(c, s, 0, 0),
			Vec4(-s, c, 0, 0),
			Vec4(0, 0, 1, 0),
			Vec4(0, 0, 0, 1))
	}

	def rot(bank: Float, heading: Float, attitude: Float): Mat4 = {
		val sa = sin(attitude)
		val ca = cos(attitude)
		val sb = sin(bank)
		val cb = cos(bank)
		val sh = sin(heading)
		val ch = cos(heading)

		val shsa = sh * sa
		val chsa = ch * sa

		Mat4(
			Vec4(ch * ca, sa, -sh * ca, 0),
			Vec4(-chsa * cb + sh * sb, ca * cb, shsa * cb + ch * sb, 0),
			Vec4(chsa * sb + sh * cb, -ca * sb, -shsa * sb + ch * cb, 0),
			Vec4(0, 0, 0, 1))
	}

	def rot(vec: Vec3): Mat4 =
		rot(vec.x, vec.y, vec.z)

	def scale(x: Float, y: Float, z: Float): Mat4 =
		Mat4(Vec4(x, 0, 0, 0), Vec4(0, y, 0, 0), Vec4(0, 0, z, 0), Vec4(0, 0, 0, 1))
	def scale(vec: Vec3): Mat4 =
		scale(vec.x, vec.y, vec.z)
}

case class Mat4(val c0: Vec4, val c1: Vec4, val c2: Vec4, val c3: Vec4) {
	def unary_- = Mat4(-c0, -c1, -c2, -c3)
	def +(scalar: Float) = Mat4(c0 + scalar, c1 + scalar, c2 + scalar, c3 + scalar)
	def -(scalar: Float) = Mat4(c0 - scalar, c1 - scalar, c2 - scalar, c3 - scalar)
	def *(scalar: Float) = Mat4(c0 * scalar, c1 * scalar, c2 * scalar, c3 * scalar)
	def /(scalar: Float) = Mat4(c0 / scalar, c1 / scalar, c2 / scalar, c3 / scalar)

	def +(other: Mat4) = Mat4(c0 + other.c0, c1 + other.c1, c2 + other.c2, c3 + other.c3)
	def -(other: Mat4) = Mat4(c0 - other.c0, c1 - other.c1, c2 - other.c2, c3 - other.c3)

	def *(other: Mat4) =
		Mat4(
			Vec4(
				c0.x * other.c0.x + c1.x * other.c0.y + c2.x * other.c0.z + c3.x * other.c0.w,
				c0.y * other.c0.x + c1.y * other.c0.y + c2.y * other.c0.z + c3.y * other.c0.w,
				c0.z * other.c0.x + c1.z * other.c0.y + c2.z * other.c0.z + c3.z * other.c0.w,
				c0.w * other.c0.x + c1.w * other.c0.y + c2.w * other.c0.z + c3.w * other.c0.w),
			Vec4(
				c0.x * other.c1.x + c1.x * other.c1.y + c2.x * other.c1.z + c3.x * other.c1.w,
				c0.y * other.c1.x + c1.y * other.c1.y + c2.y * other.c1.z + c3.y * other.c1.w,
				c0.z * other.c1.x + c1.z * other.c1.y + c2.z * other.c1.z + c3.z * other.c1.w,
				c0.w * other.c1.x + c1.w * other.c1.y + c2.w * other.c1.z + c3.w * other.c1.w),
			Vec4(
				c0.x * other.c2.x + c1.x * other.c2.y + c2.x * other.c2.z + c3.x * other.c2.w,
				c0.y * other.c2.x + c1.y * other.c2.y + c2.y * other.c2.z + c3.y * other.c2.w,
				c0.z * other.c2.x + c1.z * other.c2.y + c2.z * other.c2.z + c3.z * other.c2.w,
				c0.w * other.c2.x + c1.w * other.c2.y + c2.w * other.c2.z + c3.w * other.c2.w),
			Vec4(
				c0.x * other.c3.x + c1.x * other.c3.y + c2.x * other.c3.z + c3.x * other.c3.w,
				c0.y * other.c3.x + c1.y * other.c3.y + c2.y * other.c3.z + c3.y * other.c3.w,
				c0.z * other.c3.x + c1.z * other.c3.y + c2.z * other.c3.z + c3.z * other.c3.w,
				c0.w * other.c3.x + c1.w * other.c3.y + c2.w * other.c3.z + c3.w * other.c3.w))

	def *(vec: Vec4) =
		Vec4(
			c0.x * vec.x + c1.x * vec.y + c2.x * vec.z + c3.x * vec.w,
			c0.y * vec.x + c1.y * vec.y + c2.y * vec.z + c3.y * vec.w,
			c0.z * vec.x + c1.z * vec.y + c2.z * vec.z + c3.z * vec.w,
			c0.w * vec.x + c1.w * vec.y + c2.w * vec.z + c3.w * vec.w)

	def apply(i: Int) =
		i match {
			case 0 => c0
			case 1 => c1
			case 2 => c2
			case 3 => c3
			case _ => throw new IndexOutOfBoundsException("[Mat4](apply(int)): " + i.toString + " not in range [0, 3]")
		}

	lazy val transpose =
		Mat4(
			Vec4(c0.x, c1.x, c2.x, c3.x),
			Vec4(c0.y, c1.y, c2.y, c3.y),
			Vec4(c0.z, c1.z, c2.z, c3.z),
			Vec4(c0.w, c1.w, c3.w, c3.w))

	lazy val determinant =
		(c0.x * (c1.y * c2.z * c3.w + c2.y * c3.z * c1.w + c3.y * c1.z * c2.w -
			c1.w * c2.z * c3.y - c2.w * c3.z * c1.y - c3.w * c1.z * c2.y)) -
			(c1.x * (c0.y * c2.z * c3.w + c2.y * c3.z * c0.w + c3.y * c0.z * c2.w -
				c0.w * c2.z * c3.y - c2.w * c3.z * c0.y - c3.w * c0.z * c2.y)) +
				(c2.x * (c0.y * c1.z * c3.w + c1.y * c3.z * c0.w + c3.y * c0.z * c1.w -
					c0.w * c1.z * c3.y - c1.w * c3.z * c0.y - c3.w * c0.z * c1.y)) -
					(c3.x * (c0.y * c1.z * c2.w + c1.y * c2.z * c0.w + c2.y * c0.z * c1.w -
						c0.w * c1.z * c2.y - c1.w * c2.z * c0.y - c2.w * c0.z * c1.y))

	lazy val inverse = {
		val d = determinant

		Mat4(
			Vec4((c1.z * c2.w * c3.y - c1.w * c2.z * c3.y + c1.w * c2.y * c3.z - c1.y * c2.w * c3.z - c1.z * c2.y * c3.w + c1.y * c2.z * c3.w) / d,
				(c0.w * c2.z * c3.y - c0.z * c2.w * c3.y - c0.w * c2.y * c3.z + c0.y * c2.w * c3.z + c0.z * c2.y * c3.w - c0.y * c2.z * c3.w) / d,
				(c0.z * c1.w * c3.y - c0.w * c1.z * c3.y + c0.w * c1.y * c3.z - c0.y * c1.w * c3.z - c0.z * c1.y * c3.w + c0.y * c1.z * c3.w) / d,
				(c0.w * c1.z * c2.y - c0.z * c1.w * c2.y - c0.w * c1.y * c2.z + c0.y * c1.w * c2.z + c0.z * c1.y * c2.w - c0.y * c1.z * c2.w) / d),
			Vec4((c1.w * c2.z * c3.x - c1.z * c2.w * c3.x - c1.w * c2.x * c3.z + c1.x * c2.w * c3.z + c1.z * c2.x * c3.w - c1.x * c2.z * c3.w) / d,
				(c0.z * c2.w * c3.x - c0.w * c2.z * c3.x + c0.w * c2.x * c3.z - c0.x * c2.w * c3.z - c0.z * c2.x * c3.w + c0.x * c2.z * c3.w) / d,
				(c0.w * c1.z * c3.x - c0.z * c1.w * c3.x - c0.w * c1.x * c3.z + c0.x * c1.w * c3.z + c0.z * c1.x * c3.w - c0.x * c1.z * c3.w) / d,
				(c0.z * c1.w * c2.x - c0.w * c1.z * c2.x + c0.w * c1.x * c2.z - c0.x * c1.w * c2.z - c0.z * c1.x * c2.w + c0.x * c1.z * c2.w) / d),
			Vec4((c1.y * c2.w * c3.x - c1.w * c2.y * c3.x + c1.w * c2.x * c3.y - c1.x * c2.w * c3.y - c1.y * c2.x * c3.w + c1.x * c2.y * c3.w) / d,
				(c0.w * c2.y * c3.x - c0.y * c2.w * c3.x - c0.w * c2.x * c3.y + c0.x * c2.w * c3.y + c0.y * c2.x * c3.w - c0.x * c2.y * c3.w) / d,
				(c0.y * c1.w * c3.x - c0.w * c1.y * c3.x + c0.w * c1.x * c3.y - c0.x * c1.w * c3.y - c0.y * c1.x * c3.w + c0.x * c1.y * c3.w) / d,
				(c0.w * c1.y * c2.x - c0.y * c1.w * c2.x - c0.w * c1.x * c2.y + c0.x * c1.w * c2.y + c0.y * c1.x * c2.w - c0.x * c1.y * c2.w) / d),
			Vec4((c1.z * c2.y * c3.x - c1.y * c2.z * c3.x - c1.z * c2.x * c3.y + c1.x * c2.z * c3.y + c1.y * c2.x * c3.z - c1.x * c2.y * c3.z) / d,
				(c0.y * c2.z * c3.x - c0.z * c2.y * c3.x + c0.z * c2.x * c3.y - c0.x * c2.z * c3.y - c0.y * c2.x * c3.z + c0.x * c2.y * c3.z) / d,
				(c0.z * c1.y * c3.x - c0.y * c1.z * c3.x - c0.z * c1.x * c3.y + c0.x * c1.z * c3.y + c0.y * c1.x * c3.z - c0.x * c1.y * c3.z) / d,
				(c0.y * c1.z * c2.x - c0.z * c1.y * c2.x + c0.z * c1.x * c2.y - c0.x * c1.z * c2.y - c0.y * c1.x * c2.z + c0.x * c1.y * c2.z) / d))
	}

	def rotx(angle: Float) = {
		val c = cos(angle)
		val s = sin(angle)
		Mat4(
			c0,
			Vec4(c1.x * c + c2.x * s, c1.y * c + c2.y * s, c1.z * c + c2.z * s, c1.w * c + c2.w * s),
			Vec4(c2.x * c - c1.x * s, c2.y * c - c1.y * s, c2.z * c - c1.z * s, c2.w * c - c1.w * s),
			c3)
	}

	def roty(angle: Float) = {
		val c = cos(angle)
		val s = sin(angle)

		Mat4(
			Vec4(c0.x * c - c2.x * s, c0.y * c - c2.y * s, c0.z * c - c2.z * s, c0.w * c - c2.w * s),
			c1,
			Vec4(c0.x * s + c2.x * c, c0.y * s + c2.y * c, c0.z * s + c2.z * c, c0.w * s + c2.w * c),
			c3)
	}

	def rotz(angle: Float) = {
		val c = cos(angle)
		val s = sin(angle)

		Mat4(
			Vec4(c0.x * c + c1.x * sin(angle), c0.y * c + c1.y * sin(angle), c0.z * c + c1.z * sin(angle), c0.w * c + c1.w * sin(angle)),
			Vec4(c1.x * c - c0.x * sin(angle), c1.y * c - c0.y * sin(angle), c1.z * c - c0.z * sin(angle), c1.w * c - c0.w * sin(angle)),
			c2,
			c3)
	}

	def translate(x: Float, y: Float, z: Float): Mat4 =
		Mat4(
			Vec4(c0.x + c3.x * x, c0.y + c3.y * x, c0.z + c3.z * x, c0.w + c3.w * x),
			Vec4(c1.x + c3.x * y, c1.y + c3.y * y, c1.z + c3.z * y, c1.w + c3.w * y),
			Vec4(c2.x + c3.x * z, c2.y + c3.y * z, c2.z + c3.z * z, c2.w + c3.w * z),
			c3)

	def translate(vec: Vec3): Mat4 = translate(vec.x, vec.y, vec.z)

	def scale(x: Float, y: Float, z: Float): Mat4 =
		Mat4(
			Vec4(c0.x * x, c0.y * x, c0.z * x, c0.w * x),
			Vec4(c1.x * y, c1.y * y, c1.z * y, c1.w * y),
			Vec4(c2.x * z, c2.y * z, c2.z * z, c2.w * z),
			c3)

	def scale(vec: Vec3): Mat4 = scale(vec.x, vec.y, vec.z)

	def store(buffer: FloatBuffer) {
		c0.store(buffer)
		c1.store(buffer)
		c2.store(buffer)
		c3.store(buffer)
	}
}
