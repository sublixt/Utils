package sublixt.math

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

	def translate(x: Float, y: Float, z: Float) =
		Mat4(Vec4(1, 0, 0, x), Vec4(0, 1, 0, y), Vec4(0, 0, 1, z), Vec4(0, 0, 0, 1))
	def translate(vec: Vec3): Mat4 =
		translate(vec.x, vec.y, vec.z)

	def rotx(angle: Float) =
		Mat4(
			Vec4(1, 0, 0, 0),
			Vec4(0, cos(angle), sin(angle), 0),
			Vec4(0, -sin(angle), cos(angle), 0),
			Vec4(0, 0, 0, 1))

	def roty(angle: Float) =
		Mat4(
			Vec4(cos(angle), 0, -sin(angle), 0),
			Vec4(0, 1, 0, 0),
			Vec4(sin(angle), 0, cos(angle), 0),
			Vec4(0, 0, 0, 1))

	def rotz(angle: Float) =
		Mat4(
			Vec4(cos(angle), sin(angle), 0, 0),
			Vec4(-sin(angle), cos(angle), 0, 0),
			Vec4(0, 0, 1, 0),
			Vec4(0, 0, 0, 1))

	def scale(x: Float, y: Float, z: Float) =
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

	def transpose =
		Mat4(
			Vec4(c0.x, c1.x, c2.x, c3.x),
			Vec4(c0.y, c1.y, c2.y, c3.y),
			Vec4(c0.z, c1.z, c2.z, c3.z),
			Vec4(c0.w, c1.w, c3.w, c3.w))

	def determinant =
		(c0.x * (c1.y * c2.z * c3.w + c2.y * c3.z * c1.w + c3.y * c1.z * c2.w -
			c1.w * c2.z * c3.y - c2.w * c3.z * c1.y - c3.w * c1.z * c2.y)) -
			(c1.x * (c0.y * c2.z * c3.w + c2.y * c3.z * c0.w + c3.y * c0.z * c2.w -
				c0.w * c2.z * c3.y - c2.w * c3.z * c0.y - c3.w * c0.z * c2.y)) +
				(c2.x * (c0.y * c1.z * c3.w + c1.y * c3.z * c0.w + c3.y * c0.z * c1.w -
					c0.w * c1.z * c3.y - c1.w * c3.z * c0.y - c3.w * c0.z * c1.y)) -
					(c3.x * (c0.y * c1.z * c2.w + c1.y * c2.z * c0.w + c2.y * c0.z * c1.w -
						c0.w * c1.z * c2.y - c1.w * c2.z * c0.y - c2.w * c0.z * c1.y))

	def inverse = {
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

	def rotx(angle: Float) =
		Mat4(
			c0,
			Vec4(c1.x * cos(angle) + c2.x * sin(angle), c1.y * cos(angle) + c2.y * sin(angle), c1.z * cos(angle) + c2.z * sin(angle), c1.w * cos(angle) + c2.w * sin(angle)),
			Vec4(c2.x * cos(angle) - c1.x * sin(angle), c2.y * cos(angle) - c1.y * sin(angle), c2.z * cos(angle) - c1.z * sin(angle), c2.w * cos(angle) - c1.w * sin(angle)),
			c3)

	def roty(angle: Float) =
		Mat4(
			Vec4(c0.x * cos(angle) - c2.x * sin(angle), c0.y * cos(angle) - c2.y * sin(angle), c0.z * cos(angle) - c2.z * sin(angle), c0.w * cos(angle) - c2.w * sin(angle)),
			c1,
			Vec4(c0.x * sin(angle) + c2.x * cos(angle), c0.y * sin(angle) + c2.y * cos(angle), c0.z * sin(angle) + c2.z * cos(angle), c0.w * sin(angle) + c2.w * cos(angle)),
			c3)

	def rotz(angle: Float) =
		Mat4(
			Vec4(c0.x * cos(angle) + c1.x * sin(angle), c0.y * cos(angle) + c1.y * sin(angle), c0.z * cos(angle) + c1.z * sin(angle), c0.w * cos(angle) + c1.w * sin(angle)),
			Vec4(c1.x * cos(angle) - c0.x * sin(angle), c1.y * cos(angle) - c0.y * sin(angle), c1.z * cos(angle) - c0.z * sin(angle), c1.w * cos(angle) - c0.w * sin(angle)),
			c2,
			c3)

	def translate(x: Float, y: Float, z: Float) =
		Mat4(
			Vec4(c0.x + c3.x * x, c0.y + c3.y * x, c0.z + c3.z * x, c0.w + c3.w * x),
			Vec4(c1.x + c3.x * y, c1.y + c3.y * y, c1.z + c3.z * y, c1.w + c3.w * y),
			Vec4(c2.x + c3.x * z, c2.y + c3.y * z, c2.z + c3.z * z, c2.w + c3.w * z),
			c3)

	def translate(vec: Vec3): Mat4 = translate(vec.x, vec.y, vec.z)

	def scale(x: Float, y: Float, z: Float) =
		Mat4(
			Vec4(c0.x * x, c0.y * x, c0.z * x, c0.w * x),
			Vec4(c1.x * y, c1.y * y, c1.z * y, c1.w * y),
			Vec4(c2.x * z, c2.y * z, c2.z * z, c2.w * z),
			c3)

	def scale(vec: Vec3): Mat4 = scale(vec.x, vec.y, vec.z)
}