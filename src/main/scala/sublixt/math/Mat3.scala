package sublixt.math

object Mat3 {
	val identity =
		Mat3(
			Vec3(1, 0, 0),
			Vec3(0, 1, 0),
			Vec3(0, 0, 1))
			
	val zero = Mat3(Vec3(), Vec3(), Vec3())

	def apply(mat: Mat2): Mat3 = Mat3(Vec3(mat.c0, 0), Vec3(mat.c1, 0), Vec3(0, 0, 1))
	def apply(mat: Mat4): Mat3 = Mat3(mat.c0.xyz, mat.c1.xyz, mat.c2.xyz)
	def apply(): Mat3 = identity
}

case class Mat3(val c0: Vec3, val c1: Vec3, val c2: Vec3) {
	def unary_- = Mat3(-c0, -c1, -c2)
	def +(scalar: Float) = Mat3(c0 + scalar, c1 + scalar, c2 + scalar)
	def -(scalar: Float) = Mat3(c0 - scalar, c1 - scalar, c2 - scalar)
	def *(scalar: Float) = Mat3(c0 * scalar, c1 * scalar, c2 * scalar)
	def /(scalar: Float) = Mat3(c0 / scalar, c1 / scalar, c2 / scalar)

	def +(other: Mat3) = Mat3(c0 + other.c0, c1 + other.c1, c2 + other.c2)
	def -(other: Mat3) = Mat3(c0 - other.c0, c1 - other.c1, c2 - other.c2)

	def *(other: Mat3) =
		Mat3(
			Vec3(
				c0.x * other.c0.x + c1.x * other.c0.y + c2.x * other.c0.z,
				c0.y * other.c0.x + c1.y * other.c0.y + c2.y * other.c0.z,
				c0.z * other.c0.x + c1.z * other.c0.y + c2.z * other.c0.z),
			Vec3(
				c0.x * other.c1.x + c1.x * other.c1.y + c2.x * other.c1.z,
				c0.y * other.c1.x + c1.y * other.c1.y + c2.y * other.c1.z,
				c0.z * other.c1.x + c1.z * other.c1.y + c2.z * other.c1.z),
			Vec3(
				c0.x * other.c2.x + c1.x * other.c2.y + c2.x * other.c2.z,
				c0.y * other.c2.x + c1.y * other.c2.y + c2.y * other.c2.z,
				c0.z * other.c2.x + c1.z * other.c2.y + c2.z * other.c2.z))

	def *(vec: Vec3) =
		Vec3(
			c0.x * vec.x + c1.x * vec.y + c2.x * vec.z,
			c0.y * vec.x + c1.y * vec.y + c2.y * vec.z,
			c0.z * vec.x + c1.z * vec.y + c2.z * vec.z)

	def apply(i: Int) =
		i match {
			case 0 => c0
			case 1 => c1
			case 2 => c2
			case _ => throw new IndexOutOfBoundsException("[Mat3](apply(int)): " + i.toString + " not in range [0, 2]")
		}

	lazy val transpose =
		Mat3(
			Vec3(c0.x, c1.x, c2.x),
			Vec3(c0.y, c1.y, c2.y),
			Vec3(c0.z, c1.z, c2.z))

	lazy val determinant =
		c0.x * c1.y * c2.z + c1.x * c2.y * c0.z + c2.x * c0.y * c1.z -
			c0.z * c1.y * c2.x - c1.z * c2.y * c0.x - c2.z * c0.y * c1.x

	lazy val inverse = {
		val d = determinant
		Mat3(
			Vec3((c2.z * c1.y - c1.z * c2.y) / d, -(c2.z * c0.y - c0.z * c2.y) / d, (c1.z * c0.y - c0.z * c1.y) / d),
			Vec3(-(c2.z * c1.x - c1.z * c2.x) / d, (c2.z * c0.x - c0.z * c2.x) / d, -(c1.z * c0.x - c0.z * c1.x) / d),
			Vec3((c2.y * c1.x - c1.y * c2.x) / d, -(c2.y * c0.x - c0.y * c2.x) / d, (c1.y * c0.x - c0.y * c1.x) / d))
	}
}