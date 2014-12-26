package sublixt.math

object Mat2 {
	val identity =
		Mat2(
			Vec2(1, 0),
			Vec2(0, 1))
			
	val zero = Mat2(Vec2(), Vec2())

	def apply(mat: Mat3): Mat2 = Mat2(mat.c0.xy, mat.c1.xy)
	def apply(mat: Mat4): Mat2 = Mat2(mat.c0.xy, mat.c1.xy)
	def apply(): Mat2 = identity
}

case class Mat2(val c0: Vec2, val c1: Vec2) {
	def unary_- = Mat2(-c0, -c1)
	def +(scalar: Float) = Mat2(c0 + scalar, c1 + scalar)
	def -(scalar: Float) = Mat2(c0 - scalar, c1 - scalar)
	def *(scalar: Float) = Mat2(c0 * scalar, c1 * scalar)
	def /(scalar: Float) = Mat2(c0 / scalar, c1 / scalar)
	
	def +(other: Mat2) = Mat2(c0 + other.c0, c1 + other.c1)
	def -(other: Mat2) = Mat2(c0 - other.c0, c1 - other.c1)

	def *(other: Mat2) =
		Mat2(
			Vec2(
				c0.x * other.c0.x + c1.x * other.c0.y,
				c0.y * other.c0.x + c1.y * other.c0.y),
			Vec2(
				c0.x * other.c1.x + c1.x * other.c1.y,
				c0.y * other.c1.x + c1.y * other.c1.y))

	def *(vec: Vec2) =
		Vec2(
			c0.x * vec.x + c1.x * vec.y,
			c0.y * vec.x + c1.y * vec.y)

	def apply(i: Int) =
		i match {
			case 0 => c0
			case 1 => c1
			case _ => throw new IndexOutOfBoundsException("[Mat2](apply(int)): " + i.toString + " not in range [0, 1]")
		}

	lazy val transpose =
		Mat2(
			Vec2(c0.x, c1.x),
			Vec2(c0.y, c1.y))
	
	lazy val determinant =
		c0.x * c1.y - c0.y * c1.x
		
	lazy val inverse = {
		val d = determinant
		Mat2(Vec2(c1.y / d, -c0.y / d), Vec2(-c1.x / d, c0.x / d))
	}
}