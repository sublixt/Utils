package sublixt.math

object Quat {
	val zero = Quat(0, 0, 0, 0)
	def apply(): Quat = zero

	def apply(bank: Float, heading: Float, attitude: Float): Quat = {
		val c1 = cos(heading / 2)
		val c2 = cos(attitude / 2)
		val c3 = cos(bank / 2)
		val s1 = sin(heading / 2)
		val s2 = sin(attitude / 2)
		val s3 = sin(bank / 2)
		
		Quat(
			s1 * s2 * c3 + c1 * c2 * s3, 
			s1 * c2 * c3 + c1 * s2 * s3, 
			c1 * s2 * c3 - s1 * c2 * s3,
			c1 * c2 * c3 - s1 * s2 * s3)
	}

	def apply(euler: Vec3): Quat = Quat(euler.x, euler.y, euler.z)
}

//make sure you normalize the quaternion before doing vector
//and matrix operations
case class Quat(val x: Float, val y: Float, val z: Float, val w: Float) {
	def unary_- = Quat(-x, -y, -z, -w)
	def conjugate = Quat(-x, -y, -z, w)
	def dot(other: Quat) = x * other.x + y * other.y + z * other.z + w * other.w

	def magnitudeSquared = x * x + y * y + z * z + w * w
	def magnitude = sqrt(magnitudeSquared)
	def normalize = this / magnitude

	def invert = {
		val m = magnitude
		Quat(-x / m, -y / m, -z / m, w / m)
		//conjugate / magnitude
	}

	def toRotationMatrix = {
		val xx = x * x * 2
		val yy = y * y * 2
		val zz = z * z * 2
		val xy = x * y * 2
		val xz = x * z * 2
		val xw = x * w * 2
		val yz = y * z * 2
		val yw = y * w * 2
		val zw = z * w * 2
		
		Mat4(
			Vec4(1 - yy - zz, xy + zw, xz - yw, 0),
			Vec4(xy - zw, 1 - xx - zz, yz + xw, 0),
			Vec4(xz + yw, yz - xw, 1 - xx - yy, 0),
			Vec4(0, 0, 0, 1))
	}

	def toEuler = {
		val test = x * y + z * w
		if (test > 0.499f) Vec3(2 * atan2(x, w), HALFPI, 0) //north pole singularity
		else if (test < -0.499f) Vec3(-2 * atan2(x, w), -HALFPI, 0) //south pole singularity
		else {
			val xs = x * x
			val ys = y * y
			val zs = z * z
			Vec3(
				atan2(2 * x * w - 2 * y * z, 1 - 2 * xs - 2 * zs),
				atan2(2 * y * w - 2 * x * z, 1 - 2 * ys - 2 * zs),
				asin(2 * test))
		}
	}

	def *(vec: Vec4) = {
		val tx = 2 * (y * vec.z - z * vec.y)
		val ty = 2 * (z * vec.x - x * vec.z)
		val tz = 2 * (x * vec.y - y * vec.x)
		Vec4(vec.x + w * tx + (y * tz - z * ty), vec.y + w * ty + (z * tx - x * tz), vec.z + w * tz + (x * ty - y * tx), vec.w)
		//val t = 2 * (xyz cross vec)
		//vec + w * t + (xyz cross t)
	}

	def +(other: Quat) = Quat(x + other.x, y + other.y, z + other.z, w + other.w)
	def -(other: Quat) = Quat(x - other.x, y + other.y, z + other.z, w + other.w)
	def *(other: Quat) =
		Quat(
			w * other.x + x * other.w + y * other.z - z * other.y,
			w * other.y + y * other.w - x * other.z + z * other.x,
			w * other.z + z * other.w + x * other.y - y * other.x,
			w * other.w - x * other.x - y * other.y - z * other.z)

	def *(scalar: Float) = Quat(x * scalar, y * scalar, z * scalar, w * scalar)
	def /(scalar: Float) = Quat(x / scalar, y / scalar, z / scalar, w / scalar)
}