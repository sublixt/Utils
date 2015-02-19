package sublixt

package object math
		extends MathConstants
		with SquareRootFunctions
		with TrigFunctions
		with RoundingFunctions
		with RNGFunctions {

	implicit class floatToScalar(val scalar: Float) extends AnyVal {
		def *(vec: Vec2) = vec * scalar
		def *(vec: Vec3) = vec * scalar
		def *(vec: Vec4) = vec * scalar
		def *(mat: Mat2) = mat * scalar
		def *(mat: Mat3) = mat * scalar
		def *(mat: Mat4) = mat * scalar
		def *(quat: Quat) = quat * scalar
		def +(vec: Vec2) = vec + scalar
		def +(vec: Vec3) = vec + scalar
		def +(vec: Vec4) = vec + scalar
		def +(mat: Mat2) = mat + scalar
		def +(mat: Mat3) = mat + scalar
		def +(mat: Mat4) = mat + scalar
	}

	//Faster than java.lang.Math.min, but a little less secure
	final def min(x: Float, y: Float) =
		if (x < y) x else y

	//Faster than java.lang.Math.max, but a little less secure
	final def max(x: Float, y: Float) =
		if (x > y) x else y

	final def clamp(x: Float, min: Float, max: Float) =
		if (x < min) min
		else if (x > max) max
		else x
}
