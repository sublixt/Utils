package sublixt

package object math
	extends MathConstants
	with SquareRootFunctions
	with TrigFunctions
	with RoundingFunctions {
	
	implicit class floatToScalar(val scalar: Float) extends AnyVal {
		def *(vec: Vec2) = Vec2(scalar * vec.x, scalar * vec.y)
		def *(vec: Vec3) = Vec3(scalar * vec.x, scalar * vec.y, scalar * vec.z)
		def *(vec: Vec4) = Vec4(scalar * vec.x, scalar * vec.y, scalar * vec.z, scalar * vec.w)
		def +(vec: Vec2) = Vec2(scalar + vec.x, scalar + vec.y)
		def +(vec: Vec3) = Vec3(scalar + vec.x, scalar + vec.y, scalar + vec.z)
		def +(vec: Vec4) = Vec4(scalar + vec.x, scalar + vec.y, scalar + vec.z, scalar + vec.w)
		//TODO im not exactly sure based on the specification if subtraction and division in
		//(scalar op vector) form is defined. I will need to test it in glsl to before I 
		//implement it
	}
}
