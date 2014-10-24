package sublixt.math

private[math] trait SquareRootFunctions {
	def sqrt(x: Float) = {
		val i = java.lang.Float.floatToIntBits(x)
		val b = (1 << 29) + (i >> 1) - (1 << 22)
		val f = java.lang.Float.intBitsToFloat(b)
		0.5f * (f + x / f)
	}

	def invSqrt(x: Float) = {
		val halfX = x * 0.5f
		val i = 0x5f3759df - (java.lang.Float.floatToIntBits(x) >> 1)
		val y = java.lang.Float.intBitsToFloat(i)
		y * (1.5f - halfX * y * y)
	}
}