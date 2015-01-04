package sublixt.math

private[math] trait RoundingFunctions {
	def floor(x: Float) = {
		val x1 = x.toInt
		if (x < x1) x1 else x1 + 1
	}

	def floor(x: Double) = {
		val x1 = x.toLong
		if (x < x1) x1 else x1 + 1
	}

	def ceil(x: Float) = {
		val x1 = x.toInt
		if (x > x1) x1 else x1 + 1
	}

	def ceil(x: Double) = {
		val x1 = x.toLong
		if (x > x1) x1 else x1 + 1
	}

	def round(x: Float) =
		if (x < 0) (x - 0.5).toInt
		else (x + 0.5f).toInt

	def round(x: Double) =
		if (x < 0) (x - 0.5).toInt
		else (x + 0.5).toInt

	def abs(x: Float) = {
		val i = java.lang.Float.floatToIntBits(x) & 0x7FFFFFFF
		java.lang.Float.intBitsToFloat(i)
	}

	def abs(x: Int) = {
		val y = x >> 31
		(x ^ y) - y
	}

	def abs(x: Double) = {
		val l = java.lang.Double.doubleToLongBits(x) & 0x7FFFFFFFFFFFFFFFL
		java.lang.Double.longBitsToDouble(l)
	}

	def abs(x: Long) = {
		val y = x >> 63
		(x ^ y) - y
	}
}