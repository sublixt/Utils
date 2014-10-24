package sublixt.math

private[math] trait TrigFunctions {
	private val SIN_BITS = 12
	private val SIN_MASK = ~(-1 << SIN_BITS)
	private val SIN_COUNT = SIN_MASK + 1
	private val SIN_RAD_TO_INDEX = SIN_COUNT / TWOPI

	private val sinTable =
		(for (i <- 0 until SIN_COUNT) yield {
			scala.math.sin((i + 0.5f) / SIN_COUNT * TWOPI).toFloat
		}).toArray

	/** Sine function that uses a lookup table.
	  *
	  * @param rad angle in radians
	  */
	def sin(rad: Float) =
		sinTable((rad * SIN_RAD_TO_INDEX).toInt & SIN_MASK)

	/** Cosine function that uses a lookup table.
	  *
	  * @param rad angle in radians
	  */
	def cos(rad: Float) = sin(HALFPI - rad)

	/** Tangent function that uses a lookup table.
	  *
	  * @param rad angle in radians
	  */
	def tan(rad: Float) = sin(rad) / cos(rad)

	/** Cosecent function that uses a lookup table.
	  *
	  * @param rad angle in radians
	  */
	def csc(rad: Float) = 1 / sin(rad)

	/** Secent function that uses a lookup table.
	  *
	  * @param rad angle in radians
	  */
	def sec(rad: Float) = 1 / cos(rad)

	/** Cotangent function that uses a lookup table.
	  *
	  * @param rad angle in radians
	  */
	def cot(rad: Float) = cos(rad) / sin(rad)

	//TODO try to use a lookup table
	/** atan2 function that uses an approximation.
	  *
	  * @param rad angle in radians
	  */
	def atan2(y: Float, x: Float) = {
		if (x == 0.0f) {
			if (y > 0.0f) HALFPI
			else if (y == 0.0f) 0.0f
			else -HALFPI
		} else {
			val z = y / x
			if (abs(z) < 1.0f) {
				val atan = z / (1.0f + 0.28f * z * z)
				if (x < 0.0f)
					if (y < 0.0f) atan - PI
					else atan + PI
				else atan
			} else {
				val atan = HALFPI - z / (z * z + 0.28f)
				if (y < 0.0f) atan - PI
				else atan
			}
		}
	}

	//TODO acos and asin can use a lookup table with the domain [-1, 1]
	/** Arcsine function that uses atan2 for approximation.
	  *
	  * @param rad angle in radians
	  */
	def asin(x: Float) = atan2(x, sqrt((1.0f + x) * (1.0f - x)))

	/** Arccosine function that uses atan2 for approximation.
	  *
	  * @param rad angle in radians
	  */
	def acos(x: Float) = atan2(sqrt((1.0f + x) * (1.0f - x)), x)

	/** Arctangent function that uses an approximation.
	  *
	  * @param rad angle in radians
	  */
	def atan(rad: Float) = (0.97239f * rad) - (0.19195f * rad * rad * rad)

	/** Arccosecent function that uses atan2 for approximation.
	  *
	  * @param rad angle in radians
	  */
	def acsc(x: Float) = asin(1.0f / x)

	/** Arcsecent function that uses atan2 for approximation.
	  *
	  * @param rad angle in radians
	  */
	def asec(x: Float) = acos(1.0f / x)

	/** Arccotangent function that uses an approximation.
	  *
	  * @param rad angle in radians
	  */
	def acot(rad: Float) = atan(1.0f / rad)
}