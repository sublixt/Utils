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

	def sin(rad: Float) =
		sinTable((rad * SIN_RAD_TO_INDEX).toInt & SIN_MASK)
	def cos(rad: Float) = sin(HALFPI - rad)
	def tan(rad: Float) = sin(rad) / cos(rad)
	def csc(rad: Float) = 1 / sin(rad)
	def sec(rad: Float) = 1 / cos(rad)
	def cot(rad: Float) = cos(rad) / sin(rad)

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

	private val ASIN_BITS = 12
	private val ASIN_MASK = ~(-1 << ASIN_BITS)
	private val ASIN_TO_INDEX = ASIN_MASK / 2f
	private val asinTable =
		(for (x <- 0 to ASIN_MASK) yield {
			scala.math.asin((x.toFloat / ASIN_MASK * 2) - 1).toFloat
		}).toArray

	def asin(x: Float) =
		asinTable(((x + 1) * ASIN_TO_INDEX).toInt & ASIN_MASK)
	def acos(x: Float) = HALFPI - asin(x)
	def atan(x: Float) = atan2(x, 1)
	def acsc(x: Float) = asin(1.0f / x)
	def asec(x: Float) = acos(1.0f / x)
	def acot(x: Float) = atan2(1, x)
}