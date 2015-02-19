package sublixt.math

private[math] trait TrigFunctions {
	private final val SIN_BITS = 12
	private final val SIN_MASK = ~(-1 << SIN_BITS)
	private final val SIN_COUNT = SIN_MASK + 1
	private final val SIN_RAD_TO_INDEX = SIN_COUNT / TWOPI

	private final val sinTable =
		(for (i <- 0 until SIN_COUNT) yield {
			scala.math.sin((i + 0.5f) / SIN_COUNT * TWOPI).toFloat
		}).toArray

	final def sin(rad: Float) =
		sinTable((rad * SIN_RAD_TO_INDEX).toInt & SIN_MASK)
	final def cos(rad: Float) = sin(HALFPI - rad)
	final def tan(rad: Float) = sin(rad) / cos(rad)
	final def csc(rad: Float) = 1 / sin(rad)
	final def sec(rad: Float) = 1 / cos(rad)
	final def cot(rad: Float) = cos(rad) / sin(rad)

	final def atan2(y: Float, x: Float) = {
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

	private final val ASIN_BITS = 12
	private final val ASIN_MASK = ~(-1 << ASIN_BITS)
	private final val ASIN_TO_INDEX = ASIN_MASK / 2f
	private final val asinTable =
		(for (x <- 0 to ASIN_MASK) yield {
			scala.math.asin((x.toFloat / ASIN_MASK * 2) - 1).toFloat
		}).toArray

	final def asin(x: Float) =
		asinTable(((x + 1) * ASIN_TO_INDEX).toInt & ASIN_MASK)
	final def acos(x: Float) = HALFPI - asin(x)
	final def atan(x: Float) = atan2(x, 1)
	final def acsc(x: Float) = asin(1.0f / x)
	final def asec(x: Float) = acos(1.0f / x)
	final def acot(x: Float) = atan2(1, x)
}
