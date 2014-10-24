package sublixt.math

private[math] trait RoundingFunctions {
	private val BASE = 16 * 1024
	private val ROUND = BASE + 0.5

	/** Rounds a float to the closes integer.
	  *
	  * Only works on floats from [-(2^14), Float.MAX_VALUE - 2^14]
	  */
	def round(x: Float) = (x + ROUND).toInt - BASE

	/** Rounds an float to the closest integer that is less than 
	  * or equal to the specified float.
	  *
	  * Only works on floats from [-(2^14), Float.MAX_VALUE - 2^14]
	  */
	def floor(x: Float) = (x + BASE).toInt - BASE

	/** Rounds an float to the closest integer that is greater than
	  * or equal to the specified float.
	  *
	  * Only works on floats from [-(2^14), Float.MAX_VALUE - 2^14]
	  */
	def ceil(x: Float) = BASE - (BASE - x).toInt

	/** Replaces the sign bit of a 32-bit float with a 0.
	  */
	def abs(x: Float) = {
		val i = java.lang.Float.floatToIntBits(x) & 0x7FFFFFFF
		java.lang.Float.intBitsToFloat(i)
	}

	/** Replaces the sign bit of a 32-bit int with a 0.
	  */
	def abs(x: Int) = {
		val y = x >> 31
		(x ^ y) - y
	}
}