package sublixt.math

private[math] trait MathConstants {
	/**PI as a 32-bit float*/
	val PI = 3.1415927f
	/**PI * 2 as a 32-bit float*/
	val TWOPI = PI * 2
	/**PI / 2 as a 32-bit float*/
	val HALFPI = PI / 2

	/**The ratio to convert degrees to radians.*/
	val degToRad = PI / 180.0f
	/**The ratio to convert radians to degrees.*/
	val radToDeg = 180.0f / PI
}