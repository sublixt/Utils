package sublixt.math

private[math] trait LogFunctions {
	def log(x: Float, base: Float) = ln(x) / ln(base)

	def ln(x: Float) = {
		val y = (x - 1) / (x + 1)
		val y2 = y * y
		y * 2 * (15 - y2 * 4) / (15 - y2 * 9)
	}
}