package sublixt.math

trait RNGFunctions {
	final def xorShift(x: Long) = {
		var x1 = x
		x1 ^= (x1 << 21)
		x1 ^= (x1 >>> 35)
		x1 ^= (x1 << 4)
		x1 * 2685821657736338717L
	}
}
