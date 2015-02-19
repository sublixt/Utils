package sublixt.math

import java.lang.Float._
import java.lang.Double._

private[math] trait RoundingFunctions {
	final def floor(x: Float) = {
		val xi = x.toInt
		if (x < xi) xi - 1 else xi
	}

	final def floor(x: Double) = {
		val xi = x.toLong
		if (x < xi) xi - 1 else xi
	}

	final def ceil(x: Float) = {
		val x1 = x.toInt
		if (x > x1) x1 + 1 else x1
	}

	final def ceil(x: Double) = {
		val x1 = x.toLong
		if (x > x1) x1 + 1 else x1
	}

	final def round(x: Float) =
		floor(x + 0.5f)

	final def round(x: Double) =
		floor(x + 0.5)

	final def abs(x: Float) = {
		val i = floatToIntBits(x) & 0x7FFFFFFF
		intBitsToFloat(i)
	}

	final def abs(x: Int) = {
		val y = x >> 31
		(x ^ y) - y // twos complement when negative
	}

	final def abs(x: Double) = {
		val l = doubleToLongBits(x) & 0x7FFFFFFFFFFFFFFFL
		longBitsToDouble(l)
	}

	final def abs(x: Long) = {
		val y = x >> 63
		(x ^ y) - y
	}
}

/*object Main extends App {
	def floor1(x: Float) = {
		val bits = floatToIntBits(x)

		val sign = bits >> 31
		val signBit = sign & 0x1
		val exponent = bits >>> 23
		val exponent23 = (22 - exponent) & 0x7F
		val magnitude = (bits & 0x7FFFFF) | 0x800000

		val characteristic = magnitude >>> exponent23
		val characteristicShifted = characteristic << exponent23
		val mantissa = magnitude ^ characteristicShifted
		val preTest = (characteristicShifted - mantissa) >>> exponent23
		val integerTest = (characteristic ^ preTest) & signBit

		val twosComp = (characteristic ^ sign) - sign
		twosComp - integerTest
	}

	def floor2(x: Float) = {
		val xi = x.toInt
		if (x < xi) xi - 1 else xi
	}

	def floor3(x: Float) = {
		//most promising so far
		//but still slower than the comparison
		//also has problems with numbers that have a really small and really large exponent
		val bits = floatToIntBits(x)

		val sign = bits >> 31
		val exponent = (22 - (bits >>> 23)) & 0x7F // Magic
		val magnitude = (bits & 0x7FFFFF) | 0x800000

		val characteristic = magnitude >>> exponent
		val twosComp = (characteristic ^ sign) - sign

		val mantissa = magnitude << (32 - exponent)
		val realTest = ((0 - mantissa) & sign) >>> 31

		twosComp - realTest

		(((magnitude >>> exponent) ^ sign) - sign) -
			(((0 - (magnitude << (32 - exponent))) & sign) >>> 31)
	}

	val BIG_ENOUGH_INT = 16 * 1024
	val BIG_ENOUGH_FLOOR = BIG_ENOUGH_INT
	val BIG_ENOUGH_ROUND = BIG_ENOUGH_INT + 0.5

	def floor4(x: Float) = {
		(x + BIG_ENOUGH_FLOOR).toInt - BIG_ENOUGH_INT
	}

	val values = (-1000000 until 1000000).map(_ / 10000.0f).toArray

	locally {
		var rng = xorShift(-1)
		rng = xorShift(rng)
		rng = xorShift(rng)

		var i = 0
		while (i < 2000000) {
			val swap_i = (rng & (2000000 - 1)).toInt
			val swap = values(swap_i)
			values(swap_i) = values(i)
			values(i) = swap

			rng = xorShift(rng)
			i += 1
		}
	}

	System.gc()

	val start = System.nanoTime
	var c = 0
	while (c < 2000000) {
		floor4(values(c))
		c += 1
	}
	val end = System.nanoTime

	println((end - start) / 1000000.0f)

	println(floor3(0.00000000042f))

	def printBits(x: Int) {
		for (i <- 31 to 0 by -1) {
			print((x >>> i) & 0x1)
			if (i % 4 == 0) print(' ')
		}
		println
	}

	def printBits(x: Byte) {
		for (i <- 7 to 0 by -1) {
			print((x >>> i) & 0x1)
			if (i % 4 == 0) print(' ')
		}
		println
	}

	def printFBits(x: Int) {
		print((x >>> 31) & 0x1)
		print(" : ")

		for (i <- 30 until 22 by -1) {
			print((x >>> i) & 0x1)
			if (i == 27) print(' ')
		}
		print(" : (1)")

		for (i <- 22 to 0 by -1) {
			print((x >>> i) & 0x1)
			if (i % 4 == 0) print(' ')
		}

		println
	}

	//always returns a negative number
	def invAbs(x: Int) = {
		val y = (~x) >> 31
		(x ^ y) - y
	}

	//so far this == floor(abs(x))
	def absFloor(bits: Int) = {
		//val bits = floatToIntBits(x)
		//val sign = bits >> 31
		val exp = (bits >>> 23) & 0xFF
		val mag = (bits & 0x7fffff) | 0x800000
		mag >>> (23 - (exp - 127))
	}

	//this works, but probably not faster
	//Unexpected results at -infinity, infinity, NaN
	def ffloor(x: Float) = {
		val bits = floatToIntBits(x)

		// seperate the float into:
		//	* sign - 1 bit
		//	* exponent - 8 bits
		//	* mantissa - 23 bits (24 with implicit 1)
		val sign = bits >> 31 // I need this for 2's complement
		val signBit = sign & 0x1 // I need this to seperate the positive and negative
		//val exp = ((bits >>> 23) & 0xFF) // I need this to cut out the decimal points
		val exponent = bits >>> 23 // the 0xFF is probably unnecessary. it seems to work without it due to & 0x7f in exp23
		//val exp23 = 23 - (exp - 127) // (< 0) floating point overflow, (<= 23) |x| >= 1, (> 23) |x| < 1
		val exponent23 = (22 - exponent) & 0x7F // (< 0) doesnt happen, (0 <= exp <= 23) |x| >= 1, (> 23) |x| < 1 or unexpected behavior
		val magnitude = (bits & 0x7FFFFF) | 0x800000 // the fraction of the number with the implicit one added

		val characteristic = magnitude >>> exponent23 // the whole number portion of x. = floor(abs(x))

		// these next four lines separate out the real numbers from integers
		val characteristicShifted = characteristic << exponent23 // if it's a whole number this should equal mag
		val mantissa = magnitude ^ characteristicShifted // the decimal portion of x. 0 if integer
		val preTest = (characteristicShifted - mantissa) >>> exponent23 // (integer) floor(abs(x)), (real) floor(abs(x)) - 1
		val integerTest = (characteristic ^ preTest) & signBit // (negative and real) 1, (positive or integer) 0

		val twosComp = (characteristic ^ sign) - sign // gets the number back to the initial sign
		twosComp - integerTest
	}

	//println(ffloor1(8388608.5f))

	for (i <- (127 - 24) to (127 + 24)) {
		println(i, 23 - (i - 127), (22 - i) & 0x7f)
	}

	def fastRoundf(x: Float) = {
		val bits = floatToIntBits(x)
		val exp = (bits >>> 23) & 0xFF
		val sign = bits >> 31
		val point5 = (0x00400000 - sign) >> (exp - 127)
		intBitsToFloat(bits + point5).toInt
	}
}*/
