package sublixt.math

import java.lang.Float._
import java.lang.Double._

private[math] trait RoundingFunctions {
	def floor(x: Float) = {
		val xi = x.toInt
		if (x < xi) xi - 1 else xi
	}

	def floor(x: Double) = {
		val xi = x.toLong
		if (x < xi) xi - 1 else xi
	}

	def ceil(x: Float) = {
		val x1 = x.toInt
		if (x > x1) x1 + 1 else x1
	}

	def ceil(x: Double) = {
		val x1 = x.toLong
		if (x > x1) x1 + 1 else x1
	}

	def round(x: Float) =
		floor(x + 0.5f)

	def round(x: Double) =
		floor(x + 0.5)

	def abs(x: Float) = {
		val i = floatToIntBits(x) & 0x7FFFFFFF
		intBitsToFloat(i)
	}

	def abs(x: Int) = {
		val y = x >> 31
		(x ^ y) - y // twos complement when negative
	}

	def abs(x: Double) = {
		val l = doubleToLongBits(x) & 0x7FFFFFFFFFFFFFFFL
		longBitsToDouble(l)
	}

	def abs(x: Long) = {
		val y = x >> 63
		(x ^ y) - y
	}
}

object Main extends App {
	def printBits(x: Int) {
		for (i <- 31 to 0 by -1) {
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
	//I can see why floating point calculations are slow
	def ffloor(x: Float) = {
		val bits = floatToIntBits(x)

		// seperate the float into:
		//	* sign
		//	* exponent
		//	* mantissa
		val signBit = bits >>> 31 // I need this to seperate the positive and negative
		val sign = bits >> 31 // I need this for 2's complement
		val exp = ((bits >>> 23) & 0xFF) // I need this to cut out the decimal points
		val exp23 = 23 - (exp - 127) // (< 0) floating point overflow, (<= 23) |x| >= 1, (> 23) |x| < 1
		val mag = (bits & 0x7fffff) | 0x800000 // the fraction of the number with the implicit one added

		val stripped = mag >>> exp23 // the whole number portion of x. = floor(abs(x))

		// these next three lines separate out the real numbers from integers
		val magComp = stripped << exp23 // if it's a whole number this should equal mag
		val comp = mag ^ magComp // the decimal portion of x. 0 if integer
		val something = (magComp - comp) >>> exp23 // (integer) floor(abs(x)), (real) floor(abs(x)) - 1

		val twosComp = (stripped ^ sign) - sign // gets the number back to the initial sign
		val diff = (stripped ^ something) & signBit // (negative and real) 1, (positive or integer) 0
		twosComp - diff
	}

	println(ffloor(1.5f))

	def fastRoundf(x: Float) = {
		val bits = floatToIntBits(x)
		val exp = (bits >>> 23) & 0xFF
		val sign = bits >> 31
		val point5 = (0x00400000 - sign) >> (exp - 127)
		intBitsToFloat(bits + point5).toInt
	}
}
