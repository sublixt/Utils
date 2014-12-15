package sublixt.math

object Random {
	/* Creates a Random with a specified seed.
	 * 
	 * @param seed the seed for the rng. Must be greater than 0 and less than Long.MaxValue.
	 * */
	def apply(seed: Long) = {
		require(seed > 0 && seed < Long.MaxValue)
		new Random(seed, ~seed)
	}
}

/* An imutable, thread safe implementation of the Multiply-with-Carry PRNG.
 * */
class Random private (val x: Long, val y: Long) {
	/* Returns the Long representation of this pseudorandom number
	 * */
	def toLong = ((x << 16) + (y & 0xFFFF))

	/* Returns the Int representation of this pseudorandom number
	 * */
	def toInt = (toLong & 0x7FFFFFFF).toInt

	/* Returns the Double representation of this pseudorandom number
	 * */
	def toDouble = toLong.toDouble / 0x7FFFFFFFFFFFFFFFL

	/* Returns the Float representation of this pseudorandom number
	 * */
	def toFloat = toInt.toFloat / 0x7FFFFFFF

	/* Returns the Boolean representation of this pseudorandom number
	 * */
	def toBoolean = toLong % 2 == 1

	/* Calculates the next random number in the sequence.
	 * 
	 * @return a new Random that contains the next pseudorandom number in the sequence
	 * */
	def next = new Random(18000 * (x & 0xFFFF) + (x >> 16), 30903 * (y & 0xFFFF) + (y >> 16))
}
