package sublixt.math

class Hash(val seed: Long) {
	private val hashLength = 1 << 12
	private val tableSize = hashLength * 2
	private val hash = (-hashLength until hashLength).map(_.toFloat / hashLength).toArray
	locally {
		var rng = xorShift(seed)
		for (i <- -hashLength until hashLength) {
			val swap_i = (rng % hashLength).toInt
			val swap = hash(swap_i)
			hash(swap_i) = hash(i)
			hash(i) = swap
			rng = xorShift(rng)
		}
	}
}