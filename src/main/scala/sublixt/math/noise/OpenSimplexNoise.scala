package sublixt.math.noise

object OpenSimplexNoise {
	private val STRETCH_2D = -0.211324865405187 //(1/Math.sqrt(2+1)-1)/2
	private val STRETCH_3D = -1.0 / 6.0 //(1/Math.sqrt(3+1)-1)/3
	private val STRETCH_4D = -0.138196601125011 //(1/Math.sqrt(4+1)-1)/4
	private val SQUISH_2D = 0.366025403784439 //(Math.sqrt(2+1)-1)/2
	private val SQUISH_3D = 1.0 / 3.0 //(Math.sqrt(3+1)-1)/3
	private val SQUISH_4D = 0.309016994374947 //(Math.sqrt(4+1)-1)/4
	private val NORM_2D = 1.0 / 47.0
	private val NORM_3D = 1.0 / 103.0
	private val NORM_4D = 1.0 / 30.0

	private val gradients2D =
		Array(
			5, 2, 2, 5,
			-5, 2, -2, 5,
			5, -2, 2, -5,
			-5, -2, -2, -5
		)

	private val gradients3D =
		Array(
			-11, 4, 4, -4, 11, 4, -4, 4, 11,
			11, 4, 4, 4, 11, 4, 4, 4, 11,
			-11, -4, 4, -4, -11, 4, -4, -4, 11,
			11, -4, 4, 4, -11, 4, 4, -4, 11,
			-11, 4, -4, -4, 11, -4, -4, 4, -11,
			11, 4, -4, 4, 11, -4, 4, 4, -11,
			-11, -4, -4, -4, -11, -4, -4, -4, -11,
			11, -4, -4, 4, -11, -4, 4, -4, -11
		)

	private val gradients4D =
		Array(
			3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3,
			-3, 1, 1, 1, -1, 3, 1, 1, -1, 1, 3, 1, -1, 1, 1, 3,
			3, -1, 1, 1, 1, -3, 1, 1, 1, -1, 3, 1, 1, -1, 1, 3,
			-3, -1, 1, 1, -1, -3, 1, 1, -1, -1, 3, 1, -1, -1, 1, 3,
			3, 1, -1, 1, 1, 3, -1, 1, 1, 1, -3, 1, 1, 1, -1, 3,
			-3, 1, -1, 1, -1, 3, -1, 1, -1, 1, -3, 1, -1, 1, -1, 3,
			3, -1, -1, 1, 1, -3, -1, 1, 1, -1, -3, 1, 1, -1, -1, 3,
			-3, -1, -1, 1, -1, -3, -1, 1, -1, -1, -3, 1, -1, -1, -1, 3,
			3, 1, 1, -1, 1, 3, 1, -1, 1, 1, 3, -1, 1, 1, 1, -3,
			-3, 1, 1, -1, -1, 3, 1, -1, -1, 1, 3, -1, -1, 1, 1, -3,
			3, -1, 1, -1, 1, -3, 1, -1, 1, -1, 3, -1, 1, -1, 1, -3,
			-3, -1, 1, -1, -1, -3, 1, -1, -1, -1, 3, -1, -1, -1, 1, -3,
			3, 1, -1, -1, 1, 3, -1, -1, 1, 1, -3, -1, 1, 1, -1, -3,
			-3, 1, -1, -1, -1, 3, -1, -1, -1, 1, -3, -1, -1, 1, -1, -3,
			3, -1, -1, -1, 1, -3, -1, -1, 1, -1, -3, -1, 1, -1, -1, -3,
			-3, -1, -1, -1, -1, -3, -1, -1, -1, -1, -3, -1, -1, -1, -1, -3
		)
}

class OpenSimplexNoise {

}