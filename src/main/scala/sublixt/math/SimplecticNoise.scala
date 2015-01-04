// Copyright 2013 The noise-rs developers. For a full listing of the authors,
// refer to the AUTHORS file at the top-level directory of https://github.com/bjz/noise-rs.
//
// Copyright 2015 Mark Reuter
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/* I ported this code using the original source found at https://github.com/bjz/noise-rs.
 * 
 * ChangeLog 
 * v1.0: 
 * 		* Initial port
 * v1.1:
 * 		* removed all Point* classes. Using tuples their place.
 * 		* cleaned out a few redundant methods in favor of 
 * 		  sublixt.math functions.
 * 		* added more control for the seeding prng.
 * */

package sublixt.math

import scala.util.Random

case class SimplecticPoint2(val x_cell: Long, val y_cell: Long, val x_offset: Double, val y_offset: Double) {
	def toSimplecticPoint3(z_cell: Long, z_offset: Double) = SimplecticPoint3(x_cell, y_cell, z_cell, x_offset, y_offset, z_offset)
}
case class SimplecticPoint3(val x_cell: Long, val y_cell: Long, val z_cell: Long, val x_offset: Double, val y_offset: Double, val z_offset: Double) {
	def toSimplecticPoint4(w_cell: Long, w_offset: Double) = SimplecticPoint4(x_cell, y_cell, z_cell, w_cell, x_offset, y_offset, z_offset, w_offset)
}
case class SimplecticPoint4(val x_cell: Long, val y_cell: Long, val z_cell: Long, val w_cell: Long, val x_offset: Double, val y_offset: Double, val z_offset: Double, val w_offset: Double)

object SimplecticNoise {
	def signedMod(a: Long, b: Long) =
		(if (a < 0) b - (abs(a) % b) else a % b).toInt

	val defaultPRNG = (x: Long) => xorShift(x)

	private val SKEW_CONSTANT = 0.36602540378 // 0.5*(sqrt(3.0)-1.0)
	private val UNSKEW_CONSTANT = 0.2113248654 // (3.0-sqrt(3.0))/6.0

	private val SIMPLEX_SIZE = 0.70710678119
	private val INV_SIMPLEX_SIZE = 1.41421356235 // 1 / SIMPLEX_SIZE
	private val LAYER_OFFSET_X = 0.45534180126 // (2.0-3.0*UNSKEW_CONSTANT)/3.0
	private val LAYER_OFFSET_Y = 0.12200846793 // (1.0-3.0*UNSKEW_CONSTANT)/3.0
	private val LAYER_OFFSET_Z = 0.35355339059 // (1.0-3.0*UNSKEW_CONSTANT)/3.0

	private val NORM_CONSTANT_2D = 8.0
	private val NORM_CONSTANT_3D = 9.0
	private val NORM_CONSTANT_4D = 10.0

	private val TABLE_SIZE = 256

	private val DIAG2 = 0.70710678118
	private val NDIAG2 = -0.70710678118

	@inline
	private def gradient2(index: Int) =
		(index % 8) match {
			case 0 => (DIAG2, DIAG2)
			case 1 => (DIAG2, NDIAG2)
			case 2 => (NDIAG2, DIAG2)
			case 3 => (NDIAG2, NDIAG2)
			case 4 => (1.0, 0.0)
			case 5 => (-1.0, 0.0)
			case 6 => (0.0, 1.0)
			case 7 => (0.0, -1.0)
			case _ => (0.0, 0.0) //this should never happen
		}

	private val DIAG3 = 0.70710678118
	private val NDIAG3 = -0.70710678118

	@inline
	private def gradient3(index: Int) =
		(index % 12) match {
			case 0 => (DIAG3, DIAG3, 0.0)
			case 1 => (DIAG3, NDIAG3, 0.0)
			case 2 => (NDIAG3, DIAG3, 0.0)
			case 3 => (NDIAG3, NDIAG3, 0.0)
			case 4 => (DIAG3, 0.0, DIAG3)
			case 5 => (DIAG3, 0.0, NDIAG3)
			case 6 => (NDIAG3, 0.0, DIAG3)
			case 7 => (NDIAG3, 0.0, NDIAG3)
			case 8 => (0.0, DIAG3, DIAG3)
			case 9 => (0.0, DIAG3, NDIAG3)
			case 10 => (0.0, NDIAG3, DIAG3)
			case 11 => (0.0, NDIAG3, NDIAG3)
			case _ => (0.0, 0.0, 0.0) //this should never happen
		}

	private val DIAG4 = 0.57735026919
	private val NDIAG4 = -0.57735026919

	@inline
	private def gradient4(index: Int) =
		(index % 32) match {
			case 0 => (DIAG4, DIAG4, DIAG4, 0.0)
			case 1 => (DIAG4, NDIAG4, DIAG4, 0.0)
			case 2 => (NDIAG4, DIAG4, DIAG4, 0.0)
			case 3 => (NDIAG4, NDIAG4, DIAG4, 0.0)
			case 4 => (DIAG4, DIAG4, NDIAG4, 0.0)
			case 5 => (DIAG4, NDIAG4, NDIAG4, 0.0)
			case 6 => (NDIAG4, DIAG4, NDIAG4, 0.0)
			case 7 => (NDIAG4, NDIAG4, NDIAG4, 0.0)
			case 8 => (DIAG4, DIAG4, 0.0, DIAG4)
			case 9 => (DIAG4, NDIAG4, 0.0, DIAG4)
			case 10 => (NDIAG4, DIAG4, 0.0, DIAG4)
			case 11 => (NDIAG4, NDIAG4, 0.0, DIAG4)
			case 12 => (DIAG4, DIAG4, 0.0, NDIAG4)
			case 13 => (DIAG4, NDIAG4, 0.0, NDIAG4)
			case 14 => (NDIAG4, DIAG4, 0.0, NDIAG4)
			case 15 => (NDIAG4, NDIAG4, 0.0, NDIAG4)
			case 16 => (DIAG4, 0.0, DIAG4, DIAG4)
			case 17 => (DIAG4, 0.0, NDIAG4, DIAG4)
			case 18 => (NDIAG4, 0.0, DIAG4, DIAG4)
			case 19 => (NDIAG4, 0.0, NDIAG4, DIAG4)
			case 20 => (DIAG4, 0.0, DIAG4, NDIAG4)
			case 21 => (DIAG4, 0.0, NDIAG4, NDIAG4)
			case 22 => (NDIAG4, 0.0, DIAG4, NDIAG4)
			case 23 => (NDIAG4, 0.0, NDIAG4, NDIAG4)
			case 24 => (0.0, DIAG4, DIAG4, DIAG4)
			case 25 => (0.0, DIAG4, NDIAG4, DIAG4)
			case 26 => (0.0, NDIAG4, DIAG4, DIAG4)
			case 27 => (0.0, NDIAG4, NDIAG4, DIAG4)
			case 28 => (0.0, DIAG4, DIAG4, NDIAG4)
			case 29 => (0.0, DIAG4, NDIAG4, NDIAG4)
			case 30 => (0.0, NDIAG4, DIAG4, NDIAG4)
			case 31 => (0.0, NDIAG4, NDIAG4, NDIAG4)
			case _ => (0.0, 0.0, 0.0, 0.0) //this should never happen	
		}

	@inline
	private def simplectic2Points(x: Double, y: Double) = {
		// Skew the input coordinates into the grid to figure out which grid cell we're in
		val skew_offset = (x + y) * SKEW_CONSTANT
		val x_cell = floor(x + skew_offset)
		val y_cell = floor(y + skew_offset)

		// Unskew the floored coordinates to find the real coordinates of the cell's origin
		val unskew_offset = (x_cell + y_cell) * UNSKEW_CONSTANT
		val x_origin = x_cell - unskew_offset
		val y_origin = y_cell - unskew_offset

		// Compute the delta from the first point, which is the cell origin
		val dx0 = x - x_origin
		val dy0 = y - y_origin

		// Compute the delta from the second point, which depends on which simplex we're in
		val (x1_offset, y1_offset) = if (dx0 > dy0) (1.0, 0.0) else (0.0, 1.0)
		val dx1 = dx0 - x1_offset + UNSKEW_CONSTANT
		val dy1 = dy0 - y1_offset + UNSKEW_CONSTANT

		// Compute the delta from the third point
		val dx2 = dx0 - 1.0 + 2.0 * UNSKEW_CONSTANT
		val dy2 = dy0 - 1.0 + 2.0 * UNSKEW_CONSTANT

		(
			SimplecticPoint2(x_cell.toLong, y_cell.toLong, dx0, dy0),
			SimplecticPoint2((x_cell + x1_offset).toLong, (y_cell + y1_offset).toLong, dx1, dy1),
			SimplecticPoint2(x_cell.toLong + 1, y_cell.toLong + 1, dx2, dy2)
		)
	}

	@inline
	private def simplectic3Points(x: Double, y: Double, z: Double) = {
		val layer = floor(z * INV_SIMPLEX_SIZE)
		val layer_int = layer.toLong

		val ((layer1_x, layer1_y), (layer2_x, layer2_y)) =
			if (layer_int % 2 == 0) {
				((x, y), (x + LAYER_OFFSET_X, y + LAYER_OFFSET_Y))
			} else {
				((x + LAYER_OFFSET_X, y + LAYER_OFFSET_Y), (x, y))
			}

		val (p1, p2, p3) = simplectic2Points(layer1_x, layer1_y)
		val (p4, p5, p6) = simplectic2Points(layer2_x, layer2_y)
		val z_offset = z - layer * SIMPLEX_SIZE

		(
			p1.toSimplecticPoint3(layer_int, z_offset),
			p2.toSimplecticPoint3(layer_int, z_offset),
			p3.toSimplecticPoint3(layer_int, z_offset),
			p4.toSimplecticPoint3(layer_int + 1, z_offset - SIMPLEX_SIZE),
			p5.toSimplecticPoint3(layer_int + 1, z_offset - SIMPLEX_SIZE),
			p6.toSimplecticPoint3(layer_int + 1, z_offset - SIMPLEX_SIZE)
		)
	}

	@inline
	private def simplectic4Points(x: Double, y: Double, z: Double, w: Double) = {
		val layer = floor(w * INV_SIMPLEX_SIZE)
		val layer_int = layer.toLong

		val ((layer1_x, layer1_y, layer1_z), (layer2_x, layer2_y, layer2_z)) =
			if (layer_int % 2 == 0)
				((x, y, z), (x + LAYER_OFFSET_X, y + LAYER_OFFSET_Y, z + LAYER_OFFSET_Z))
			else
				((x + LAYER_OFFSET_X, y + LAYER_OFFSET_Y, z + LAYER_OFFSET_Z), (x, y, z))

		val (p1, p2, p3, p4, p5, p6) = simplectic3Points(layer1_x, layer1_y, layer1_z)
		val (p7, p8, p9, p10, p11, p12) = simplectic3Points(layer2_x, layer2_y, layer2_z)

		val w_offset = w - layer * SIMPLEX_SIZE
		(
			p1.toSimplecticPoint4(layer_int, w_offset),
			p2.toSimplecticPoint4(layer_int, w_offset),
			p3.toSimplecticPoint4(layer_int, w_offset),
			p4.toSimplecticPoint4(layer_int, w_offset),
			p5.toSimplecticPoint4(layer_int, w_offset),
			p6.toSimplecticPoint4(layer_int, w_offset),
			p7.toSimplecticPoint4(layer_int + 1, w_offset - SIMPLEX_SIZE),
			p8.toSimplecticPoint4(layer_int + 1, w_offset - SIMPLEX_SIZE),
			p9.toSimplecticPoint4(layer_int + 1, w_offset - SIMPLEX_SIZE),
			p10.toSimplecticPoint4(layer_int + 1, w_offset - SIMPLEX_SIZE),
			p11.toSimplecticPoint4(layer_int + 1, w_offset - SIMPLEX_SIZE),
			p12.toSimplecticPoint4(layer_int + 1, w_offset - SIMPLEX_SIZE)
		)
	}
}

class SimplecticNoise private (private val values: Array[Int]) {
	import SimplecticNoise._

	def this(prng: Long => Long, seed: Long) {
		this((0 until SimplecticNoise.TABLE_SIZE * 2).toArray)

		var rng = prng(seed)
		val range = 0 until TABLE_SIZE
		for (i <- range) {
			val swap_i = (abs(rng) % TABLE_SIZE).toInt
			val swap = values(swap_i)
			values(swap_i) = values(i)
			values(i) = swap
			rng = prng(rng)
		}

		for (i <- range) {
			values(i + TABLE_SIZE) = values(i)
		}
	}

	def this(rng: Random) {
		this((0 until SimplecticNoise.TABLE_SIZE * 2).toArray)

		val range = 0 until TABLE_SIZE
		for (i <- range) {
			val swap_i = (rng.nextLong % TABLE_SIZE).toInt
			val swap = values(swap_i)
			values(swap_i) = values(i)
			values(i) = swap
		}

		for (i <- range) {
			values(i + TABLE_SIZE) = values(i)
		}
	}

	def this(seed: Long) {
		this(SimplecticNoise.defaultPRNG, seed)
	}

	@inline private def get1(x: Long) = values(signedMod(x, TABLE_SIZE))
	@inline private def get2(x: Long, y: Long) = values(signedMod(y, TABLE_SIZE) + get1(x))
	@inline private def get3(x: Long, y: Long, z: Long) = values(signedMod(z, TABLE_SIZE) + get2(x, y))
	@inline private def get4(x: Long, y: Long, z: Long, w: Long) = values(signedMod(w, TABLE_SIZE) + get3(x, y, z))

	def apply(x: Double, y: Double): Double = {
			@inline
			def gradient(p: SimplecticPoint2) = {
				val attn = SIMPLEX_SIZE - p.x_offset * p.x_offset - p.y_offset * p.y_offset
				if (attn > 0.0) {
					val (vx, vy) = gradient2(get2(p.x_cell, p.y_cell))
					val attn2 = attn * attn
					attn2 * attn2 * (p.x_offset * vx + p.y_offset * vy)
				} else 0.0
			}

		val (p1, p2, p3) = simplectic2Points(x, y)
		(gradient(p1) + gradient(p2) + gradient(p3)) * NORM_CONSTANT_2D
	}

	def apply(x: Double, y: Double, z: Double): Double = {
			@inline
			def gradient(p: SimplecticPoint3) = {
				val attn = SIMPLEX_SIZE - p.x_offset * p.x_offset - p.y_offset * p.y_offset - p.z_offset * p.z_offset
				if (attn > 0.0) {
					val (vx, vy, vz) = gradient3(get3(p.x_cell, p.y_cell, p.z_cell))
					val attn2 = attn * attn
					attn2 * attn2 * (p.x_offset * vx + p.y_offset * vy + p.z_offset * vz)
				} else 0.0
			}

		val (p1, p2, p3, p4, p5, p6) = simplectic3Points(x, y, z)
		(gradient(p1) + gradient(p2) + gradient(p3) + gradient(p4) + gradient(p5) + gradient(p6)) * NORM_CONSTANT_3D
	}

	def apply(x: Double, y: Double, z: Double, w: Double): Double = {
			@inline
			def gradient(p: SimplecticPoint4) = {
				val attn = SIMPLEX_SIZE - p.x_offset * p.x_offset - p.y_offset * p.y_offset - p.z_offset * p.z_offset - p.w_offset * p.w_offset
				if (attn > 0.0) {
					val (vx, vy, vz, vw) = gradient4(get4(p.x_cell, p.y_cell, p.z_cell, p.w_cell))
					val attn2 = attn * attn
					attn2 * attn2 * (p.x_offset * vx + p.y_offset * vy + p.z_offset * vz + p.w_offset * vw)
				} else 0.0
			}

		val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = simplectic4Points(x, y, z, w)

		(gradient(p1) + gradient(p2) + gradient(p3) + gradient(p4) + gradient(p5) + gradient(p6) +
			gradient(p7) + gradient(p8) + gradient(p9) + gradient(p10) + gradient(p11) + gradient(p12)) * NORM_CONSTANT_4D
	}
}