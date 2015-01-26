// Copyright 2015 The noise-rs developers. For a full listing of the authors,
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

//I ported this code using the original source found at https://github.com/bjz/noise-rs
package sublixt.math.noise

import sublixt.math._
import java.util.Random

object Seed {
	val TABLE_SIZE = 0xFF
	val defaultPRNG = (x: Long) => xorShift(x)
	private val baseTable = (0 until Seed.TABLE_SIZE).toArray.map(_.toShort)
}

import Seed._
class Seed private (private val values: Array[Short]) {
	def this(seed: Long, prng: Long => Long) {
		this(new Array[Short](TABLE_SIZE))
		Array.copy(baseTable, 0, values, 0, TABLE_SIZE)

		var rng = prng(seed)

		var i = 0
		while (i < TABLE_SIZE) {
			val swap_i = (rng & TABLE_SIZE).toInt
			val swap = values(swap_i)
			values(swap_i) = values(i)
			values(i) = swap

			rng = prng(seed)
			i += 1
		}
	}

	def this(seed: Long) {
		this(seed, defaultPRNG)
	}

	def this(rng: Random) {
		this(new Array[Short](TABLE_SIZE))
		Array.copy(baseTable, 0, values, 0, TABLE_SIZE)

		var i = 0
		while (i < TABLE_SIZE) {
			val swap_i = (rng.nextLong() & TABLE_SIZE).toInt
			val swap = values(swap_i)
			values(swap_i) = values(i)
			values(i) = swap

			i += 1
		}
	}

	@inline def get1(x: Int) = values(x & TABLE_SIZE)
	@inline def get2(x: Int, y: Int) = values(get1(x) ^ (y & TABLE_SIZE))
	@inline def get3(x: Int, y: Int, z: Int) = values(get2(x, y) ^ (z & TABLE_SIZE))
	@inline def get4(x: Int, y: Int, z: Int, w: Int) = values(get3(x, y, z) ^ (w & TABLE_SIZE))
}
