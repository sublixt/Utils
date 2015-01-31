// Copyright 2015 The noise-rs developers. For a full listing of the authors,
// refer to the AUTHORS file at the top-level directory of https://github.com/bjz/noise-rs.
//
// Copyright 2015 Mark Reuter
//
// Licensed under the Apache License, Version 2.0 (the "License")
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
import Gradient._

class PerlinNoise(val seed: Seed) {
	def perlin(x: Double, y: Double) = {
			@inline
			def gradient(wx: Int, wy: Int, fx: Double, fy: Double) = {
				val attn = 1.0 - (fx * fx + fy * fy)
				if (attn > 0.0) {
					val (gx, gy) = get2(seed.get2(wx, wy))
					(attn * attn) * (fx * gx + fy * gy)
				} else
					0.0
			}

		val fx = floor(x)
		val fy = floor(y)
		val w0x = fx.toInt
		val w0y = fy.toInt
		val w1x = w0x + 1
		val w1y = w0y + 1
		val f0x = x - fx
		val f0y = y - fy
		val f1x = f0x - 1
		val f1y = f0y - 1

		val f00 = gradient(w0x, w0y, f0x, f0y)
		val f10 = gradient(w1x, w0y, f1x, f0y)
		val f01 = gradient(w0x, w1y, f0x, f1y)
		val f11 = gradient(w1x, w1y, f1x, f1y)

		(f00 + f10 + f01 + f11 + 0.053179) * 1.056165
	}

	def perlin(x: Double, y: Double, z: Double) = {
			@inline
			def gradient(wx: Int, wy: Int, wz: Int, fx: Double, fy: Double, fz: Double) = {
				val attn = 1.0 - (fx * fx + fy * fy + fz * fz)
				if (attn > 0.0) {
					val (gx, gy, gz) = get3(seed.get3(wx, wy, wz))
					(attn * attn) * (fx * gx + fy * gy + fz * gz)
				} else
					0.0
			}

		val fx = floor(x)
		val fy = floor(y)
		val fz = floor(z)
		val w0x = fx.toInt
		val w0y = fy.toInt
		val w0z = fz.toInt
		val w1x = w0x + 1
		val w1y = w0y + 1
		val w1z = w0z + 1
		val f0x = x - fx
		val f0y = y - fy
		val f0z = z - fz
		val f1x = f0x - 1
		val f1y = f0y - 1
		val f1z = f0z - 1

		val f000 = gradient(w0x, w0y, w0z, f0x, f0y, f0z)
		val f100 = gradient(w1x, w0y, w0z, f1x, f0y, f0z)
		val f010 = gradient(w0x, w1y, w0z, f0x, f1y, f0z)
		val f110 = gradient(w1x, w1y, w0z, f1x, f1y, f0z)
		val f001 = gradient(w0x, w0y, w1z, f0x, f0y, f1z)
		val f101 = gradient(w1x, w0y, w1z, f1x, f0y, f1z)
		val f011 = gradient(w0x, w1y, w1z, f0x, f1y, f1z)
		val f111 = gradient(w1x, w1y, w1z, f1x, f1y, f1z)

		(f000 + f100 + f010 + f110 +
			f001 + f101 + f011 + f111 + 0.053179) * 1.056165
	}

	def perlin(x: Double, y: Double, z: Double, w: Double) = {
			@inline
			def gradient(wx: Int, wy: Int, wz: Int, ww: Int, fx: Double, fy: Double, fz: Double, fw: Double) = {
				val attn = 1.0 - (fx * fx + fy * fy + fz * fz + fw * fw)
				if (attn > 0.0) {
					val (gx, gy, gz, gw) = get4(seed.get4(wx, wy, wz, ww))
					(attn * attn) * (fx * gx + fy * gy + fz * gz + fw * gw)
				} else
					0.0
			}

		val fx = floor(x)
		val fy = floor(y)
		val fz = floor(z)
		val fw = floor(w)
		val w0x = fx.toInt
		val w0y = fy.toInt
		val w0z = fz.toInt
		val w0w = fw.toInt
		val w1x = w0x + 1
		val w1y = w0y + 1
		val w1z = w0z + 1
		val w1w = w0w + 1
		val f0x = x - fx
		val f0y = y - fy
		val f0z = z - fz
		val f0w = w - fw
		val f1x = f0x - 1
		val f1y = f0y - 1
		val f1z = f0z - 1
		val f1w = f0w - 1

		val f0000 = gradient(w0x, w0y, w0z, w1w, f0x, f0y, f0z, f0w)
		val f1000 = gradient(w1x, w0y, w0z, w1w, f1x, f0y, f0z, f0w)
		val f0100 = gradient(w0x, w1y, w0z, w1w, f0x, f1y, f0z, f0w)
		val f1100 = gradient(w1x, w1y, w0z, w1w, f1x, f1y, f0z, f0w)
		val f0010 = gradient(w0x, w0y, w1z, w1w, f0x, f0y, f1z, f0w)
		val f1010 = gradient(w1x, w0y, w1z, w1w, f1x, f0y, f1z, f0w)
		val f0110 = gradient(w0x, w1y, w1z, w1w, f0x, f1y, f1z, f0w)
		val f1110 = gradient(w1x, w1y, w1z, w1w, f1x, f1y, f1z, f0w)
		val f0001 = gradient(w0x, w0y, w0z, w1w, f0x, f0y, f0z, f1w)
		val f1001 = gradient(w1x, w0y, w0z, w1w, f1x, f0y, f0z, f1w)
		val f0101 = gradient(w0x, w1y, w0z, w1w, f0x, f1y, f0z, f1w)
		val f1101 = gradient(w1x, w1y, w0z, w1w, f1x, f1y, f0z, f1w)
		val f0011 = gradient(w0x, w0y, w1z, w1w, f0x, f0y, f1z, f1w)
		val f1011 = gradient(w1x, w0y, w1z, w1w, f1x, f0y, f1z, f1w)
		val f0111 = gradient(w0x, w1y, w1z, w1w, f0x, f1y, f1z, f1w)
		val f1111 = gradient(w1x, w1y, w1z, w1w, f1x, f1y, f1z, f1w)

		(f0000 + f1000 + f0100 + f1100 +
			f0010 + f1010 + f0110 + f1110 +
			f0001 + f1001 + f0101 + f1101 +
			f0011 + f1011 + f0111 + f1111) * 1.119016
	}
}
