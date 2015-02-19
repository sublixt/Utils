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

object Gradient {
	private val gradient =
		Array(
			(0.0, 1.0, 1.0, 1.0),
			(0.0, 1.0, 1.0, -1.0),
			(0.0, 1.0, -1.0, 1.0),
			(0.0, 1.0, -1.0, -1.0),
			(0.0, -1.0, 1.0, 1.0),
			(0.0, -1.0, 1.0, -1.0),
			(0.0, -1.0, -1.0, 1.0),
			(0.0, -1.0, -1.0, -1.0),
			(1.0, 0.0, 1.0, 1.0),
			(1.0, 0.0, 1.0, -1.0),
			(1.0, 0.0, -1.0, 1.0),
			(1.0, 0.0, -1.0, -1.0),
			(-1.0, 0.0, 1.0, 1.0),
			(-1.0, 0.0, 1.0, -1.0),
			(-1.0, 0.0, -1.0, 1.0),
			(-1.0, 0.0, -1.0, -1.0),
			(1.0, 1.0, 0.0, 1.0),
			(1.0, 1.0, 0.0, -1.0),
			(1.0, -1.0, 0.0, 1.0),
			(1.0, -1.0, 0.0, -1.0),
			(-1.0, 1.0, 0.0, 1.0),
			(-1.0, 1.0, 0.0, -1.0),
			(-1.0, -1.0, 0.0, 1.0),
			(-1.0, -1.0, 0.0, -1.0),
			(1.0, 1.0, 1.0, 0.0),
			(1.0, 1.0, -1.0, 0.0),
			(1.0, -1.0, 1.0, 0.0),
			(1.0, -1.0, -1.0, 0.0),
			(-1.0, 1.0, 1.0, 0.0),
			(-1.0, 1.0, -1.0, 0.0),
			(-1.0, -1.0, 1.0, 0.0),
			(-1.0, -1.0, -1.0, 0.0)
		)

	@inline def get2(index: Byte) = {
		val value = gradient(index & 0x1F)
		(value._1, value._2)
	}

	@inline def get3(index: Byte) = {
		val value = gradient(index & 0x1F)
		(value._1, value._2, value._3)
	}

	@inline def get4(index: Byte) =
		gradient(index & 0x1F)
}
