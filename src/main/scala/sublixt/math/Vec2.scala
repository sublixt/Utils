package sublixt.math

object Vec2 {
	val zero = Vec2(0, 0)
	def apply(): Vec2 = zero
	def apply(vec: Vec4): Vec2 = vec.xy
	def apply(vec: Vec3): Vec2 = vec.xy
}

case class Vec2(val x: Float, val y: Float) {
	def apply(i: Int) = i match {
		case 0 => x
		case 1 => y
		case _ => throw new IndexOutOfBoundsException("[Vec2](apply(int)): " + i.toString + " not in range [0, 1]")
	}

	def dot(other: Vec2) = x * other.x + y * other.y
	def length = sqrt(x * x + y * y)
	def normalize = this / length

	def unary_- = Vec2(-x, -y)
	def *(other: Vec2) = Vec2(x * other.x, y * other.y)
	def /(other: Vec2) = Vec2(x / other.x, y * other.y)
	def +(other: Vec2) = Vec2(x + other.x, y + other.y)
	def -(other: Vec2) = Vec2(x - other.x, y - other.y)
	def *(scalar: Float) = Vec2(x * scalar, y * scalar)
	def /(scalar: Float) = Vec2(x / scalar, y / scalar)
	def +(scalar: Float) = Vec2(x + scalar, y + scalar)
	def -(scalar: Float) = Vec2(x - scalar, y - scalar)

	def <(other: Vec4) =
		x < other.x ||
			y < other.y

	def <=(other: Vec4) =
		x <= other.x ||
			y <= other.y

	def >(other: Vec4) =
		x > other.x ||
			y > other.y

	def >=(other: Vec4) =
		x >= other.x ||
			y >= other.y

	//even though I think this is exactly how case class would implement 
	//the equals method. I'm going to implement it for now until I'm sure
	override def equals(other: Any) =
		other match {
			case Vec2(x2, y2) =>
				x == x2 &&
					y == y2
			case _ => false
		}

	def xx = Vec2(x, x)
	def xy = this
	def yx = Vec2(y, x)
	def yy = Vec2(y, y)

	def xxx = Vec3(x, x, x)
	def xxy = Vec3(x, x, y)
	def xyx = Vec3(x, y, x)
	def xyy = Vec3(x, y, y)
	def yxx = Vec3(y, x, x)
	def yxy = Vec3(y, x, y)
	def yyx = Vec3(y, y, x)
	def yyy = Vec3(y, y, y)

	def xxxx = Vec4(x, x, x, x)
	def xxxy = Vec4(x, x, x, y)
	def xxyx = Vec4(x, x, y, x)
	def xxyy = Vec4(x, x, y, y)
	def xyxx = Vec4(x, y, x, x)
	def xyxy = Vec4(x, y, x, y)
	def xyyx = Vec4(x, y, y, x)
	def xyyy = Vec4(x, y, y, y)
	def yxxx = Vec4(y, x, x, x)
	def yxxy = Vec4(y, x, x, y)
	def yxyx = Vec4(y, x, y, x)
	def yxyy = Vec4(y, x, y, y)
	def yyxx = Vec4(y, y, x, x)
	def yyxy = Vec4(y, y, x, y)
	def yyyx = Vec4(y, y, y, x)
	def yyyy = Vec4(y, y, y, y)

	def s = x
	def t = y

	def ss = Vec2(x, x)
	def st = this
	def ts = Vec2(y, x)
	def tt = Vec2(y, y)

	def sss = Vec3(x, x, x)
	def sst = Vec3(x, x, y)
	def sts = Vec3(x, y, x)
	def stt = Vec3(x, y, y)
	def tss = Vec3(y, x, x)
	def tst = Vec3(y, x, y)
	def tts = Vec3(y, y, x)
	def ttt = Vec3(y, y, y)

	def ssss = Vec4(x, x, x, x)
	def ssst = Vec4(x, x, x, y)
	def ssts = Vec4(x, x, y, x)
	def sstt = Vec4(x, x, y, y)
	def stss = Vec4(x, y, x, x)
	def stst = Vec4(x, y, x, y)
	def stts = Vec4(x, y, y, x)
	def sttt = Vec4(x, y, y, y)
	def tsss = Vec4(y, x, x, x)
	def tsst = Vec4(y, x, x, y)
	def tsts = Vec4(y, x, y, x)
	def tstt = Vec4(y, x, y, y)
	def ttss = Vec4(y, y, x, x)
	def ttst = Vec4(y, y, x, y)
	def ttts = Vec4(y, y, y, x)
	def tttt = Vec4(y, y, y, y)

	def r = x
	def g = y

	def rr = Vec2(x, x)
	def rg = this
	def gr = Vec2(y, x)
	def gg = Vec2(y, y)

	def rrr = Vec3(x, x, x)
	def rrg = Vec3(x, x, y)
	def rgr = Vec3(x, y, x)
	def rgg = Vec3(x, y, y)
	def grr = Vec3(y, x, x)
	def grg = Vec3(y, x, y)
	def ggr = Vec3(y, y, x)
	def ggg = Vec3(y, y, y)

	def rrrr = Vec4(x, x, x, x)
	def rrrg = Vec4(x, x, x, y)
	def rrgr = Vec4(x, x, y, x)
	def rrgg = Vec4(x, x, y, y)
	def rgrr = Vec4(x, y, x, x)
	def rgrg = Vec4(x, y, x, y)
	def rggr = Vec4(x, y, y, x)
	def rggg = Vec4(x, y, y, y)
	def grrr = Vec4(y, x, x, x)
	def grrg = Vec4(y, x, x, y)
	def grgr = Vec4(y, x, y, x)
	def grgg = Vec4(y, x, y, y)
	def ggrr = Vec4(y, y, x, x)
	def ggrg = Vec4(y, y, x, y)
	def gggr = Vec4(y, y, y, x)
	def gggg = Vec4(y, y, y, y)
}