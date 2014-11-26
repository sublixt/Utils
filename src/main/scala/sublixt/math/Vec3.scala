package sublixt.math

object Vec3 {
	val zero = Vec3(0, 0, 0)
	def apply(): Vec3 = zero
	def apply(vec: Vec4): Vec3 = vec.xyz
	def apply(vec: Vec2, z: Float): Vec3 = Vec3(vec.x, vec.y, z)
	def apply(x: Float, vec: Vec2): Vec3 = Vec3(x, vec.x, vec.y)
}

case class Vec3(val x: Float, val y: Float, val z: Float) {
	def apply(i: Int) = i match {
		case 0 => x
		case 1 => y
		case 2 => z
		case _ => throw new IndexOutOfBoundsException("[Vec3](apply(int)): " + i.toString + " not in range [0, 2]")
	}

	def cross(other: Vec3) = Vec3(y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x)
	def dot(other: Vec3) = x * other.x + y * other.y + z * other.z
	def length = sqrt(x * x + y * y + z * z)
	def normalize = this / length

	def unary_- = Vec3(-x, -y, -z)
	def *(other: Vec3) = Vec3(x * other.x, y * other.y, z * other.z)
	def /(other: Vec3) = Vec3(x / other.x, y / other.y, z / other.z)
	def +(other: Vec3) = Vec3(x + other.x, y + other.y, z + other.z)
	def -(other: Vec3) = Vec3(x - other.x, y - other.y, z - other.z)
	def *(scalar: Float) = Vec3(x * scalar, y * scalar, z * scalar)
	def /(scalar: Float) = Vec3(x / scalar, y / scalar, z / scalar)
	def +(scalar: Float) = Vec3(x + scalar, y + scalar, z + scalar)
	def -(scalar: Float) = Vec3(x - scalar, y - scalar, z - scalar)

	def <(other: Vec4) =
		x < other.x ||
			y < other.y ||
			z < other.z

	def <=(other: Vec4) =
		x <= other.x ||
			y <= other.y ||
			z <= other.z

	def >(other: Vec4) =
		x > other.x ||
			y > other.y ||
			z > other.z

	def >=(other: Vec4) =
		x >= other.x ||
			y >= other.y ||
			z >= other.z

	//even though I think this is exactly how case class would implement 
	//the equals method. I'm going to implement it for now until I'm sure
	override def equals(other: Any) =
		other match {
			case Vec3(x2, y2, z2) =>
				x == x2 &&
				y == y2 &&
				z == z2
			case _ => false
		}

	def xx = Vec2(x, x)
	def xy = Vec2(x, y)
	def xz = Vec2(x, z)
	def yx = Vec2(y, x)
	def yy = Vec2(y, y)
	def yz = Vec2(y, z)
	def zx = Vec2(z, x)
	def zy = Vec2(z, y)
	def zz = Vec2(z, z)

	def xxx = Vec3(x, x, x)
	def xxy = Vec3(x, x, y)
	def xxz = Vec3(x, x, z)
	def xyx = Vec3(x, y, x)
	def xyy = Vec3(x, y, y)
	def xyz = this
	def xzx = Vec3(x, z, x)
	def xzy = Vec3(x, z, y)
	def xzz = Vec3(x, z, z)
	def yxx = Vec3(y, x, x)
	def yxy = Vec3(y, x, y)
	def yxz = Vec3(y, x, z)
	def yyx = Vec3(y, y, x)
	def yyy = Vec3(y, y, y)
	def yyz = Vec3(y, y, z)
	def yzx = Vec3(y, z, x)
	def yzy = Vec3(y, z, y)
	def yzz = Vec3(y, z, z)
	def zxx = Vec3(z, x, x)
	def zxy = Vec3(z, x, y)
	def zxz = Vec3(z, x, z)
	def zyx = Vec3(z, y, x)
	def zyy = Vec3(z, y, y)
	def zyz = Vec3(z, y, z)
	def zzx = Vec3(z, z, x)
	def zzy = Vec3(z, z, y)
	def zzz = Vec3(z, z, z)

	def xxxx = Vec4(x, x, x, x)
	def xxxy = Vec4(x, x, x, y)
	def xxxz = Vec4(x, x, x, z)
	def xxyx = Vec4(x, x, y, x)
	def xxyy = Vec4(x, x, y, y)
	def xxyz = Vec4(x, x, y, z)
	def xxzx = Vec4(x, x, z, x)
	def xxzy = Vec4(x, x, z, y)
	def xxzz = Vec4(x, x, z, z)
	def xyxx = Vec4(x, y, x, x)
	def xyxy = Vec4(x, y, x, y)
	def xyxz = Vec4(x, y, x, z)
	def xyyx = Vec4(x, y, y, x)
	def xyyy = Vec4(x, y, y, y)
	def xyyz = Vec4(x, y, y, z)
	def xyzx = Vec4(x, y, z, x)
	def xyzy = Vec4(x, y, z, y)
	def xyzz = Vec4(x, y, z, z)
	def xzxx = Vec4(x, z, x, x)
	def xzxy = Vec4(x, z, x, y)
	def xzxz = Vec4(x, z, x, z)
	def xzyx = Vec4(x, z, y, x)
	def xzyy = Vec4(x, z, y, y)
	def xzyz = Vec4(x, z, y, z)
	def xzzx = Vec4(x, z, z, x)
	def xzzy = Vec4(x, z, z, y)
	def xzzz = Vec4(x, z, z, z)
	def yxxx = Vec4(y, x, x, x)
	def yxxy = Vec4(y, x, x, y)
	def yxxz = Vec4(y, x, x, z)
	def yxyx = Vec4(y, x, y, x)
	def yxyy = Vec4(y, x, y, y)
	def yxyz = Vec4(y, x, y, z)
	def yxzx = Vec4(y, x, z, x)
	def yxzy = Vec4(y, x, z, y)
	def yxzz = Vec4(y, x, z, z)
	def yyxx = Vec4(y, y, x, x)
	def yyxy = Vec4(y, y, x, y)
	def yyxz = Vec4(y, y, x, z)
	def yyyx = Vec4(y, y, y, x)
	def yyyy = Vec4(y, y, y, y)
	def yyyz = Vec4(y, y, y, z)
	def yyzx = Vec4(y, y, z, x)
	def yyzy = Vec4(y, y, z, y)
	def yyzz = Vec4(y, y, z, z)
	def yzxx = Vec4(y, z, x, x)
	def yzxy = Vec4(y, z, x, y)
	def yzxz = Vec4(y, z, x, z)
	def yzyx = Vec4(y, z, y, x)
	def yzyy = Vec4(y, z, y, y)
	def yzyz = Vec4(y, z, y, z)
	def yzzx = Vec4(y, z, z, x)
	def yzzy = Vec4(y, z, z, y)
	def yzzz = Vec4(y, z, z, z)
	def zxxx = Vec4(z, x, x, x)
	def zxxy = Vec4(z, x, x, y)
	def zxxz = Vec4(z, x, x, z)
	def zxyx = Vec4(z, x, y, x)
	def zxyy = Vec4(z, x, y, y)
	def zxyz = Vec4(z, x, y, z)
	def zxzx = Vec4(z, x, z, x)
	def zxzy = Vec4(z, x, z, y)
	def zxzz = Vec4(z, x, z, z)
	def zyxx = Vec4(z, y, x, x)
	def zyxy = Vec4(z, y, x, y)
	def zyxz = Vec4(z, y, x, z)
	def zyyx = Vec4(z, y, y, x)
	def zyyy = Vec4(z, y, y, y)
	def zyyz = Vec4(z, y, y, z)
	def zyzx = Vec4(z, y, z, x)
	def zyzy = Vec4(z, y, z, y)
	def zyzz = Vec4(z, y, z, z)
	def zzxx = Vec4(z, z, x, x)
	def zzxy = Vec4(z, z, x, y)
	def zzxz = Vec4(z, z, x, z)
	def zzyx = Vec4(z, z, y, x)
	def zzyy = Vec4(z, z, y, y)
	def zzyz = Vec4(z, z, y, z)
	def zzzx = Vec4(z, z, z, x)
	def zzzy = Vec4(z, z, z, y)
	def zzzz = Vec4(z, z, z, z)

	def s = x
	def t = y
	def p = z

	def ss = Vec2(x, x)
	def st = Vec2(x, y)
	def sp = Vec2(x, z)
	def ts = Vec2(y, x)
	def tt = Vec2(y, y)
	def tp = Vec2(y, z)
	def ps = Vec2(z, x)
	def pt = Vec2(z, y)
	def pp = Vec2(z, z)

	def sss = Vec3(x, x, x)
	def sst = Vec3(x, x, y)
	def ssp = Vec3(x, x, z)
	def sts = Vec3(x, y, x)
	def stt = Vec3(x, y, y)
	def stp = this
	def sps = Vec3(x, z, x)
	def spt = Vec3(x, z, y)
	def spp = Vec3(x, z, z)
	def tss = Vec3(y, x, x)
	def tst = Vec3(y, x, y)
	def tsp = Vec3(y, x, z)
	def tts = Vec3(y, y, x)
	def ttt = Vec3(y, y, y)
	def ttp = Vec3(y, y, z)
	def tps = Vec3(y, z, x)
	def tpt = Vec3(y, z, y)
	def tpp = Vec3(y, z, z)
	def pss = Vec3(z, x, x)
	def pst = Vec3(z, x, y)
	def psp = Vec3(z, x, z)
	def pts = Vec3(z, y, x)
	def ptt = Vec3(z, y, y)
	def ptp = Vec3(z, y, z)
	def pps = Vec3(z, z, x)
	def ppt = Vec3(z, z, y)
	def ppp = Vec3(z, z, z)

	def ssss = Vec4(x, x, x, x)
	def ssst = Vec4(x, x, x, y)
	def sssp = Vec4(x, x, x, z)
	def ssts = Vec4(x, x, y, x)
	def sstt = Vec4(x, x, y, y)
	def sstp = Vec4(x, x, y, z)
	def ssps = Vec4(x, x, z, x)
	def sspt = Vec4(x, x, z, y)
	def sspp = Vec4(x, x, z, z)
	def stss = Vec4(x, y, x, x)
	def stst = Vec4(x, y, x, y)
	def stsp = Vec4(x, y, x, z)
	def stts = Vec4(x, y, y, x)
	def sttt = Vec4(x, y, y, y)
	def sttp = Vec4(x, y, y, z)
	def stps = Vec4(x, y, z, x)
	def stpt = Vec4(x, y, z, y)
	def stpp = Vec4(x, y, z, z)
	def spss = Vec4(x, z, x, x)
	def spst = Vec4(x, z, x, y)
	def spsp = Vec4(x, z, x, z)
	def spts = Vec4(x, z, y, x)
	def sptt = Vec4(x, z, y, y)
	def sptp = Vec4(x, z, y, z)
	def spps = Vec4(x, z, z, x)
	def sppt = Vec4(x, z, z, y)
	def sppp = Vec4(x, z, z, z)
	def tsss = Vec4(y, x, x, x)
	def tsst = Vec4(y, x, x, y)
	def tssp = Vec4(y, x, x, z)
	def tsts = Vec4(y, x, y, x)
	def tstt = Vec4(y, x, y, y)
	def tstp = Vec4(y, x, y, z)
	def tsps = Vec4(y, x, z, x)
	def tspt = Vec4(y, x, z, y)
	def tspp = Vec4(y, x, z, z)
	def ttss = Vec4(y, y, x, x)
	def ttst = Vec4(y, y, x, y)
	def ttsp = Vec4(y, y, x, z)
	def ttts = Vec4(y, y, y, x)
	def tttt = Vec4(y, y, y, y)
	def tttp = Vec4(y, y, y, z)
	def ttps = Vec4(y, y, z, x)
	def ttpt = Vec4(y, y, z, y)
	def ttpp = Vec4(y, y, z, z)
	def tpss = Vec4(y, z, x, x)
	def tpst = Vec4(y, z, x, y)
	def tpsp = Vec4(y, z, x, z)
	def tpts = Vec4(y, z, y, x)
	def tptt = Vec4(y, z, y, y)
	def tptp = Vec4(y, z, y, z)
	def tpps = Vec4(y, z, z, x)
	def tppt = Vec4(y, z, z, y)
	def tppp = Vec4(y, z, z, z)
	def psss = Vec4(z, x, x, x)
	def psst = Vec4(z, x, x, y)
	def pssp = Vec4(z, x, x, z)
	def psts = Vec4(z, x, y, x)
	def pstt = Vec4(z, x, y, y)
	def pstp = Vec4(z, x, y, z)
	def psps = Vec4(z, x, z, x)
	def pspt = Vec4(z, x, z, y)
	def pspp = Vec4(z, x, z, z)
	def ptss = Vec4(z, y, x, x)
	def ptst = Vec4(z, y, x, y)
	def ptsp = Vec4(z, y, x, z)
	def ptts = Vec4(z, y, y, x)
	def pttt = Vec4(z, y, y, y)
	def pttp = Vec4(z, y, y, z)
	def ptps = Vec4(z, y, z, x)
	def ptpt = Vec4(z, y, z, y)
	def ptpp = Vec4(z, y, z, z)
	def ppss = Vec4(z, z, x, x)
	def ppst = Vec4(z, z, x, y)
	def ppsp = Vec4(z, z, x, z)
	def ppts = Vec4(z, z, y, x)
	def pptt = Vec4(z, z, y, y)
	def pptp = Vec4(z, z, y, z)
	def ppps = Vec4(z, z, z, x)
	def pppt = Vec4(z, z, z, y)
	def pppp = Vec4(z, z, z, z)

	def r = x
	def g = y
	def b = z

	def rr = Vec2(x, x)
	def rg = Vec2(x, y)
	def rb = Vec2(x, z)
	def gr = Vec2(y, x)
	def gg = Vec2(y, y)
	def gb = Vec2(y, z)
	def br = Vec2(z, x)
	def bg = Vec2(z, y)
	def bb = Vec2(z, z)

	def rrr = Vec3(x, x, x)
	def rrg = Vec3(x, x, y)
	def rrb = Vec3(x, x, z)
	def rgr = Vec3(x, y, x)
	def rgg = Vec3(x, y, y)
	def rgb = this
	def rbr = Vec3(x, z, x)
	def rbg = Vec3(x, z, y)
	def rbb = Vec3(x, z, z)
	def grr = Vec3(y, x, x)
	def grg = Vec3(y, x, y)
	def grb = Vec3(y, x, z)
	def ggr = Vec3(y, y, x)
	def ggg = Vec3(y, y, y)
	def ggb = Vec3(y, y, z)
	def gbr = Vec3(y, z, x)
	def gbg = Vec3(y, z, y)
	def gbb = Vec3(y, z, z)
	def brr = Vec3(z, x, x)
	def brg = Vec3(z, x, y)
	def brb = Vec3(z, x, z)
	def bgr = Vec3(z, y, x)
	def bgg = Vec3(z, y, y)
	def bgb = Vec3(z, y, z)
	def bbr = Vec3(z, z, x)
	def bbg = Vec3(z, z, y)
	def bbb = Vec3(z, z, z)

	def rrrr = Vec4(x, x, x, x)
	def rrrg = Vec4(x, x, x, y)
	def rrrb = Vec4(x, x, x, z)
	def rrgr = Vec4(x, x, y, x)
	def rrgg = Vec4(x, x, y, y)
	def rrgb = Vec4(x, x, y, z)
	def rrbr = Vec4(x, x, z, x)
	def rrbg = Vec4(x, x, z, y)
	def rrbb = Vec4(x, x, z, z)
	def rgrr = Vec4(x, y, x, x)
	def rgrg = Vec4(x, y, x, y)
	def rgrb = Vec4(x, y, x, z)
	def rggr = Vec4(x, y, y, x)
	def rggg = Vec4(x, y, y, y)
	def rggb = Vec4(x, y, y, z)
	def rgbr = Vec4(x, y, z, x)
	def rgbg = Vec4(x, y, z, y)
	def rgbb = Vec4(x, y, z, z)
	def rbrr = Vec4(x, z, x, x)
	def rbrg = Vec4(x, z, x, y)
	def rbrb = Vec4(x, z, x, z)
	def rbgr = Vec4(x, z, y, x)
	def rbgg = Vec4(x, z, y, y)
	def rbgb = Vec4(x, z, y, z)
	def rbbr = Vec4(x, z, z, x)
	def rbbg = Vec4(x, z, z, y)
	def rbbb = Vec4(x, z, z, z)
	def grrr = Vec4(y, x, x, x)
	def grrg = Vec4(y, x, x, y)
	def grrb = Vec4(y, x, x, z)
	def grgr = Vec4(y, x, y, x)
	def grgg = Vec4(y, x, y, y)
	def grgb = Vec4(y, x, y, z)
	def grbr = Vec4(y, x, z, x)
	def grbg = Vec4(y, x, z, y)
	def grbb = Vec4(y, x, z, z)
	def ggrr = Vec4(y, y, x, x)
	def ggrg = Vec4(y, y, x, y)
	def ggrb = Vec4(y, y, x, z)
	def gggr = Vec4(y, y, y, x)
	def gggg = Vec4(y, y, y, y)
	def gggb = Vec4(y, y, y, z)
	def ggbr = Vec4(y, y, z, x)
	def ggbg = Vec4(y, y, z, y)
	def ggbb = Vec4(y, y, z, z)
	def gbrr = Vec4(y, z, x, x)
	def gbrg = Vec4(y, z, x, y)
	def gbrb = Vec4(y, z, x, z)
	def gbgr = Vec4(y, z, y, x)
	def gbgg = Vec4(y, z, y, y)
	def gbgb = Vec4(y, z, y, z)
	def gbbr = Vec4(y, z, z, x)
	def gbbg = Vec4(y, z, z, y)
	def gbbb = Vec4(y, z, z, z)
	def brrr = Vec4(z, x, x, x)
	def brrg = Vec4(z, x, x, y)
	def brrb = Vec4(z, x, x, z)
	def brgr = Vec4(z, x, y, x)
	def brgg = Vec4(z, x, y, y)
	def brgb = Vec4(z, x, y, z)
	def brbr = Vec4(z, x, z, x)
	def brbg = Vec4(z, x, z, y)
	def brbb = Vec4(z, x, z, z)
	def bgrr = Vec4(z, y, x, x)
	def bgrg = Vec4(z, y, x, y)
	def bgrb = Vec4(z, y, x, z)
	def bggr = Vec4(z, y, y, x)
	def bggg = Vec4(z, y, y, y)
	def bggb = Vec4(z, y, y, z)
	def bgbr = Vec4(z, y, z, x)
	def bgbg = Vec4(z, y, z, y)
	def bgbb = Vec4(z, y, z, z)
	def bbrr = Vec4(z, z, x, x)
	def bbrg = Vec4(z, z, x, y)
	def bbrb = Vec4(z, z, x, z)
	def bbgr = Vec4(z, z, y, x)
	def bbgg = Vec4(z, z, y, y)
	def bbgb = Vec4(z, z, y, z)
	def bbbr = Vec4(z, z, z, x)
	def bbbg = Vec4(z, z, z, y)
	def bbbb = Vec4(z, z, z, z)
}