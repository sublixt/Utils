package sublixt.opengl

trait GLGen[A] {
	def gen: A
}

trait GLDelete[A] {
	def delete(obj: A)
}

trait GLBind[A] {
	def bind(obj: A): GLBind[A]
	def unbind()

	final def foreach(f: Unit => Unit) {
		f(())
		unbind()
	}
}

trait GLAttach[A, B] {
	def attach(a: A, b: B)
}

trait GLGet[A, @specialized(Int) B, @specialized(Boolean, Int) C] {
	def get(a: A, b: B): C
}

trait GLParameter[A, @specialized(Int) B, @specialized(Int) C] {
	def parameter(a: A, b: B, c: C)
}

trait GLCompile[A] {
	def compile(obj: A)
}

trait GLInfoLog[A] {
	def infoLog(obj: A): String
}

trait GLBufferData[A] {
	def bufferData(buffer: A, usage: Int)
	def subData(buffer: A, offset: Int)
}
