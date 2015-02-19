package sublixt

import sublixt.opengl.impl._

package object opengl extends ConstantValues {
	def glGen[A](implicit gen: GLGen[A]) = gen.gen
	def glDelete[A](obj: A)(implicit delete: GLDelete[A]) = delete.delete(obj)
	def glBind[A](obj: A)(implicit bind: GLBind[A]) = bind.bind(obj)
	def glUnbind[A](implicit bind: GLBind[A]) = bind.unbind()

	implicit val vboImplicit = VBOImpl
	implicit val iboImplicit = IBOImpl
	implicit val vaoImplicit = VAOImpl
	implicit val shaderProgramImplicit = ShaderProgramImpl
	implicit val programVertImplicit = ProgramVertAttach
	implicit val programFragImplicit = ProgramFragAttach
	implicit val vertexShaderImpl = VertexShaderImpl
	implicit val fragmentShaderImpl = FragmentShaderImpl
}
