package sublixt.opengl

final class TFStatus private[opengl] (val id: Int) {
	override def equals(other: Any) =
		other.isInstanceOf[TFStatus] && (this eq other.asInstanceOf[TFStatus])
}

final class TexTarget private[opengl] (val id: Int) {
	override def equals(other: Any) =
		other.isInstanceOf[TexTarget] && (this eq other.asInstanceOf[TexTarget])
}

final class FBOAttachment private[opengl] (val id: Int) {
	override def equals(other: Any) =
		other.isInstanceOf[FBOAttachment] && (this eq other.asInstanceOf[FBOAttachment])
}
