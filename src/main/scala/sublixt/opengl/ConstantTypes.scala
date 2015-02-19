package sublixt.opengl

final class TFStatus private[opengl] (val id: Int) {
	override def equals(other: Any) =
		other.isInstanceOf[TFStatus] && (this eq other.asInstanceOf[TFStatus])
}
