package sublixt.collection.immutable

object AVLTree {
	def empty: AVLTree[Nothing] = Leaf
}

trait AVLTree[+A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	protected[immutable] def balance[A1 >: A](implicit order: Ordering[A1]): AVLTree[A1]
	def apply[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	def contains[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Boolean
	def depth: Int
}

case class Node[A](left: AVLTree[A], value: A, right: AVLTree[A]) extends AVLTree[A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		if (comp > 0) Node(left, value, right + elem).balance
		else if (comp < 0) Node(left + elem, value, right).balance
		else this
	}

	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		(if (comp > 0) Node(left, value, right - elem)
		else if (comp < 0) Node(left - elem, value, right)
		else {
				def combineLeftRight(left: AVLTree[A1], right: AVLTree[A1]): AVLTree[A1] = {
					if (left == Leaf) right
					else if (right == Leaf) left
					else {
						val Node(rl, rv, rr) = right
						Node(combineLeftRight(left, rl), rv, rr)
					}
				}

			combineLeftRight(left, right)
		}).balance
	}

	protected[immutable] def balance[A1 >: A](implicit order: Ordering[A1]) = {
		val bfp = left.depth - right.depth
		if (bfp == 2) {
			val l = left.asInstanceOf[Node[A1]]
			val ll = l.left
			val lr = l.right
			val bfn = ll.depth - lr.depth

			if (bfn == -1) {
				val lrn = lr.asInstanceOf[Node[A1]]
				val lrl = lrn.left
				val lrr = lrn.right
				Node(Node(ll, l.value, lrl), lrn.value, Node(lrr, value, right))
			} else if (bfn == 1)
				Node(ll, l.value, Node(lr, value, right))
			else this
		} else if (bfp == -2) {
			val r = right.asInstanceOf[Node[A1]]
			val rl = r.left
			val rr = r.right
			val bfn = rl.depth - rr.depth

			if (bfn == 1) {
				val rln = rl.asInstanceOf[Node[A1]]
				val rll = rln.left
				val rlr = rln.right
				Node(Node(left, value, rll), rln.value, Node(rlr, r.value, rr))
			} else if (bfn == -1)
				Node(Node(left, value, rl), r.value, rr)
			else this
		} else {
			this
		}
	}
	
	def apply[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		if(comp == 0) this
		else if(comp > 0) right.apply(elem)
		else left.apply(elem)
	} 

	def contains[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		if (comp == 0) true
		else if (comp > 0) right.contains(elem)
		else left.contains(elem)
	}

	def depth = scala.math.max(left.depth, right.depth) + 1
}

case object Leaf extends AVLTree[Nothing] {
	def +[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = Node(Leaf, elem, Leaf)
	def -[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	protected[immutable] def balance[A1 >: Nothing](implicit order: Ordering[A1]) = this
	def apply[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	def contains[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = false
	def depth = 0
}