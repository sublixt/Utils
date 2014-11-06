package sublixt.collection.immutable

object AVLTree {
	def empty: AVLTree[Nothing] = Leaf
	def apply[A](elem: A, elems: A*)(implicit order: Ordering[A]) =
		elems.foldLeft(Node(Leaf, elem, Leaf): AVLTree[A])(_ + _)
}

sealed trait AVLTree[+A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	protected[immutable] def balance[A1 >: A](implicit order: Ordering[A1]): AVLTree[A1]
	def apply[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	def contains[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Boolean
	def depth: Int
	def isEmpty: Boolean
}

case class Node[A](left: AVLTree[A], value: A, right: AVLTree[A]) extends AVLTree[A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		//must rebalance the tree all the way up from the node elem was inserted into
		if (comp > 0) Node(left, value, right + elem).balance
		else if (comp < 0) Node(left + elem, value, right).balance
		else this
	}

	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		//must rebalance the tree all the way up from the node elem was inserted into
		(if (comp > 0) Node(left, value, right - elem)
		else if (comp < 0) Node(left - elem, value, right)
		else {
			//recreates the subtree where elem was removed
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
		//calculate the balance factor of the parent
		val bfp = left.depth - right.depth
		if (bfp == 2) { //the tree is imbalanced to the left
			val l = left.asInstanceOf[Node[A1]]
			val ll = l.left
			val lr = l.right
			//calculate the balace factor of the left node
			val bfn = ll.depth - lr.depth

			if (bfn == -1) {
				val lrn = lr.asInstanceOf[Node[A1]]
				val lrl = lrn.left
				val lrr = lrn.right
				//rotates the tree left then right
				//this solves the cases of
				//   n
				// n
				//  n
				Node(Node(ll, l.value, lrl), lrn.value, Node(lrr, value, right))
			} else
				//rotates the tree right
				//this solves the cases of
				//   n
				//  n
				// n
				Node(ll, l.value, Node(lr, value, right))
		} else if (bfp == -2) { //the tree is imbalanced to the right
			val r = right.asInstanceOf[Node[A1]]
			val rl = r.left
			val rr = r.right
			//calculate the balace factor of the right node
			val bfn = rl.depth - rr.depth

			if (bfn == 1) {
				val rln = rl.asInstanceOf[Node[A1]]
				val rll = rln.left
				val rlr = rln.right
				//rotates the tree right then left
				//this solves the cases of
				//   n
				//     n
				//    n
				Node(Node(left, value, rll), rln.value, Node(rlr, r.value, rr))
			} else
				//rotates the tree left
				//this solves the cases of
				//   n
				//    n
				//     n
				Node(Node(left, value, rl), r.value, rr)
		} else {
			//the tree is balanced (balance factor is in the range [-1, 1])
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
	def isEmpty = false
}

case object Leaf extends AVLTree[Nothing] {
	def +[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = Node(Leaf, elem, Leaf)
	def -[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	protected[immutable] def balance[A1 >: Nothing](implicit order: Ordering[A1]) = this
	def apply[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	def contains[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = false
	def depth = 0
	def isEmpty = true
}