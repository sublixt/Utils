package sublixt.collection.immutable

object AVLTree {
	def empty[A]: AVLTree[A] = AVLeaf
	def apply[A](elem: A, elems: A*)(implicit order: Ordering[A]) =
		elems.foldLeft(AVLNode(AVLeaf, elem, AVLeaf): AVLTree[A])(_ + _)
}

sealed trait AVLTree[+A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	protected[immutable] def rebalance[A1 >: A](implicit order: Ordering[A1]): AVLTree[A1]
	def apply[A1 >: A](elem: A1)(implicit order: Ordering[A1]): AVLTree[A1]
	def contains[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Boolean
	def prune[A1 >: A](min: A1, max: A1)(implicit order: Ordering[A1]): Tree[A1]
	def foreach(f: A => Unit)
	def foldLeft[B](sum: B)(f: (B, A) => B): B
	def foldRight[B](sum: B)(f: (B, A) => B): B
	def foldInOrder[B](sum: B)(f: (B, A) => B): B = foldLeft(sum)(f)
	def foldPreOrder[B](sum: B)(f: (B, A) => B): B
	def foldPostOrder[B](sum: B)(f: (B, A) => B): B
	def depth: Int
	def isEmpty: Boolean
	def balance: Int
	def toList = foldRight(Nil: List[A])((b, a) => a :: b)
}

case class AVLNode[A](left: AVLTree[A], value: A, right: AVLTree[A]) extends AVLTree[A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		//must rebalance the tree all the way up from the node elem was inserted into
		if (comp > 0) AVLNode(left, value, right + elem).rebalance
		else if (comp < 0) AVLNode(left + elem, value, right).rebalance
		else this
	}

	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		//must rebalance the tree all the way up from the node elem was inserted into
		(if (comp > 0) AVLNode(left, value, right - elem)
		else if (comp < 0) AVLNode(left - elem, value, right)
		else {
				//recreates the subtree where elem was removed
				def combineLeftRight(left: AVLTree[A1], right: AVLTree[A1]): AVLTree[A1] = {
					if (left == AVLeaf) right
					else if (right == AVLeaf) left
					else {
						val AVLNode(rl, rv, rr) = right
						AVLNode(combineLeftRight(left, rl), rv, rr)
					}
				}

			combineLeftRight(left, right)
		}).rebalance
	}

	protected[immutable] def rebalance[A1 >: A](implicit order: Ordering[A1]) = {
		//calculate the balance factor of the parent
		val bfp = balance
		if (bfp == 2) { //the tree is imbalanced to the left
			val l = left.asInstanceOf[AVLNode[A1]]
			val ll = l.left
			val lr = l.right
			//calculate the balace factor of the left node
			val bfn = l.balance

			if (bfn == -1) {
				val lrn = lr.asInstanceOf[AVLNode[A1]]
				val lrl = lrn.left
				val lrr = lrn.right
				//rotates the tree left then right
				//this solves the cases of
				//   n
				// n
				//  n
				AVLNode(AVLNode(ll, l.value, lrl), lrn.value, AVLNode(lrr, value, right))
			} else
				//rotates the tree right
				//this solves the cases of
				//   n
				//  n
				// n
				AVLNode(ll, l.value, AVLNode(lr, value, right))
		} else if (bfp == -2) { //the tree is imbalanced to the right
			val r = right.asInstanceOf[AVLNode[A1]]
			val rl = r.left
			val rr = r.right
			//calculate the balace factor of the right node
			val bfn = r.balance

			if (bfn == 1) {
				val rln = rl.asInstanceOf[AVLNode[A1]]
				val rll = rln.left
				val rlr = rln.right
				//rotates the tree right then left
				//this solves the cases of
				//   n
				//     n
				//    n
				AVLNode(AVLNode(left, value, rll), rln.value, AVLNode(rlr, r.value, rr))
			} else
				//rotates the tree left
				//this solves the cases of
				//   n
				//    n
				//     n
				AVLNode(AVLNode(left, value, rl), r.value, rr)
		} else {
			//the tree is balanced (balance factor is in the range [-1, 1])
			this
		}
	}

	def apply[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		if (comp == 0) this
		else if (comp > 0) right.apply(elem)
		else left.apply(elem)
	}

	def contains[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		if (comp == 0) true
		else if (comp > 0) right.contains(elem)
		else left.contains(elem)
	}

	def prune[A1 >: A](min: A1, max: A1)(implicit order: Ordering[A1]): Tree[A1] = {
		val minComp = order.compare(value, min)
		val maxComp = order.compare(max, value)
		if (minComp < 0)
			right.prune(min, max)
		else if (maxComp < 0)
			left.prune(min, max)
		else
			Node(left.prune(min, max), value, right.prune(min, max))
	}

	def foldLeft[B](sum: B)(f: (B, A) => B): B = {
		val lsum = left.foldLeft(sum)(f)
		val vsum = f(lsum, value)
		right.foldLeft(vsum)(f)
	}

	def foldRight[B](sum: B)(f: (B, A) => B): B = {
		val rsum = right.foldRight(sum)(f)
		val vsum = f(rsum, value)
		left.foldRight(vsum)(f)
	}
	
	def foldPreOrder[B](sum: B)(f: (B, A) => B): B = {
		val vsum = f(sum, value)
		val lsum = left.foldPreOrder(vsum)(f)
		right.foldPreOrder(lsum)(f)
	}
	
	def foldPostOrder[B](sum: B)(f: (B, A) => B): B = {
		val lsum = left.foldPostOrder(sum)(f)
		val rsum = right.foldPostOrder(lsum)(f)
		f(rsum, value)
	}
	
	def foreach(f: A => Unit) {
		left foreach f
		f(value)
		right foreach f
	}
	
	def isEmpty = false
	//these two being vals is very important for performance 
	val depth = scala.math.max(left.depth, right.depth) + 1
	val balance = left.depth - right.depth
}

case object AVLeaf extends AVLTree[Nothing] {
	def +[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = AVLNode(AVLeaf, elem, AVLeaf)
	def -[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	protected[immutable] def rebalance[A1 >: Nothing](implicit order: Ordering[A1]) = this
	def apply[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	def contains[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = false
	def prune[A1 >: Nothing](min: A1, max: A1)(implicit order: Ordering[A1]) = Leaf
	def foldLeft[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foldRight[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foldPreOrder[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foldPostOrder[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foreach(f: Nothing => Unit) {}
	def depth = 0
	def isEmpty = true
	def balance = 0
}