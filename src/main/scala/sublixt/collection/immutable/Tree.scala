package sublixt.collection.immutable

sealed trait Tree[+A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Tree[A1]
	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Tree[A1]
	def apply[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Tree[A1]
	def contains[A1 >: A](elem: A1)(implicit order: Ordering[A1]): Boolean
	def prune[A1 >: A](min: A1, max: A1)(implicit order: Ordering[A1]): Tree[A1]
	def foldLeft[B](sum: B)(f: (B, A) => B): B
	def foldRight[B](sum: B)(f: (B, A) => B): B
	def foldInOrder[B](sum: B)(f: (B, A) => B): B = foldLeft(sum)(f)
	def foldPreOrder[B](sum: B)(f: (B, A) => B): B
	def foldPostOrder[B](sum: B)(f: (B, A) => B): B
	def foreach(f: A => Unit)
	def depth: Int
	def isEmpty: Boolean
}

case class Node[+A](val left: Tree[A], val value: A, val right: Tree[A]) extends Tree[A] {
	def +[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		//must rebalance the tree all the way up from the node elem was inserted into
		if (comp > 0) Node(left, value, right + elem)
		else if (comp < 0) Node(left + elem, value, right)
		else this
	}

	def -[A1 >: A](elem: A1)(implicit order: Ordering[A1]) = {
		val comp = order.compare(elem, value)
		//must rebalance the tree all the way up from the node elem was inserted into
		if (comp > 0) Node(left, value, right - elem)
		else if (comp < 0) Node(left - elem, value, right)
		else {
				//recreates the subtree where elem was removed
				def combineLeftRight(left: Tree[A1], right: Tree[A1]): Tree[A1] = {
					if (left == Leaf) right
					else if (right == Leaf) left
					else {
						val Node(rl, rv, rr) = right
						Node(combineLeftRight(left, rl), rv, rr)
					}
				}

			combineLeftRight(left, right)
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
		//this doesnt create a legal AVLTree
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
	
	val depth = scala.math.max(left.depth, right.depth) + 1
	def isEmpty = false
}

case object Leaf extends Tree[Nothing] {
	def +[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = Node(Leaf, elem, Leaf)
	def -[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	protected[immutable] def rebalance[A1 >: Nothing](implicit order: Ordering[A1]) = this
	def apply[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = this
	def contains[A1 >: Nothing](elem: A1)(implicit order: Ordering[A1]) = false
	def prune[A1 >: Nothing](min: A1, max: A1)(implicit order: Ordering[A1]) = this
	def foldLeft[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foldRight[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foldPreOrder[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foldPostOrder[B](sum: B)(f: (B, Nothing) => B): B = sum
	def foreach(f: Nothing => Unit) {}
	def depth = 0
	def isEmpty = true
}