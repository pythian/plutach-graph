package plutarch.shared.collection

import scala.collection.mutable.ListBuffer

trait ImmutableIntervalSet {
  def add(left: Long, right: Long, id: Int): ImmutableIntervalSet
  def remove(left: Long, right: Long, id: Int): ImmutableIntervalSet
  def update(left: Long, right: Long, id: Int, newLeft: Long, newRight: Long): ImmutableIntervalSet
  def search(left: Long, right: Long): Seq[(Long, Long, Int)]
  def searchIds(left: Long, right: Long): Seq[Int]
}

object ImmutableIntervalSet {

  def empty(): ImmutableIntervalSet = new Impl(null)

  class Impl(atree: ImmutableAugmentedTree.Tree) extends ImmutableIntervalSet {

    def add(left: Long, right: Long, id: Int): ImmutableIntervalSet = {
      new Impl(ImmutableAugmentedTree.insert(atree, left, right, id))
    }

    def remove(left: Long, right: Long, id: Int): ImmutableIntervalSet = {
      new Impl(ImmutableAugmentedTree.delete(atree, left, right, id))
    }

    def update(left: Long, right: Long, id: Int, newLeft: Long, newRight: Long): ImmutableIntervalSet = {
      remove(left, right, id).add(newLeft, newRight, id)
    }

    def search(left: Long, right: Long): Seq[(Long, Long, Int)] = {
      val bld = ListBuffer.empty[(Long, Long, Int)]

      def queryTree(node: ImmutableAugmentedTree.Tree): Unit =
        if ((node ne null) && node.maxRight >= left) {
          if (node.keyRight >= left && node.keyLeft <= right) {
            node.value.foreach { id ⇒
              bld += ((node.keyLeft, node.keyRight, id))
            }
          }
          if (node.left ne null) {
            queryTree(node.left)
          }
          if ((node.right ne null) && node.keyLeft <= right) {
            queryTree(node.right)
          }
        }

      queryTree(atree)
      bld.result()
    }

    def searchIds(left: Long, right: Long): Seq[Int] = {
      val bld = ListBuffer.empty[Int]

      def queryTree(node: ImmutableAugmentedTree.Tree): Unit =
        if ((node ne null) && node.maxRight >= left) {
          if (node.keyRight >= left && node.keyLeft <= right) {
            node.value.foreach { id ⇒
              bld += id
            }
          }
          if (node.left ne null) {
            queryTree(node.left)
          }
          if ((node.right ne null) && node.keyLeft <= right) {
            queryTree(node.right)
          }
        }

      queryTree(atree)
      bld.result()
    }

    override def toString: String = if (atree != null) atree.toString else "empty"
  }

}
