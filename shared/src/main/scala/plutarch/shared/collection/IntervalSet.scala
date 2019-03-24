package plutarch.shared.collection

import scala.collection.mutable.ListBuffer

/*
* This is used for object store where each object has life-interval and we query "all objects appeared in interval"
* */

trait IntervalSet {
  def add(left: Long, right: Long, id: Int): Unit
  def remove(left: Long, right: Long, id: Int): Unit
  def update(left: Long, right: Long, id: Int, newLeft: Long, newRight: Long): Unit
  def search(left: Long, right: Long): Seq[(Long, Long, Int)]
  def searchIds(left: Long, right: Long): Seq[Int]
}

object IntervalSet {

  def empty(): Impl = new Impl()

  class Impl() extends IntervalSet {
    private val atree = AugmentedTree.empty
    def add(left: Long, right: Long, id: Int): Unit = {
      AugmentedTree.insert(atree, left, right, id)
    }
    def remove(left: Long, right: Long, id: Int): Unit = {
      AugmentedTree.delete(atree, left, right, id)
    }
    def update(left: Long, right: Long, id: Int, newLeft: Long, newRight: Long): Unit = {
      AugmentedTree.delete(atree, left, right, id)
      AugmentedTree.insert(atree, newLeft, newRight, id)
    }

    def search(left: Long, right: Long): Seq[(Long, Long, Int)] = {
      val bld = ListBuffer.empty[(Long, Long, Int)]
      def queryTree(node: AugmentedTree.Node): Unit =
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
      queryTree(atree.root)
      bld.result()
    }

    def searchIds(left: Long, right: Long): Seq[Int] = {
      val bld = ListBuffer.empty[Int]
      def queryTree(node: AugmentedTree.Node): Unit =
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
      queryTree(atree.root)
      bld.result()
    }

    override def toString: String = atree.toJsonString
  }
}
