/*
 *    Copyright (c) 2019 Pythian and Valentin Nikotin
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package plutarch.shared.collection

import scala.annotation.tailrec

/**
 * Scala's implementation of RB-tree, modified to keep nodes in double-linked list (Int -> T)
 */
object RedBlackTreeInt {

  @inline private def compare(akey: Int, bkey: Int): Int = {
    if (akey < bkey) -1 else if (akey > bkey) 1 else 0
  }

  final class Tree[B](var root: Node[B], var size: Int)

  final class Node[B](var key: Int, var value: B, var red: Boolean, var left: Node[B], var right: Node[B], var parent: Node[B], var prev: Node[B], var next: Node[B])

  def empty[B]: Tree[B] = new Tree(null, 0)

  @inline private def isRed(node: Node[_]) = (node ne null) && node.red
  @inline private def isBlack(node: Node[_]) = (node eq null) || !node.red

  @tailrec private def getNode[B](node: Node[B], key: Int): Node[B] = {
    if (node eq null) null
    else {
      val cmp = compare(key, node.key)
      if (cmp < 0) getNode(node.left, key)
      else if (cmp > 0) getNode(node.right, key)
      else node
    }
  }

  @tailrec private def minNodeNonNull[B](node: Node[B]): Node[B] = {
    if (node.left eq null) node else minNodeNonNull(node.left)
  }

  @inline private def fixAfterInsert[B](tree: Tree[B], node: Node[B]): Unit = {
    var z = node
    while (isRed(z.parent)) {
      if (z.parent eq z.parent.parent.left) {
        val y = z.parent.parent.right
        if (isRed(y)) {
          z.parent.red = false
          y.red = false
          z.parent.parent.red = true
          z = z.parent.parent
        } else {
          if (z eq z.parent.right) {
            z = z.parent
            rotateLeft(tree, z)
          }
          z.parent.red = false
          z.parent.parent.red = true
          rotateRight(tree, z.parent.parent)
        }
      } else { // symmetric cases
        val y = z.parent.parent.left
        if (isRed(y)) {
          z.parent.red = false
          y.red = false
          z.parent.parent.red = true
          z = z.parent.parent
        } else {
          if (z eq z.parent.left) {
            z = z.parent
            rotateRight(tree, z)
          }
          z.parent.red = false
          z.parent.parent.red = true
          rotateLeft(tree, z.parent.parent)
        }
      }
    }
    tree.root.red = false
  }

  @inline private def fixAfterDelete[B](tree: Tree[B], node: Node[B], parent: Node[B]): Unit = {
    var x = node
    var xParent = parent
    while ((x ne tree.root) && isBlack(x)) {
      if (x eq xParent.left) {
        var w = xParent.right
        // assert(w ne null)

        if (w.red) {
          w.red = false
          xParent.red = true
          rotateLeft(tree, xParent)
          w = xParent.right
        }
        if (isBlack(w.left) && isBlack(w.right)) {
          w.red = true
          x = xParent
        } else {
          if (isBlack(w.right)) {
            w.left.red = false
            w.red = true
            rotateRight(tree, w)
            w = xParent.right
          }
          w.red = xParent.red
          xParent.red = false
          w.right.red = false
          rotateLeft(tree, xParent)
          x = tree.root
        }
      } else { // symmetric cases
        var w = xParent.left
        // assert(w ne null)

        if (w.red) {
          w.red = false
          xParent.red = true
          rotateRight(tree, xParent)
          w = xParent.left
        }
        if (isBlack(w.right) && isBlack(w.left)) {
          w.red = true
          x = xParent
        } else {
          if (isBlack(w.left)) {
            w.right.red = false
            w.red = true
            rotateLeft(tree, w)
            w = xParent.left
          }
          w.red = xParent.red
          xParent.red = false
          w.left.red = false
          rotateRight(tree, xParent)
          x = tree.root
        }
      }
      xParent = x.parent
    }
    if (x ne null) x.red = false
  }

  @inline private def rotateLeft[B](tree: Tree[B], x: Node[B]): Unit = if (x ne null) {
    val y = x.right
    x.right = y.left

    if (y.left ne null) y.left.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.left) x.parent.left = y
    else x.parent.right = y

    y.left = x
    x.parent = y
  }

  @inline private def rotateRight[B](tree: Tree[B], x: Node[B]): Unit = if (x ne null) {
    val y = x.left
    x.left = y.right

    if (y.right ne null) y.right.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.right) x.parent.right = y
    else x.parent.left = y

    y.right = x
    x.parent = y
  }

  @inline private def transplant[B](tree: Tree[B], to: Node[B], from: Node[B]): Unit = {
    if (to.parent eq null) tree.root = from
    else if (to eq to.parent.left) to.parent.left = from
    else to.parent.right = from

    if (from ne null) from.parent = to.parent
  }

  // opereations

  def insert[B](tree: Tree[B], key: Int, value: B): Unit = {
    var y: Node[B] = null
    var x = tree.root
    var cmp = 1
    while ((x ne null) && cmp != 0) {
      y = x
      cmp = compare(key, x.key)
      x = if (cmp < 0) x.left else x.right
    }

    if (cmp == 0) y.value = value
    else {
      val z = new Node(key, value, true, null, null, y, null, null)

      if (y eq null) tree.root = z
      else if (cmp < 0) {
        y.left = z
        z.next = y
        if (y.prev ne null) {
          y.prev.next = z
          z.prev = y.prev
        }
        y.prev = z
      } else {
        y.right = z
        z.prev = y
        if (y.next ne null) {
          y.next.prev = z
          z.next = y.next
        }
        y.next = z
      }

      fixAfterInsert(tree, z)
      tree.size += 1
    }
  }

  def delete[B](tree: Tree[B], key: Int): Unit = {
    val z = getNode(tree.root, key)
    if (z ne null) {
      var y = z
      var yIsRed = y.red
      var x: Node[B] = null
      var xParent: Node[B] = null

      if (z.left eq null) {
        x = z.right
        transplant(tree, z, z.right)
        xParent = z.parent
      } else if (z.right eq null) {
        x = z.left
        transplant(tree, z, z.left)
        xParent = z.parent
      } else {
        y = minNodeNonNull(z.right)
        yIsRed = y.red
        x = y.right

        if (y.parent eq z) xParent = y
        else {
          xParent = y.parent
          transplant(tree, y, y.right)
          y.right = z.right
          y.right.parent = y
        }
        transplant(tree, z, y)
        y.left = z.left
        y.left.parent = y
        y.red = z.red
      }

      if (!yIsRed) fixAfterDelete(tree, x, xParent)
      tree.size -= 1

      if (z.prev ne null) z.prev.next = z.next
      if (z.next ne null) z.next.prev = z.prev
    }
  }

  def minNodeNonNull[B](tree: Tree[B]): Node[B] = minNodeNonNull(tree.root)

  def getNode[B](tree: Tree[B], key: Int): Node[B] = getNode(tree.root, key)

  def minNodeAfter[B](tree: Tree[B], key: Int): Node[B] = {
    if (tree.root eq null) null
    else {
      var y: Node[B] = null
      var x = tree.root
      var cmp = 1
      while ((x ne null) && cmp != 0) {
        y = x
        cmp = compare(key, x.key)
        x = if (cmp < 0) x.left else x.right
      }
      if (cmp <= 0) y else y.next // successor(y)
    }
  }

  def maxNodeBefore[B](tree: Tree[B], key: Int): Node[B] = {
    if (tree.root eq null) null
    else {
      var y: Node[B] = null
      var x = tree.root
      var cmp = 1
      while ((x ne null) && cmp != 0) {
        y = x
        cmp = compare(key, x.key)
        x = if (cmp < 0) x.left else x.right
      }
      if (cmp >= 0) y else y.prev //predecessor(y)
    }
  }

  def maxNodeBeforeOrFirst[B](tree: Tree[B], key: Int): Node[B] = if (tree.root eq null) null else {
    val node = maxNodeBefore(tree, key)
    if (node eq null) minNodeNonNull(tree) else node
  }

  def iterator[B](tree: Tree[B]): Iterator[(Int, B)] = if (tree.root eq null) Iterator.empty else new Iterator[(Int, B)] {
    private var node = minNodeNonNull(tree.root)
    override def hasNext: Boolean = node ne null
    override def next(): (Int, B) = {
      val res = node
      node = node.next
      (res.key, res.value)
    }
  }

  def getOrElseUpdate[B](tree: Tree[B], key: Int, value: ⇒ B): B = {
    val node = RedBlackTreeInt.getNode(tree.root, key)
    if (node eq null) {
      val data = value
      RedBlackTreeInt.insert(tree, key, data)
      data
    } else {
      node.value
    }
  }

  def clear[B](tree: Tree[B]): Unit = {
    tree.root = null
    tree.size = 0
  }

  def toString[B](tree: Tree[B]): String = {
    iterator(tree).map {
      case (key, value) ⇒
        s""""$key": "$value""""
    }.mkString("{", ", ", "}")
  }

}