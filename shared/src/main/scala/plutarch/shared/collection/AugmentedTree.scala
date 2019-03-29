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
import scala.collection.mutable.{ Set ⇒ MSet }

/**
 * Based on Scala's implementation of mutable RB trees
 */
object AugmentedTree {

  @inline private def compare(akeyLeft: Long, akeyRight: Long, bkeyLeft: Long, bkeyRight: Long): Int = {
    if ((akeyLeft < bkeyLeft) || ((akeyLeft == bkeyLeft) && (akeyRight < bkeyRight))) -1
    else if ((akeyLeft > bkeyLeft) || ((akeyLeft == bkeyLeft) && (akeyRight > bkeyRight))) 1
    else 0
  }

  final class Tree(var root: Node, var size: Int) {
    def toJsonString: String =
      s"""{"arraysize": $size, "root": ${if (root ne null) root.toJsonString else null}}"""
  }

  final class Node(
      var keyLeft:  Long,
      var keyRight: Long,
      val value:    MSet[Int],
      var red:      Boolean,
      var left:     Node,
      var right:    Node,
      var parent:   Node,
      var maxRight: Long) {
    override def toString: String = s"Node($keyLeft, $keyRight, $maxRight, $left, $right)"
    def toJsonString: String =
      s"""{"keyLeft": $keyLeft, "keyRight": $keyRight, "value": ${value.mkString("[", ", ", "]")}, "maxRight": $maxRight, "left": ${if (left ne null) left.toJsonString else null}, "right": ${if (right ne null) right.toJsonString else null}}"""
  }

  def empty: Tree = new Tree(null, 0)

  @inline private def isRed(node: Node) = (node ne null) && node.red
  @inline private def isBlack(node: Node) = (node eq null) || !node.red

  @tailrec private def getNode(node: Node, keyLeft: Long, keyRight: Long): Node = {
    if (node eq null) null
    else {
      val cmp = compare(keyLeft, keyRight, node.keyLeft, node.keyRight)
      if (cmp < 0) getNode(node.left, keyLeft, keyRight)
      else if (cmp > 0) getNode(node.right, keyLeft, keyRight)
      else node
    }
  }

  @tailrec private def minNodeNonNull(node: Node): Node = {
    if (node.left eq null) node else minNodeNonNull(node.left)
  }

  def insert(tree: Tree, keyLeft: Long, keyRight: Long, value: Int): Unit = {
    var y: Node = null
    var x = tree.root
    var cmp = 1
    while ((x ne null) && cmp != 0) {
      if (x.maxRight < keyRight) x.maxRight = keyRight
      y = x
      cmp = compare(keyLeft, keyRight, x.keyLeft, x.keyRight)
      x = if (cmp < 0) x.left else x.right
    }

    if (cmp == 0) y.value.add(value)
    else {
      val z = new Node(keyLeft, keyRight, MSet(value), true, null, null, y, keyRight)

      if (y eq null) tree.root = z
      else if (cmp < 0) y.left = z
      else y.right = z

      fixAfterInsert(tree, z)
      tree.size += 1
    }
  }

  @inline private def fixAfterInsert(tree: Tree, node: Node): Unit = {
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

  def delete(tree: Tree, keyLeft: Long, keyRight: Long, value: Int): Unit = {
    val z = getNode(tree.root, keyLeft, keyRight)
    z.value.remove(value)
    if (z.value.isEmpty) {
      deleteNode(tree, keyLeft, keyRight, z)
    }
  }

  private def deleteNode(tree: Tree, keyLeft: Long, keyRight: Long, initz: Node = null): Unit = {
    val z = if (initz ne null) initz else getNode(tree.root, keyLeft, keyRight)
    if (z ne null) {
      var y = z
      var yIsRed = y.red
      var x: Node = null
      var xParent: Node = null

      if (z.left eq null) {
        x = z.right
        transplant(tree, z, z.right)
        recalculateUntil(z.parent, null)
        xParent = z.parent
      } else if (z.right eq null) {
        x = z.left
        transplant(tree, z, z.left)
        recalculateUntil(z.parent, null)
        xParent = z.parent
      } else {
        y = minNodeNonNull(z.right)
        yIsRed = y.red
        x = y.right

        if (y.parent eq z) xParent = y
        else {
          xParent = y.parent
          transplant(tree, y, y.right)
          recalculateUntil(xParent, z)
          y.right = z.right
          y.right.parent = y
        }
        transplant(tree, z, y)
        y.left = z.left
        y.left.parent = y
        y.red = z.red
        recalculateUntil(y, null)
      }

      if (!yIsRed) fixAfterDelete(tree, x, xParent)
      tree.size -= 1

    }
  }

  @inline private def fixAfterDelete(tree: Tree, node: Node, parent: Node): Unit = {
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

  @tailrec private def recalculateUntil(x: Node, until: Node): Unit = {
    if (x ne until) {
      recalculateMaxRight(x)
      recalculateUntil(x.parent, until)
    }
  }

  @inline private def recalculateMaxRight(x: Node): Unit = {
    x.maxRight = x.keyRight
    if ((x.left ne null) && x.left.maxRight > x.maxRight) x.maxRight = x.left.maxRight
    if ((x.right ne null) && x.right.maxRight > x.maxRight) x.maxRight = x.right.maxRight
  }

  @inline private def rotateLeft(tree: Tree, x: Node): Unit = if (x ne null) {
    val y = x.right
    x.right = y.left

    if (y.left ne null) y.left.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.left) x.parent.left = y
    else x.parent.right = y

    y.left = x
    x.parent = y

    recalculateMaxRight(x)
    y.maxRight = y.maxRight.max(x.maxRight)
  }

  @inline private def rotateRight(tree: Tree, x: Node): Unit = if (x ne null) {
    val y = x.left
    x.left = y.right

    if (y.right ne null) y.right.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.right) x.parent.right = y
    else x.parent.left = y

    y.right = x
    x.parent = y

    recalculateMaxRight(x)
    y.maxRight = y.maxRight.max(x.maxRight)
  }

  @inline private def transplant(tree: Tree, to: Node, from: Node): Unit = {
    if (to.parent eq null) tree.root = from
    else if (to eq to.parent.left) to.parent.left = from
    else to.parent.right = from

    if (from ne null) from.parent = to.parent
  }

  // testing

  def foldTree[B, Acc](node: Node, acc: Acc)(fun: (Acc, Node) ⇒ Acc): Acc = {
    if (node eq null) acc
    else foldTree(node.right, foldTree(node.left, fun(acc, node))(fun))(fun)
  }

}
