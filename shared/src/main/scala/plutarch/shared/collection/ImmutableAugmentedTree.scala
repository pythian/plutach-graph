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

import scala.annotation.meta.getter
import scala.annotation.tailrec

/**
 * Based on Scala's implementation of immutable RB trees
 */
object ImmutableAugmentedTree {

  @inline private def compare(akeyLeft: Long, akeyRight: Long, bkeyLeft: Long, bkeyRight: Long): Int = {
    if ((akeyLeft < bkeyLeft) || ((akeyLeft == bkeyLeft) && (akeyRight < bkeyRight))) -1
    else if ((akeyLeft > bkeyLeft) || ((akeyLeft == bkeyLeft) && (akeyRight > bkeyRight))) 1
    else 0
  }

  sealed abstract class Tree(
      @(inline @getter) final val keyLeft: Long,
      @(inline @getter) final val keyRight:Long,
      @(inline @getter) final val value:   Set[Int],
      @(inline @getter) final val left:    Tree,
      @(inline @getter) final val right:   Tree) {
    @(inline @getter) final val count: Int = 1 + ImmutableAugmentedTree.count(left) + ImmutableAugmentedTree.count(right)
    @(inline @getter) final val maxRight: Long = calculateMaxRight(keyRight, left, right)
    def black: Tree
    def red: Tree
  }

  final class RedTree(
      keyLeft:  Long,
      keyRight: Long,
      value:    Set[Int],
      left:     Tree,
      right:    Tree) extends Tree(keyLeft, keyRight, value, left, right) {
    override def black: Tree = BlackTree(keyLeft, keyRight, value, left, right)
    override def red: Tree = this
    override def toString: String =
      s"""{"color": "red", "keyLeft": $keyLeft, "keyRight": $keyRight, "value": $value, "maxRight": $maxRight, "left": $left, "right": $right}"""
  }

  final class BlackTree(
      keyLeft:  Long,
      keyRight: Long,
      value:    Set[Int],
      left:     Tree,
      right:    Tree) extends Tree(keyLeft, keyRight, value, left, right) {
    override def black: Tree = this
    override def red: Tree = RedTree(keyLeft, keyRight, value, left, right)
    override def toString: String =
      s"""{"color": "black", "keyLeft": $keyLeft, "keyRight": $keyRight, "value": $value, "maxRight": $maxRight, "left": $left, "right": $right}"""
  }

  object RedTree {
    @inline def apply[B](keyLeft: Long, keyRight: Long, value: Set[Int], left: Tree, right: Tree) =
      new RedTree(keyLeft, keyRight, value, left, right)
  }

  object BlackTree {
    @inline def apply[B](keyLeft: Long, keyRight: Long, value: Set[Int], left: Tree, right: Tree) =
      new BlackTree(keyLeft, keyRight, value, left, right)
  }

  @tailrec def lookup(tree: Tree, xLeft: Long, xRight: Long): Tree = {
    if (tree eq null) null else {
      val cmp = compare(xLeft, xRight, tree.keyLeft, tree.keyRight)
      if (cmp < 0) lookup(tree.left, xLeft, xRight)
      else if (cmp > 0) lookup(tree.right, xLeft, xRight)
      else tree
    }
  }

  def count(tree: Tree): Int = if (tree eq null) 0 else tree.count

  def insert(tree: Tree, keyLeft: Long, keyRight: Long, v: Int): Tree =
    blacken(upd(tree, keyLeft, keyRight, v, isDel = false))

  def delete(tree: Tree, keyLeft: Long, keyRight: Long, v: Int): Tree = {
    if ((lookup(tree, keyLeft, keyRight).value - v).isEmpty) {
      blacken(del(tree, keyLeft, keyRight))
    } else {
      blacken(upd(tree, keyLeft, keyRight, v, isDel = true))
    }
  }

  private def isRedTree(tree: Tree) = tree.isInstanceOf[RedTree]

  private def isBlackTree(tree: Tree) = tree.isInstanceOf[BlackTree]

  private def blacken(t: Tree): Tree = if (t eq null) null else t.black

  @inline private def calculateMaxRight[B](keyRight: Long, x: Tree, y: Tree): Long = {
    var maxRight = keyRight
    if ((x ne null) && x.maxRight > maxRight) maxRight = x.maxRight
    if ((y ne null) && y.maxRight > maxRight) maxRight = y.maxRight
    maxRight
  }

  private def mkTree(isBlack: Boolean, keyLeft: Long, keyRight: Long, v: Set[Int], l: Tree, r: Tree) = {
    if (isBlack) {
      BlackTree(keyLeft, keyRight, v, l, r)
    } else {
      RedTree(keyLeft, keyRight, v, l, r)
    }
  }

  private def balanceLeft(isBlack: Boolean, zLeft: Long, zRight: Long, zv: Set[Int], l: Tree, d: Tree): Tree = {
    if (isRedTree(l) && isRedTree(l.left))
      RedTree(l.keyLeft, l.keyRight, l.value,
        BlackTree(l.left.keyLeft, l.left.keyRight, l.left.value, l.left.left, l.left.right),
        BlackTree(zLeft, zRight, zv, l.right, d))
    else if (isRedTree(l) && isRedTree(l.right)) {
      RedTree(l.right.keyLeft, l.right.keyRight, l.right.value,
        BlackTree(l.keyLeft, l.keyRight, l.value, l.left, l.right.left),
        BlackTree(zLeft, zRight, zv, l.right.right, d))
    } else {
      mkTree(isBlack, zLeft, zRight, zv, l, d)
    }
  }

  private def balanceRight(isBlack: Boolean, xLeft: Long, xRight: Long, xv: Set[Int], a: Tree, r: Tree): Tree = {
    if (isRedTree(r) && isRedTree(r.left)) {
      RedTree(r.left.keyLeft, r.left.keyRight, r.left.value,
        BlackTree(xLeft, xRight, xv, a, r.left.left),
        BlackTree(r.keyLeft, r.keyRight, r.value, r.left.right, r.right))
    } else if (isRedTree(r) && isRedTree(r.right)) {
      RedTree(r.keyLeft, r.keyRight, r.value,
        BlackTree(xLeft, xRight, xv, a, r.left),
        BlackTree(r.right.keyLeft, r.right.keyRight, r.right.value, r.right.left, r.right.right))
    } else {
      mkTree(isBlack, xLeft, xRight, xv, a, r)
    }
  }

  private def upd(tree: Tree, kLeft: Long, kRight: Long, v: Int, isDel: Boolean): Tree = {
    if (tree eq null) {
      RedTree(kLeft, kRight, Set(v), null, null)
    } else {
      val cmp = compare(kLeft, kRight, tree.keyLeft, tree.keyRight)
      if (cmp < 0) {
        balanceLeft(isBlackTree(tree), tree.keyLeft, tree.keyRight, tree.value,
          upd(tree.left, kLeft, kRight, v, isDel), tree.right)
      } else if (cmp > 0) {
        balanceRight(isBlackTree(tree), tree.keyLeft, tree.keyRight, tree.value,
          tree.left, upd(tree.right, kLeft, kRight, v, isDel))
      } else {
        val set = if (isDel) tree.value - v else tree.value + v
        mkTree(isBlackTree(tree), kLeft, kRight, set, tree.left, tree.right)
      }
    }
  }

  private def del(tree: Tree, kLeft: Long, kRight: Long): Tree = {
    if (tree eq null) null else {

      def balance(xLeft: Long, xright: Long, xv: Set[Int], tl: Tree, tr: Tree) = {
        if (isRedTree(tl)) {
          if (isRedTree(tr)) {
            RedTree(xLeft, xright, xv, tl.black, tr.black)
          } else if (isRedTree(tl.left)) {
            RedTree(tl.keyLeft, tl.keyRight, tl.value, tl.left.black,
              BlackTree(xLeft, xright, xv, tl.right, tr))
          } else if (isRedTree(tl.right)) {
            RedTree(tl.right.keyLeft, tl.right.keyRight, tl.right.value,
              BlackTree(tl.keyLeft, tl.keyRight, tl.value, tl.left, tl.right.left),
              BlackTree(xLeft, xright, xv, tl.right.right, tr))
          } else {
            BlackTree(xLeft, xright, xv, tl, tr)
          }
        } else if (isRedTree(tr)) {
          if (isRedTree(tr.right)) {
            RedTree(tr.keyLeft, tr.keyRight, tr.value,
              BlackTree(xLeft, xright, xv, tl, tr.left), tr.right.black)
          } else if (isRedTree(tr.left)) {
            RedTree(tr.left.keyLeft, tr.left.keyRight, tr.left.value,
              BlackTree(xLeft, xright, xv, tl, tr.left.left),
              BlackTree(tr.keyLeft, tr.keyRight, tr.value, tr.left.right, tr.right))
          } else {
            BlackTree(xLeft, xright, xv, tl, tr)
          }
        } else {
          BlackTree(xLeft, xright, xv, tl, tr)
        }
      }

      def subl(t: Tree) = {
        if (t.isInstanceOf[BlackTree]) t.red
        else sys.error("Defect: invariance violation; expected black, got " + t)
      }

      def balLeft(xLeft: Long, xRight: Long, xv: Set[Int], tl: Tree, tr: Tree) = {
        if (isRedTree(tl)) {
          RedTree(xLeft, xRight, xv, tl.black, tr)
        } else if (isBlackTree(tr)) {
          balance(xLeft, xRight, xv, tl, tr.red)
        } else if (isRedTree(tr) && isBlackTree(tr.left)) {
          RedTree(tr.left.keyLeft, tr.left.keyRight, tr.left.value,
            BlackTree(xLeft, xRight, xv, tl, tr.left.left),
            balance(tr.keyLeft, tr.keyRight, tr.value, tr.left.right, subl(tr.right)))
        } else {
          sys.error("Defect: invariance violation")
        }
      }

      def balRight(xLeft: Long, xRight: Long, xv: Set[Int], tl: Tree, tr: Tree) = {
        if (isRedTree(tr)) {
          RedTree(xLeft, xRight, xv, tl, tr.black)
        } else if (isBlackTree(tl)) {
          balance(xLeft, xRight, xv, tl.red, tr)
        } else if (isRedTree(tl) && isBlackTree(tl.right)) {
          RedTree(tl.right.keyLeft, tl.right.keyRight, tl.right.value,
            balance(tl.keyLeft, tl.keyRight, tl.value, subl(tl.left), tl.right.left),
            BlackTree(xLeft, xRight, xv, tl.right.right, tr))
        } else {
          sys.error("Defect: invariance violation")
        }
      }

      def delLeft = {
        if (isBlackTree(tree.left)) {
          balLeft(tree.keyLeft, tree.keyRight, tree.value,
            del(tree.left, kLeft, kRight), tree.right)
        } else {
          RedTree(tree.keyLeft, tree.keyRight, tree.value,
            del(tree.left, kLeft, kRight), tree.right)
        }
      }

      def delRight = {
        if (isBlackTree(tree.right)) {
          balRight(tree.keyLeft, tree.keyRight, tree.value,
            tree.left, del(tree.right, kLeft, kRight))
        } else {
          RedTree(tree.keyLeft, tree.keyRight, tree.value,
            tree.left, del(tree.right, kLeft, kRight))
        }
      }

      def append(tl: Tree, tr: Tree): Tree = {
        if (tl eq null) {
          tr
        } else if (tr eq null) {
          tl
        } else if (isRedTree(tl) && isRedTree(tr)) {
          val bc = append(tl.right, tr.left)
          if (isRedTree(bc)) {
            RedTree(bc.keyLeft, bc.keyRight, bc.value,
              RedTree(tl.keyLeft, tl.keyRight, tl.value, tl.left, bc.left),
              RedTree(tr.keyLeft, tr.keyRight, tr.value, bc.right, tr.right))
          } else {
            RedTree(tl.keyLeft, tl.keyRight, tl.value, tl.left,
              RedTree(tr.keyLeft, tr.keyRight, tr.value, bc, tr.right))
          }
        } else if (isBlackTree(tl) && isBlackTree(tr)) {
          val bc = append(tl.right, tr.left)
          if (isRedTree(bc)) {
            RedTree(bc.keyLeft, bc.keyRight, bc.value,
              BlackTree(tl.keyLeft, tl.keyRight, tl.value, tl.left, bc.left),
              BlackTree(tr.keyLeft, tr.keyRight, tr.value, bc.right, tr.right))
          } else {
            balLeft(tl.keyLeft, tl.keyRight, tl.value, tl.left,
              BlackTree(tr.keyLeft, tr.keyRight, tr.value, bc, tr.right))
          }
        } else if (isRedTree(tr)) {
          RedTree(tr.keyLeft, tr.keyRight, tr.value,
            append(tl, tr.left), tr.right)
        } else if (isRedTree(tl)) {
          RedTree(tl.keyLeft, tl.keyRight, tl.value,
            tl.left, append(tl.right, tr))
        } else {
          sys.error("unmatched tree on append: " + tl + ", " + tr)
        }
      }

      val cmp = compare(kLeft, kRight, tree.keyLeft, tree.keyRight)
      if (cmp < 0) delLeft
      else if (cmp > 0) delRight
      else append(tree.left, tree.right)
    }
  }

}
