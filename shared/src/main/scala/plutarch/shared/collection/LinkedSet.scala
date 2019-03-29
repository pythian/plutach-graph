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

import scala.collection.mutable.{ HashMap ⇒ MHashMap }

trait LinkedSet[B] {
  def add(value: B): Unit
  def remove(value: B): Unit
  def refresh(value: B): Unit
  def getLast: B
  def clear(): Unit
  def iterator(): Iterator[B]
}

object LinkedSet {

  def empty[B]: LinkedSet[B] = new Impl[B]()

  private final class Node[B](var value: B, var prev: Node[B], var next: Node[B]) {
    override def toString: String = s"Node($value, prev=${if (prev ne null) prev.value else null}, next=$next)"
  }

  class Impl[B] extends LinkedSet[B] {
    private val store = MHashMap.empty[B, Node[B]]
    private var last: Node[B] = null
    private var head: Node[B] = null
    private def cut(node: Node[B]): Unit = {
      if (node.next ne null) node.next.prev = node.prev
      if (node.prev ne null) node.prev.next = node.next
      if (last eq node) last = node.prev
      if (head eq node) head = node.next
    }
    def add(value: B): Unit = {
      val node = if (head ne null) {
        val newNode = new Node(value, null, head)
        head.prev = newNode
        head = newNode
        newNode
      } else {
        val newNode = new Node(value, null, null)
        head = newNode
        last = newNode
        newNode
      }
      store.put(value, node)
    }
    def remove(value: B): Unit = {
      cut(store(value))
      store.remove(value)
    }
    def refresh(value: B): Unit = {
      val node = store(value)
      if (node ne head) {
        cut(node)
        node.next = head
        node.prev = null
        head.prev = node
        head = node
      }
    }
    def getLast: B = last.value
    def clear(): Unit = {
      store.clear()
      last = null
      head = null
    }

    //override def toString: String = if (head ne null) head.toString else "empty"

    override def toString: String = if (head ne null) {
      val sb = StringBuilder.newBuilder
      var node = head
      sb.append("[")
      sb.append(node.value)
      node = node.next
      while (node ne null) {
        sb.append(", ")
        sb.append(node.value)
        node = node.next
      }
      sb.append("]")
      sb.result()
    } else "[]"

    def iterator(): Iterator[B] = new Iterator[B] {
      private var elem = head
      def hasNext: Boolean = elem ne null
      def next(): B = {
        val res = elem.value
        elem = elem.next
        res
      }
    }
  }

}
