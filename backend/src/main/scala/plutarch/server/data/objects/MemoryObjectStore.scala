package plutarch.server.data.objects

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.{ Set ⇒ MSet }
import boopickle.Default.Pickle
import boopickle.Default._
import plutarch.shared.collection.ImmutableIntervalSet
import plutarch.shared.colors.Colors
import plutarch.shared.data.DataObject
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ ExecutionContext, Future }

object MemoryObjectStore {

  case class State(
      nameToId:     Map[String, Int],
      idToObjState: Map[Int, DataObjectState],
      times:        ImmutableIntervalSet)

  def emptyState: State = State(HashMap.empty, HashMap.empty, ImmutableIntervalSet.empty())

  sealed trait Result {
    def state: DataObjectState
  }
  case class Same(state: DataObjectState) extends Result
  case class Changed(state: DataObjectState) extends Result
  case class New(state: DataObjectState) extends Result

  case class DataObjectState(obj: DataObject, first: Long, last: Long) {
    def update(t: Long, latency: Int): Result = {
      if (t > last + latency) {
        Changed(DataObjectState(obj, first, t))
      } else if (t < first - latency) {
        Changed(DataObjectState(obj, t, last))
      } else {
        Same(DataObjectState(obj, first, last))
      }
    }
    override def toString: String = {
      s"""{"id": ${obj.id}, "first": $first, "last": $last}"""
    }
  }

}

abstract class MemoryObjectStore(initId: Int, initState: MemoryObjectStore.State, latency: Int) extends Objects {

  import MemoryObjectStore._

  // currently recreate new each time!
  private val colors = Colors.create()

  private val curId = new AtomicInteger(initId)

  @volatile private var state = initState

  def getState: State = state

  def checkState(t: Long, name: String, initColor: Option[String]): Result = {
    state.nameToId.get(name) match {
      case Some(objId) ⇒
        val prevObjState = state.idToObjState(objId)
        val res = prevObjState.update(t, latency)
        res match {
          case Changed(objState) ⇒
            val newNameToId = state.nameToId
            val newIdToObjState = state.idToObjState + (objId -> objState)
            val newTimes = state.times.update(prevObjState.first, prevObjState.last, objId, objState.first, objState.last)
            state = State(newNameToId, newIdToObjState, newTimes)
          case _ ⇒
        }
        res
      case None ⇒
        val objId = curId.getAndIncrement()
        val color = initColor.getOrElse(colors.next())
        val obj = DataObject(objId, name, color)
        val objState = DataObjectState(obj, t, t)
        val newNameToId = state.nameToId + (name -> objId)
        val newIdToObjState = state.idToObjState + (objId -> objState)
        val newTimes = state.times.add(t, t, objId)
        state = State(newNameToId, newIdToObjState, newTimes)
        New(objState)
    }
  }

  def getObject(name: String): DataObject = {
    val thisState = state
    val id = thisState.nameToId(name)
    thisState.idToObjState(id).obj
  }

  def get(intervals: Seq[(Long, Long)])(implicit executor: ExecutionContext): Future[ByteBuffer] = Future {
    Pickle.intoBytes(getDe(intervals))
  }

  def get(left: Long, right: Long)(implicit executor: ExecutionContext): Future[ByteBuffer] = Future {
    Pickle.intoBytes(getDe(left, right))
  }

  def getDe(left: Long, right: Long): Seq[DataObject] = {
    val thisState = state
    val adjLeft = if (left - latency > left) left else left - latency
    val adjRight = if (right + latency < right) right else right + latency
    thisState.times.searchIds(adjLeft, adjRight).map(id ⇒ thisState.idToObjState(id).obj)
  }

  def getDe(intervals: Seq[(Long, Long)]): Seq[DataObject] = {
    val thisState = state
    val objsIds = MSet.empty[Int]
    val bld = ListBuffer.empty[DataObject]
    for {
      (left, right) ← intervals
      adjLeft = if (left - latency > left) left else left - latency
      adjRight = if (right + latency < right) right else right + latency
      id ← thisState.times.searchIds(adjLeft, adjRight)
    } {
      if (objsIds.add(id)) {
        bld += thisState.idToObjState(id).obj
      }
    }
    bld.result()
  }

  override def toString: String = {
    val thisState = state
    val objectsStr = thisState.nameToId.toList.sortBy(_._2).map(x ⇒ s""""${x._1}": ${x._2}""").mkString("[", ", ", "]")
    val idObjTimeStr = thisState.idToObjState.toList.sortBy(_._1).map(x ⇒ s""""${x._1}": ${x._2}""").mkString("[", ", ", "]")
    s"""{"curId": ${curId.get()}, "objects": $objectsStr, "idObjTime": $idObjTimeStr, "times": ${thisState.times}}"""
  }

}
