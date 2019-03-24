package plutarch.server.data.objects

import plutarch.shared.data.DataObject

/*
* Immutable threadsafe implementation: 1 writer/many readers
* stat is published via volatile variable
* */

class ImmutableObjectStore(name: String) extends MemoryObjectStore(0, MemoryObjectStore.emptyState, Objects.latency) {

  def check(t: Long, name: String, initColor: Option[String]): DataObject = {
    checkState(t, name, initColor).state.obj
  }

  // setup total
  check(Long.MinValue, "total", Some("#43CD80"))
  check(Long.MaxValue, "total")
}