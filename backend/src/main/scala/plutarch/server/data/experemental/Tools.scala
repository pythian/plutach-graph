package plutarch.server.data.experemental

object Tools {
  @inline
  def timing[T](name: String)(action: ⇒ T): T = {
    val t0 = System.nanoTime()
    val res = action
    val t1 = System.nanoTime()
    println(s"$name executed in ${(t1 - t0) / 1e6} ms")
    res
  }
}
