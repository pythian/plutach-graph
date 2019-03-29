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

package plutarch.server.data.persistence

import java.nio.ByteBuffer
import java.nio.file.{ Files, Path, Paths }

import com.typesafe.scalalogging.LazyLogging
import plutarch.server.data.persistence.util.{ AppendableFileStore, FileOffsetStore }
import plutarch.server.data.scale.AggregationStore
import scala.concurrent.{ ExecutionContext, Future }

object FileAggregationStore extends LazyLogging {

  private val emptyByteBuffer: ByteBuffer = ByteBuffer.wrap(Array[Byte]())
  private val partialOffsetsPath = Paths.get("offsets.dat")
  private val partialDataPath = Paths.get("data.dat")

  def create(path: Path, step: Long, count: Int)(implicit executor: ExecutionContext): FileAggregationStore = {
    logger.info(s"Creating FileAggregationStore($path)")
    Files.createDirectories(path)
    val offsetsPath = path.resolve(partialOffsetsPath)
    val dataPath = path.resolve(partialDataPath)
    val fos = FileOffsetStore.create(offsetsPath, step, count)
    val afs = AppendableFileStore.create(dataPath)
    new FileAggregationStore(fos, afs)
  }

  def open(path: Path)(implicit executor: ExecutionContext): FileAggregationStore = {
    logger.info(s"Opening FileAggregationStore($path)")
    val offsetsPath = path.resolve(partialOffsetsPath)
    val dataPath = path.resolve(partialDataPath)
    val fos = FileOffsetStore.open(offsetsPath)
    val afs = AppendableFileStore.open(dataPath)
    new FileAggregationStore(fos, afs)
  }

}

class FileAggregationStore(val fos: FileOffsetStore, val afs: AppendableFileStore)(implicit executor: ExecutionContext) extends AggregationStore with LazyLogging {
  import FileAggregationStore._

  private val step: Long = fos.step

  def add(key: Long, value: ByteBuffer): Future[Unit] = {

    if (!fos.isEmpty && key < fos.until) {
      logger.error(s"Late data keys are not supported, key=$key, expected=${fos.until}, FileAggregationStore(${afs.path.getParent})")
      throw new RuntimeException("Late data keys are not supported")
    }

    for {
      pos ← afs.put(value)
    } yield {
      if (pos > Int.MaxValue) {
        logger.error(s"Storage exceed Int.MaxValue size FileAggregationStore(${afs.path.getParent}), pos=$pos")
      } else {
        if (!fos.isEmpty && key > fos.until) {
          fos.until.to(key, step).foreach(fos.add(_, pos.toInt))
        } else {
          fos.add(key, pos.toInt)
        }
      }
    }
  }

  def get(x: Long, y: Long): Future[ByteBuffer] = {

    if (x < fos.until && y >= fos.from) {
      // round x/y to align with offsets
      val xRound = ((x / step) * step).max(fos.from)
      val yRound = ((y / step) * step).min(fos.until)

      val startOffset = fos.getOrElse(xRound, 0)
      assert(afs.current <= Int.MaxValue)
      val len = fos.getOrElse(yRound, afs.current.toInt) - startOffset

      afs.get(startOffset, len)
    } else {
      Future.successful(emptyByteBuffer)
    }

  }

  def commit(): Unit = {
    afs.commit()
    fos.commit()
  }

  def close(): Unit = {
    afs.close()
    fos.close()
  }

}