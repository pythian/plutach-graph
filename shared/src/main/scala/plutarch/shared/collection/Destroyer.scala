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

import java.nio.ByteBuffer

object Destroyer {

  def destroy(bb: ByteBuffer): Unit = {
    if (bb.isDirect) {
      bb.asInstanceOf[{ def cleaner: { def clean: Unit } }].cleaner.clean
    }
  }

  //  public static void destroyDirectByteBuffer(ByteBuffer toBeDestroyed)
  //  throws IllegalArgumentException, IllegalAccessException,
  //  InvocationTargetException, SecurityException, NoSuchMethodException {
  //
  //    Preconditions.checkArgument(toBeDestroyed.isDirect(),
  //      "toBeDestroyed isn't direct!");
  //
  //    Method cleanerMethod = toBeDestroyed.getClass().getMethod("cleaner");
  //    cleanerMethod.setAccessible(true);
  //    Object cleaner = cleanerMethod.invoke(toBeDestroyed);
  //    Method cleanMethod = cleaner.getClass().getMethod("clean");
  //    cleanMethod.setAccessible(true);
  //    cleanMethod.invoke(cleaner);
  //
  //  }

}
