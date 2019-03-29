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

package plutarch.shared.data.metrics

import plutarch.shared.data.Aggregations.Aggregation
import plutarch.shared.data.MetricDomain._

case class Conf(
    name:         String,
    step:         Long,
    scales:       Seq[Int],
    aggregations: Seq[Aggregation],
    withTotal:    Boolean,
    unitSelector: UnitSelector     = DefaultUnitSelector,
    dataFormat:   DataFormat       = DefaultDataFormat)