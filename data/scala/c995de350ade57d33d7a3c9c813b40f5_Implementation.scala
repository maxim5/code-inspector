/**
 * *****************************************************************************
 * Copyright 2012 Roland Ewald
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ****************************************************************************
 */
package sessl.reference

import sessl.AbstractObservation
import sessl.AbstractExperiment
import sessl.AbstractDataSink
import sessl.AbstractHypothesis
import sessl.AbstractReport
import sessl._LCG
import sessl._MersenneTwister
import sessl.AbstractOptimization
import sessl.optimization._SimulatedAnnealing
import sessl.AbstractParallelExecution
import sessl._DirectMethod
import sessl._NextReactionMethod
import sessl._TauLeaping
import sessl.ExperimentResults
import sessl.util.SimpleObservation

/**
 * Empty entities to easily check whether an experiment specification depends on system-specific
 *  configuration and components or not. This is a 'formal' reference implementation, i.e. it provides all entities with
 *  correct names and interfaces - but it does not realize any functionality so far.
 *
 *  @author Roland Ewald
 */

//Central entities
class Experiment extends AbstractExperiment with SimpleObservation {
  def executeExperiment() = {}
  def basicConfiguration = {}
}
object EmptyExperiment extends Experiment

//Instrumentation
trait Observation extends AbstractObservation {
  this: AbstractExperiment =>
}

//Data sinks
trait DataSink extends AbstractDataSink {
  this: Observation =>
}

//Optimization
trait Optimization extends AbstractOptimization {
  this: AbstractExperiment with AbstractObservation =>
}

//Parallel execution
trait ParallelExecution extends AbstractParallelExecution {
  override def configureParallelExecution(threads: Int) = {}
}

//Hypothesis tests
trait Hypothesis extends AbstractHypothesis {
  this: Observation =>
}

//Reporting
trait Report extends AbstractReport {
  this: AbstractExperiment with Observation =>
  override def generateReport(results: ExperimentResults) = {}
}

//Algorithms:

//RNGs
case class LCG(val seed: Long = 0) extends _LCG
case class MersenneTwister(val seed: Long = 0) extends _MersenneTwister

//Optimizers
case object SimulatedAnnealing extends _SimulatedAnnealing
case object HillClimbing extends _SimulatedAnnealing

//Simulators
case object DirectMethod extends _DirectMethod
case object NextReactionMethod extends _NextReactionMethod
case object TauLeaping extends _TauLeaping
