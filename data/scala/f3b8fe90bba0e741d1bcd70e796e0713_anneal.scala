/**
 * Manage a simulated annealing trial
 *
 * With an energy function E that we're looking to minimize, for any given
 *   state y, the probability of changing to that state from state x
 *   is exp(-lambda*(E(y)-E(x))).
 *
 * To perform an annealing step, this class takes (E(y)-E(x)) as the
 *   parameter delta, applying the given state change function if necessary.
 *
 * The annealing parameter lambda (alternatively, 1/kT) is given by
 *   lambda_n = log(1+n), where n is the number of state changes divided by
 *   some arbitrary threshold. For example, in the monkey painting application,
 *   a threshold high enough to yield a starting canvas that is not completely
 *   white but splattered with a few random colors.
 */
import scala.math.{exp, floor, log}
import scala.util.Random

abstract class Anneal {
  def generator: Random
  def threshold: Int

  var changes: Int = 0
  var iterations: Int = 0

  def lambda: Double = log(1 + floor(changes/threshold))

  def step(delta: Long)(change: => Unit) {
    iterations += 1
    if (delta < 0 || exp(-lambda*delta) > generator.nextDouble) {
      changes += 1
      change
    }
  }

  override def toString: String = "" + changes + "/" + iterations
}
