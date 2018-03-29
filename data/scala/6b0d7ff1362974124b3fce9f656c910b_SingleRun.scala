/*
 * Implementation of the trait T_Measurer to run of all the models sequentially
 */
package benchTool

import scala.annotation.tailrec

object SingleRun extends Measurer {
  /**
   * Run a part of sequential execution of a pair model-interpreter and
   *  return the corresponding times and the number of iterations
   */
  @tailrec def singleAux(mod: String, inter: String,
                         startingIteNb: Int, endingIteNb: Int,
                         startingTime: Long, endingTime: Long,
                         tmpResult: LLtimeT): (LLtimeT, Int) = {
    val list: LLtimeT =
      (for (i <- startingIteNb to endingIteNb) yield { bench.timeModInter(mod, inter) }) toList
    val mergedList = tmpResult ::: list
    val currTime = System.currentTimeMillis - startingTime
    if (currTime < endingTime)
      singleAux(mod, inter, endingIteNb + 1, endingIteNb * 2, startingTime, endingTime, mergedList)
    else
      (mergedList, endingIteNb)
  }

  /**
   * Function to measure times for only one pair model-interpreter
   */
  def runOneModOneInter(mod: String, inter: String, longTimeInMs: Long,
                        nbIte: Int): partialResultT = {
    val t0 = System.currentTimeMillis
    val values =
      ((for (i <- 1 to nbIte) yield bench.timeModInter(mod, inter)).toList)
    val time = System.currentTimeMillis
    val (rest, globalNbIte, finalTime) =
      if ((time - t0) < longTimeInMs) {
        val moreResults =
          singleAux(mod, inter, 1, 1, t0, longTimeInMs, List[LtimeT]())
        val time2 = System.currentTimeMillis
        (moreResults._1, nbIte + moreResults._2, System.currentTimeMillis)
      } else
        (Nil, nbIte, time)
    (mod, inter, (values ::: rest), globalNbIte, finalTime - t0)
  }

  def run(mods: List[String], inters: List[String], timeToSpendinSec: timeT,
          nbIte: Int): ResultT = {
    val longTimeInMs = (timeToSpendinSec * 1000L).toLong
    for (mod <- mods; inter <- inters) yield {
      runOneModOneInter(mod, inter, longTimeInMs, nbIte)
    }
  }
}	
