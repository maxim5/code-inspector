/*
 * Implementation of the trait T_Measurer to interleave the run of all the models
 */
package benchTool

import acumen._
import java.io._
import java.nio._
import scala.annotation.tailrec
import scala.collection.mutable.Map

object InterleavedRun extends Measurer {
  /**
   * Run a part of an interleaved execution of a pair model-interpreter of the experiment and
   *  return the corresponding times and the number of iterations and the duration
   */
  @tailrec def interleavedAux(mods: List[String], inters: List[String],
                              startingIteNb: Int, endingIteNb: Int,
                              startingTime: Long, endingTime: Long,
                              tmpResult: List[LLtimeT]): (List[LLtimeT], Int, Long) = {
    val firstPart: List[List[LtimeT]] =
      (for (i <- startingIteNb to endingIteNb) yield (
        for (mod <- mods; inter <- inters) yield bench.timeModInter(mod, inter)))toList;
    val mergedList = tmpResult ::: firstPart
    val currTime = System.currentTimeMillis
    if (currTime < endingTime)
      interleavedAux(mods, inters, endingIteNb + 1, endingIteNb * 2,
        startingTime, endingTime, mergedList)
    else
      (mergedList, endingIteNb, currTime - startingTime)
  }

  /**
   * Wrapping function to run time measurements
   */
  def run(mods: List[String], inters: List[String], timeToSpendinSec: timeT,
          nbIte: Int): ResultT = {
    println("Interleaved call with " + inters + " for " + mods)
    val t0 = System.currentTimeMillis
    val toTimeEndInMs = (timeToSpendinSec.toLong) * 1000L + t0
    val values = (for (i <- 1 to nbIte) yield {
      for (mod <- mods; inter <- inters) yield bench.timeModInter(mod, inter)
    }).toList

    val curTime = System.currentTimeMillis
    var globalNbIte = nbIte
    var globalDuration = curTime - t0
    val times = if (curTime >= toTimeEndInMs) {
      values
    } else {
      val rest = interleavedAux(mods, inters, 0, 1, t0, toTimeEndInMs, values)
      globalNbIte = globalNbIte + rest._2
      globalDuration = globalDuration + rest._3
      (values ::: rest._1)
    }
    val names = for (mod <- mods; inter <- inters) yield (mod, inter)
    //Recombining based on the assumption iterations on list are always done in the same order
    val total = names zip (times.transpose)
    for (((m, i), result) <- total) yield (m, i, result, globalNbIte, globalDuration)
  }

}	
