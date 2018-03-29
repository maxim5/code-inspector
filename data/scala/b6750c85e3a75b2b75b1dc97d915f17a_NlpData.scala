package edu.mit.media.defuse.libdefuse.nlp

import edu.mit.media.defuse.libdefuse._
import edu.mit.media.defuse.libdefuse.Util
import edu.mit.media.defuse.mapreduce._
import java.io._
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs._
import org.apache.hadoop.mapreduce._
import scala.io._
import scala.collection.mutable
import edu.mit.media.defuse.libdefuse.IntCounter
import scalanlp.data.process.SegmentWords
import com.twitter.json._

/*
 * Modified from ScalaNLP's for Defuse's world
 */
object NlpData extends Logging {
  private lazy val classLocation = NlpData.getClass.getProtectionDomain.getCodeSource.getLocation
  private lazy val classLoader = NlpData.getClass.getClassLoader
  private def mapFirstWordFilter(l:String) = l.split("<-")(0).split("->")(0)
  private def stringsFromFile(s:String) = {
    val src = Util.loadFile(s)
    val result = (for{ line <- src.getLines();
                      // Comments start with #
                      trimmed = line.trim if !line.startsWith("#") && line.trim.size > 0 }
                      yield trimmed).toSeq
    result
  }

  private def lineToNGram(line:String) = NGram.apply(line)

  private def wordListFromJarPath(path:String):Set[NGram] = stringsFromFile(path).foldLeft(mutable.HashSet[NGram]())((acc, line) => {
    try {
      acc += lineToNGram(line.toLowerCase)
    } catch {
      case e:MatchError =>
      log.warn("Could not convert to ngram (n<=4): " + line)
    }
    acc
  }).toSet
  private def wordListFromMapAtJarPath(path:String):Set[NGram] = stringsFromFile(path).map(mapFirstWordFilter).map(lineToNGram).toSet

  lazy val famousPeople:Set[NGram] = wordListFromJarPath("wordlists/people.txt")
  lazy val greVocab:Set[NGram] = wordListFromJarPath("wordlists/gre words.txt")
  lazy val worstWords:Set[NGram] = wordListFromJarPath("wordlists/worst words.txt")
  lazy val insults:Set[NGram] = wordListFromMapAtJarPath("wordlists/insults map.txt")
  lazy val academicWordList:Set[NGram] = wordListFromJarPath("wordlists/west-lists/academic word list.txt")
  lazy val generalServiceList:Set[NGram] = wordListFromJarPath("wordlists/west-lists/general service list.txt")
  lazy val stopWords:Set[NGram] = wordListFromJarPath("wordlists/stopwords.txt")
  lazy val wordListByTopic:Map[String, Set[NGram]] = Map(
    "business" -> wordListFromJarPath("wordlists/west-lists/topic/business.txt"),
    "computer science" -> wordListFromJarPath("wordlists/west-lists/topic/computer science.txt"),
    "environmental science" -> wordListFromJarPath("wordlists/west-lists/topic/environmental science.txt"),
    "health science" -> wordListFromJarPath("wordlists/west-lists/topic/health science.txt"),
    "law" -> wordListFromJarPath("wordlists/west-lists/topic/law.txt"),
    "mathematics" -> wordListFromJarPath("wordlists/west-lists/topic/mathematics.txt"),
    "music" -> wordListFromJarPath("wordlists/west-lists/topic/music.txt"),
    "science and technology" -> wordListFromJarPath("wordlists/west-lists/topic/science and technology.txt")
  )

  lazy val memetrackerText:mutable.Map[String, MemetrackerUnit] = loadMemetracker
  private def loadMemetracker = {
    val st = System.currentTimeMillis
    val data = stringsFromFile("wordlists/memetracker_clusters.json").foldLeft(
        mutable.HashMap[String, MemetrackerUnit]())((acc, line) => {
      val clusterJson = Json.parse(line).asInstanceOf[Map[String, Any]]
      val phrasesJson = clusterJson("phrases").asInstanceOf[List[Map[String, Any]]]
      val phrases = phrasesJson.map((phraseJson) => {
        val phrase = MemetrackerPhrase(phraseJson("phrase").asInstanceOf[String],
                                       phraseJson("frequency").asInstanceOf[Int],
                                       phraseJson("daysCounts").asInstanceOf[Map[String, Int]])
        acc += (phrase.phrase -> phrase)
        phrase
      })
      val cluster = MemetrackerCluster(clusterJson("rootPhrase").asInstanceOf[String],
                                       clusterJson("totalFrequency").asInstanceOf[Int],
                                       phrases)
      acc += (cluster.rootPhrase -> cluster)
      phrases.foreach { (phrase) => phrase.cluster = cluster }
      acc
    })
    val decodeTime = (System.currentTimeMillis - st) / 1000.0
    log.info("Loaded memetracker data in %2.2f sec".format(decodeTime))
    data
  }

  val nytimesCommentsPerMonth = Map(
    "2007-10" -> 264,
    "2007-11" -> 6789,
    "2007-12" -> 11281,
    "2008-01" -> 13519,
    "2008-02" -> 24356,
    "2008-03" -> 18091,
    "2008-04" -> 17954,
    "2008-05" -> 19431,
    "2008-06" -> 21212,
    "2008-07" -> 22895,
    "2008-08" -> 24067,
    "2008-09" -> 66875,
    "2008-10" -> 55916,
    "2008-11" -> 49170,
    "2008-12" -> 44471,
    "2009-01" -> 48394,
    "2009-02" -> 50078,
    "2009-03" -> 59890,
    "2009-04" -> 53704,
    "2009-05" -> 51510,
    "2009-06" -> 48584,
    "2009-07" -> 52693,
    "2009-08" -> 55549,
    "2009-09" -> 50877,
    "2009-10" -> 67170,
    "2009-11" -> 71853,
    "2009-12" -> 102002,
    "2010-01" -> 128118,
    "2010-02" -> 118915,
    "2010-03" -> 138022,
    "2010-04" -> 124494,
    "2010-05" -> 115520,
    "2010-06" -> 123969,
    "2010-07" -> 115572,
    "2010-08" -> 119953,
    "2010-09" -> 125181,
    "2010-10" -> 19340)

  val nytimesNumPeopleCommentedPerMonth = Map(
    "2007-10" -> 247,
    "2007-11" -> 5526,
    "2007-12" -> 8989,
    "2008-01" -> 10335,
    "2008-02" -> 17632,
    "2008-03" -> 12759,
    "2008-04" -> 12543,
    "2008-05" -> 13178,
    "2008-06" -> 14019,
    "2008-07" -> 14445,
    "2008-08" -> 14470,
    "2008-09" -> 32182,
    "2008-10" -> 27823,
    "2008-11" -> 25828,
    "2008-12" -> 22769,
    "2009-01" -> 24112,
    "2009-02" -> 23183,
    "2009-03" -> 27965,
    "2009-04" -> 25329,
    "2009-05" -> 23822,
    "2009-06" -> 23149,
    "2009-07" -> 25576,
    "2009-08" -> 26673,
    "2009-09" -> 24398,
    "2009-10" -> 33439,
    "2009-11" -> 33489,
    "2009-12" -> 46487,
    "2010-01" -> 54873,
    "2010-02" -> 51814,
    "2010-03" -> 57663,
    "2010-04" -> 55296,
    "2010-05" -> 50649,
    "2010-06" -> 54457,
    "2010-07" -> 49819,
    "2010-08" -> 50884,
    "2010-09" -> 50627,
    "2010-10" -> 12131)

  lazy val phraseFinderMonthHistogram = loadPhraseFinderMonthHistogram
  private def loadPhraseFinderMonthHistogram = {
    val st = System.currentTimeMillis
    val data = stringsFromFile("wordlists/phrase-finder-month-histogram.json").foldLeft(
        mutable.HashMap[String, Map[String, Int]]())((acc, line) => {
      val s = Util.splitAtFirst(line, "\t")
      val phrase = s(0).replace("_", " ")
      val counts = Json.parse(s(1)).asInstanceOf[Map[String, Int]]
      acc += (phrase -> counts)
    })
    val decodeTime = (System.currentTimeMillis - st) / 1000.0
    log.info("Loaded phrase-finder month histogram data in %2.2f sec".format(decodeTime))
    data.toMap
  }
}

object Memetracker {
  def getInstanceListCountsInLongSequence(str:String):IntCounter[MemetrackerUnit] = {
    // TODO this is super inefficient.. some kind of traversing-radix solution would prob be better.
    val counter = new IntCounter[MemetrackerUnit]()
    NlpData.memetrackerText.foreach { case (phrase:String, unit:MemetrackerUnit) =>
      if (str.indexOf(phrase) >= 0) {
        counter.add(unit)
      }
    }
    counter
  }
}

abstract class MemetrackerUnit() {}

case class MemetrackerCluster(
    val rootPhrase:String, val totalFrequency:Int, val phrases:Seq[MemetrackerPhrase]) extends MemetrackerUnit

case class MemetrackerPhrase(val phrase:String, val frequency:Int, val dayCounts:Map[String, Int])
extends MemetrackerUnit {
  var cluster:MemetrackerCluster = null
  lazy val monthCounts = calcMonthCounts
  def calcMonthCounts():IntCounter[String] = {
    val counts = new IntCounter[String]
    dayCounts.foreach { case (day, count) => counts.add(day.split("-").take(2).mkString("-"), count) }
    return counts
  }
}
