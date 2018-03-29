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

object Phrases {
  // implicit def ngramify(target:Map[String, Map[String, String]]):Map[NGram, Map[String, NGram]] = target.map((tup) => {
  //   val ngramValues:Map[String, NGram] = tup._2.map((vtup) => (vtup._1, NGram(vtup._2))).toMap
  //   (NGram(tup._1), ngramValues)
  // }).toMap

  // val toBe:Map[NGram, Map[String, NGram]] = ngramify(Map(
  //   "he" -> Map("present" -> "is", "past" -> "was", "future" -> "will be"),
  //   "she" -> Map("present" -> "is", "past" -> "was", "future" -> "will be"),
  //   "i" -> Map("present" -> "am", "past" -> "was", "future" -> "will be"),
  //   "you" -> Map("present" -> "are", "past" -> "was", "future" -> "will be"),
  //   "we" -> Map("present" -> "are", "past" -> "was", "future" -> "will be")))

  val toBe = List(
    "hes", "he is", "he was", "he will be",
    "shes", "she is", "she was", "she will be",
    "im", "i am", "i was", "i will be",
    "youre", "you are", "you was", "you will be",
    "we are", "we were", "we will be").map((x:String) => NGram(x)).toSet

  val sides = List(
    "republican", "democrat", "independent voters", "independent voter", "tea party", "tea partier",
     "marxist", "marxists", "capitalists", "communists", "socialists").map((x:String) => NGram(x)).toSet

  val cultures = List(
    // regions
    "american", "european", "asian", "north american", "south american", "central american",
    "east asian", "south asian", "southeast asian", "western asian", "eurasian", "oceanian",
    "eastern european", "western european", "central american", "middle eastern", "middle east",
    // g20++
    "african", "south african", "canandian", "mexican", "argentinian", "brazillian",
    "chinese", "japanese", "south korean", "north korean", "korean", "indian",
    "indonesian", "saudi arabian", "russian", "turkish", "french", "german", "italian",
    "british", "english", "brazilian", "welsh", "irish", "scotish", "spanish", "romanian",
    "dutch", "danish", "swedish", "finnish", "nigerian", "united states of america", "united states",
    "palestine", "palestinian", "israel", "israeli", "iran", "iranian", "persian",
    // subcultures (http://en.wikipedia.org/wiki/List_of_subcultures)
    "hippy", "punk", "beatnik", "bdsm", "bohemian", "cosplayer", "cybergoth", "deaf",
    "emo", "gothic", "greaser", "rocker", "hacker", "rapper", "black culture", "urban culture",
    "hippie", "hipster", "nazi", "nudist", "preppy", "queer", "raver", "rockabilly", "scater",
    "steampunk", "swinger", "trekkie").map((x:String) => NGram(x)).toSet

  val issues = List(
    "abortion", "gays", "homosexual", "religion", "spending", "economy", "resession",
    "crime", "drugs", "education", "energy", "environment", "foreign policy",
    "iraq", "afghanistan", "gun control", "health care", "immigration", "jobs",
    "family values", "gay marriage", "gays in the military", "social security",
    "welfare", "poverty", "corporate america", "communism", "capitalism",
    "lehman brothers", "lehman bros", "aig", "enron", "bear sterns",
    "wall street", "main street", "stock market", "deficit", "debt",
    "the country", "the streets", "real estate", "us government",
    "islamic", "islamist", "muslum", "muslim", "musloem",
    "armed forces", "clean energy", "going green", "green tech",
    "constitution", "consitutional", "united nations",
    "foreign", "foreigner", "foreigners", "xenophobic", "xenophobia",
    "terrorism", "terrorist", "terrorist attack",
    "offshore drilling", "community organizer", "acorn", "common man", "homeless",
    "jesus", "mohammad", "muhammad", "god", "hospitals", "mass media", "fox news",
    "abc news", "nbc news", "wallstreet journal", "wsj", "slavery", "piracy", "rape",
    "creationism", "evolution", "global warming", "schools", "sat", "gre", "testing in schools",
    "for the people", "rural america", "farmers", "corn farmers", "industry",
    "assisted suicide", "pedophiles", "pedophile", "pedophilia", "priests", "the system",
    "child abuse", "wife beater", "adultery", "adulterer", "sodomite",
    "prison", "prisons", "prisoner", "budget", "budgetary", "freedom fighter",
    "poor people", "people at the bottom", "privacy", "social media",
    "taxes", "the election", "post office", "postal service", "social security",
    "bp", "oil spill", "gulf").map((x:String) => NGram(x)).toSet

  val expressions = List(
    "i cannot believe", "i can't believe", "i cant believe",
    "i couldn't believe", "i could not believe",
    "i fear", "i'm scared", "im scared",
    "this country is", "this country has", "this country will",
    "out of control", "it's out of control", "its out of control",
    "why are they", "what are they trying",
    "its idiotic that", "it's idiotic that",
    "its stupid that", "it's stupid that",
    "its wonderful that", "it's wonderful that",
    "its crazy that", "it's crazy that",
    "it's amazing that", "its amazing that",
    "how great is", "how great was", "how great are",
    "i just want", "i just wanted").map((x:String) => NGram(x)).toSet

// Think about implementing actual sides
//  (List("you are right", "youre right"), List("you are wrong", "youre wrong"),
  val takingSides = List(
    "you are right", "youre right", "you are wrong", "youre wrong",
    "imho", "in my humble opinion", "i think", "you think",
    "its wrong to", "its right to",
    "why would i", "why would you", "why would one", "why would people",
    "sky is not falling", "the bottom line is", "the smoking gun",
    "we have a right", "i love", "i hate").map((x:String) => NGram(x)).toSet


  // Fortune 50
  val companies = List(
    "Wal-Mart", "Exxon Mobil", "Chevron", "General Electric", "Bank of America",
    "ConocoPhillips", "AT&T", "Ford Motor", "J.P. Morgan Chase", "JP Morgan", "Hewlett-Packard",
    "Berkshire Hathaway", "Citigroup", "Verizon Communications", "McKesson", "General Motors",
    "American International Group", "AIG", "Cardinal Health", "CVS Caremark", "Wells Fargo",
    "International Business Machines", "IBM", "UnitedHealth Group", "Procter & Gamble",
    "Procter Gamble", "Procter and Gamble", "Kroger", "AmerisourceBergen", "Costco Wholesale",
    "Valero Energy", "Archer Daniels Midland", "Boeing", "Home Depot", "Target", "WellPoint",
    "Walgreen", "Johnson & Johnson", "Johnson and Johnson", "Johnson Johnson", "State Farm Insurance",
    "Medco Health Solutions", "Microsoft", "United Technologies", "Dell", "Goldman Sachs Group",
    "Pfizer", "Marathon Oil", "Lowes", "United Parcel Service", "UPS", "Lockheed Martin",
    "Best Buy", "Dow", "Supervalu", "Sears", "International Assets Holding", "Pepsi",
    "PepsiCo", "MetLife", "Safeway", "Kraft", "Freddie Mac", "Sysco", "Apple", "Walt Disney",
    "Disney", "Cisco", "Comcast", "FedEx", "Northrop Grumman", "Intel", "Aetna",
    "New York Life Insurance", "Prudential Financial", "Caterpillar", "Sprint Nextel",
    "Allstate", "General Dynamics", "Morgan Stanley", "Liberty Mutual Insurance", "Coca-Cola",
    "Coca Cola", "Humana", "Honeywell International", "Abbott Laboratories", "News Corp", "HCA",
    "Sunoco", "Hess", "Ingram Micro", "Fannie Mae", "Time Warner", "Johnson Controls",
    "Delta Air Lines", "Merck", "DuPont", "Tyson Foods", "American Express", "Rite Aid",
    "TIAA-CREF", "CHS", "Enterprise GP Holdings", "Massachusetts Mutual Life Insurance",
    "Philip Morris International", "Raytheon", "Express Scripts", "Hartford Financial Services",
    "Travelers Cos", "Publix Super Markets", "Amazon.com", "facebook",
    "twitter", "google", "microsoft", "apple", "iphone", "ipod").map((x:String) => NGram(x)).toSet
}

object PhraseFinder extends Logging {
  def findPhrases(tokens:Seq[String]) = Map(
      "toBe" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.toBe),
      "sides" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.sides),
      "issues" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.issues),
      "cultures" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.cultures),
      "expressions" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.expressions),
      "takingSides" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.takingSides),
      "companies" -> NGram.getNGramListCountsInLongSequence(tokens, Phrases.companies),
      "famousPeople" -> NGram.getNGramListCountsInLongSequence(tokens, NlpData.famousPeople))

  private def _getNGramListCountsInLongSequenceAcrossSentences(sentences:Seq[String], target:Set[NGram]):IntCounter[NGram] = {
    sentences.map((sentence) => NGram.getNGramListCountsInLongString(sentence, target))
             .reduceLeft((first, last) => { first += last
                                            first })
  }

  def findPhrasesInUntokenizedSentences(sentences:Seq[String]) = Map(
      "toBe" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.toBe),
      "sides" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.sides),
      "issues" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.issues),
      "cultures" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.cultures),
      "expressions" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.expressions),
      "takingSides" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.takingSides),
      "companies" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, Phrases.companies),
      "famousPeople" -> _getNGramListCountsInLongSequenceAcrossSentences(sentences, NlpData.famousPeople))

  def findMemesInUntokenizedSentence(comment:String):IntCounter[MemetrackerUnit] = {
    Memetracker.getInstanceListCountsInLongSequence(comment)
  }

  def findMemesInUntokenizedSentences(sentences:Seq[String]):IntCounter[MemetrackerUnit] =
    sentences.map((sentence) => Memetracker.getInstanceListCountsInLongSequence(sentence))
             .reduceLeft((first, last) => { first += last
                                            first })
}
