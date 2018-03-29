package segcala

import collection.mutable.{ListBuffer, Queue}

/**
 * Created by IntelliJ IDEA.
 * User: rockmaple(At)gmail.com
 */

object Algorithm {
  object Constants {
    val MAX_WORD_NO = 3
  }

  def seg(fragment: TextFragment): Chunk = {
    val chunks = createChunks(fragment.data, fragment.offset)
    if (chunks.length > 0) {
      val chunk = applyRules(chunks)
      fragment.offset += chunk.length
      chunk
    } else {
      throw new Exception("no chunk found.")
    }
  }

  private def createChunks(fragment: List[Char], offset: Int): List[Chunk] = {
    var chunks: ListBuffer[Chunk] = new ListBuffer()
    var words: Array[Word] = new Array(3)

    findMatches(fragment, offset, chunks, words, 0)
    chunks.toList
  }


  private def applyRules(chunks: List[Chunk]): Chunk = {

    var tmpChunks = Rules.maxMatchRule(chunks)

    if (tmpChunks.length > 1) {
      tmpChunks = Rules.largestAvgWordLenRule(chunks)
      if (tmpChunks.length > 1) {
        tmpChunks = Rules.smallestVarianceRule(chunks)
        if (tmpChunks.length > 1) {
          tmpChunks = Rules.largestSumMorphemicFreedomDegreeRule(chunks)
        }
      }
    }

    tmpChunks(0)
  }

  private def findMatches(fragment: List[Char], offset: Int, chunks: ListBuffer[Chunk], path: Array[Word], len: Int) {

    if(len >= Constants.MAX_WORD_NO || offset >= fragment.length){
      var l: List[Word] = List()
      for(i <- 0 to len - 1){
          l = path(i) :: l
      }
      val chunk = new Chunk(l)
      chunks.append(chunk)
    }else{
      val words = Dict.findMatchWords(fragment, offset)
      words.foreach(w => {
        path(len) = w
        findMatches(fragment, offset + w.length, chunks, path, len+1)
      })
    }
    
  }

  object Rules {
    def largestAvgWordLenRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.averageLength > c2.averageLength) c1 else c2})
      chunks.filter(chunk => (chunk.averageLength == c.averageLength))
    }

    def maxMatchRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.length > c2.length) c1 else c2})
      chunks.filter(chunk => (chunk.length == c.length))
    }

    def smallestVarianceRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.variance < c2.variance) c1 else c2})
      chunks.filter(chunk => (chunk.variance == c.variance))
    }

    def largestSumMorphemicFreedomDegreeRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.degreeOfMorphemicFreedom > c2.degreeOfMorphemicFreedom) c1 else c2})
      chunks.filter(chunk => (chunk.degreeOfMorphemicFreedom == c.degreeOfMorphemicFreedom))
    }

  }
}
