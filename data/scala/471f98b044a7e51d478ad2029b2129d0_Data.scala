package edu.mq.itec814
import scala.util.Random

case object Stop

case class Data (val a : Int = 0, val b : Int = 0) {

  override def toString = "(" + a.toString + ", " + b.toString + ")"  
  
}

object Data {
  
	var lists : IndexedSeq[Data] = IndexedSeq[Data]()
	
	def apply (n : Int) = {
    val r = new Random
    lists = for { i <- 0 to n } yield new Data ( r.nextInt(1000), r.nextInt(1000) )
  }
	
  def getList : List[Data] = lists.toList
  
  def getArray : Array[Data] = lists.toArray
  
}