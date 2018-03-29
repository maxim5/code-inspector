/*
 * Copyright 2011 David Crosson
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
 */

package fr.janalyse.series

import java.io.File
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import fr.janalyse.series._
import fr.janalyse.unittools._
import fr.janalyse.tools.DurationTools._

/**
  * Test cases
  * @author David Crosson
  */
@RunWith(classOf[JUnitRunner])
class SeriesTest  extends FunSuite with ShouldMatchers {

  // ---------------------------------------------------------------------------
  test("Empty test") {
    val series = Series[StatCell]("x")
    series.size should equal(0)
    series.stat.sum   should equal(0)
    series.stat.max   should equal(0)
    series.stat.min   should equal(0)
    series.stat.avg   should equal(0)
    series.stat.count should equal(0)
  }
  // ---------------------------------------------------------------------------
  test("StatCell test") {
    val series = Series[StatCell]("x")  << 1-> 10 << 1->5 << 1->15
    val StatCell(time, value, count, min, max, sum) = series(0)
    time  should equal(1)
    value should equal(10)
    count should equal(3)
    min   should equal(5)
    max   should equal(15)
    sum   should equal(30)
  }

  // ---------------------------------------------------------------------------
  
  test("AddContent series test") {
    var s = Series[AddCell]("A", 1L)
    s = s << 1L->10d << 1L->20d << 2L->2d << 2L->4d << 2L->8d << 2L->6d
    s should have size (2)
    s map {_.value} should equal(List(30d, 20d))

    var s2 = s / 2 + 1
    s2 map {_.value} should equal(List(16d, 11d))
  }
  // ---------------------------------------------------------------------------
  
  test("AddContent series test (variant)") {
    val s = Series[AddCell]("A", 1L) <<< List(1->10, 1->20, 2->2, 2->4 , 2->8, 2->6)
    s.values should equal(List(30d, 20d))

    val s2 = s / 2 + 1
    s2.values should equal(List(16d, 11d))
  }

  // ---------------------------------------------------------------------------

  test("Basic") {
    val csv="  date        \t time     \t A    \n"+
            "   2009/03/31 \t 02:00:00 \t 30.0 \n"
    val seriesList = CSV2Series.fromString(csv).values
    seriesList.size should equal(1)
  }

  // ---------------------------------------------------------------------------

  test("First") {
    val csv="""date, A
              |2009/03/31 02:00:00.100, 2
              |2009/03/31 03:00:00.100, 6
              |2009/03/31 04:00:00.100, 10""".stripMargin
    val seriesList = CSV2Series.fromString(csv).values
    seriesList.size should equal(1)
  }
  
  // ---------------------------------------------------------------------------

  test("Second") {
    val csv="""date; A
              |2009/03/31 02:00:00; 2
              |2009/03/31 03:00:00; 6
              |2009/03/31 04:00:00; 10""".stripMargin
    val seriesList = CSV2Series.fromString(csv).values
    seriesList.size should equal(1)
    seriesList.head.stat.sum should equal(18)
  }

  // ---------------------------------------------------------------------------

  test("bis") {
    val csv="""date; A
              |20-03-2011 02:00; 2
              |20-03-2011 03:00; 6
              |20-03-2011 04:00; 10""".stripMargin
    val seriesList = CSV2Series.fromString(csv).values
    seriesList.size should equal(1)
    seriesList.head.stat.sum should equal(18)
  }

  // ---------------------------------------------------------------------------

  test("empty cell") {
    val csv="""date; A
              |20-03-2011 02:00; 2
              |20-03-2011 03:00;
              |20-03-2011 04:00; 10""".stripMargin
    val seriesList = CSV2Series.fromString(csv).values
    seriesList.size should equal(1)
    seriesList.head.stat.count should equal(2)
    seriesList.head.stat.sum should equal(12)
  }


  // ---------------------------------------------------------------------------

  test("more-complex") {
    val csv="""  date ;  name ;   X
              |  2009/03/31 02:00:00 ; A; 2
              |2009/03/31 02:00:00 ; B; 2
              |2009/03/31 03:00:00; A; 6
              | 2009/03/31 03:00:00; B; 6
              |2009/03/31 04:00:00; A ; 10
              |2009/03/31 04:00:00; B ; 10
              |
              |""".stripMargin
    val seriesMap = CSV2Series.fromString(csv)
    seriesMap.size should equal(2)
    val ax=seriesMap("A-X")
    val bx=seriesMap("B-X")
    ax.stat.sum should equal(18)
    bx.stat.sum should equal(18)
  }
  
  // ---------------------------------------------------------------------------
  test("CSV date format test") {
    import java.util.{GregorianCalendar, Calendar, Date}
    import Calendar._
    def cal(t:Long) = {
      val c=new GregorianCalendar
      c.setTime(new Date(t))
      c
    }
    val daycheck = {(t:Long) => cal(t).get(DAY_OF_MONTH) should equal(20)}
    val monthcheck = {(t:Long) => cal(t).get(MONTH) should equal(MARCH)}
    val yearcheck = {(t:Long) => cal(t).get(YEAR) should equal(2011)}
    val hourcheck = {(t:Long) => cal(t).get(HOUR_OF_DAY) should equal(17)}
    val minutecheck = {(t:Long) => cal(t).get(MINUTE) should equal(27)}
    val secondcheck = {(t:Long) => cal(t).get(SECOND) should equal(37)}
    val millicheck =  {(t:Long) => cal(t).get(MILLISECOND) should equal(456)}
    val dates2test=List(
       "2011-03-20" -> List(daycheck,monthcheck,yearcheck), 
       "20-03-2011" -> List(daycheck,monthcheck,yearcheck),
       "2011_03_20 17:27" -> List(daycheck,monthcheck,yearcheck,hourcheck,minutecheck),
       "20-03-2011 17:27:37" -> List(daycheck,monthcheck,yearcheck,hourcheck,minutecheck,secondcheck),
       "20/03/2011 17:27:37.456" -> List(daycheck,monthcheck,yearcheck,hourcheck,minutecheck,secondcheck,millicheck),
       "17:27" -> List(hourcheck,minutecheck),
       "17:27:37" -> List(hourcheck,minutecheck, secondcheck),
       "17:27:37.456" -> List(hourcheck,minutecheck, secondcheck, millicheck)
    )
    dates2test foreach {case (d2t, checks)=>
      val csv="date;x\n%s;123".format(d2t)
      info("Checking %s".format(d2t))
      CSV2Series.fromString(csv).get("x") match {
        case None =>
        case Some(x) => 
          x.size should equal(1)
          x.values should equal(List(123))
          checks foreach {chk => chk(x.head.time)}
      }
    }
  }
  
  // ---------------------------------------------------------------------------
  test("sample1") {
    val seriesMap = CSV2Series.fromFile("samples/1.csv")
    seriesMap.size should equal(1)
    val (name, memUsage) = seriesMap.head
    memUsage.size should equal(3)
    memUsage.max.value  should equal(1.99d)
    memUsage.min.value  should equal(0.18d)
    memUsage.stat.sum   should equal(3d)
    memUsage.stat.max   should equal(1.99d)
    memUsage.stat.min   should equal(0.18d)
    memUsage.stat.avg   should equal(1d)
    memUsage.stat.count should equal(3L)
  }

  // ---------------------------------------------------------------------------
  test("sample2") {
    val seriesMap = CSV2Series.fromFile("samples/2.csv")
    seriesMap.size should equal(9)
    val okSeries = seriesMap("www status 200 hit count")
    okSeries.size            should equal(255)
    okSeries.max.value       should equal(6845d)
    okSeries.min.value       should equal(1d)
    okSeries.stat.sum        should equal(154275d)
    okSeries.stat.max        should equal(6845d)
    okSeries.stat.min        should equal(1d)
    okSeries.stat.avg.floor  should equal(605d)
    okSeries.stat.count      should equal(255d)
    okSeries.stat.open       should equal(1d)
    okSeries.stat.close      should equal(260d)
  }
  
  // ---------------------------------------------------------------------------  
  test("stats with standard deviation") {
    val seriesMap = CSV2Series.fromFile("samples/2.csv")
    seriesMap.size should equal(9)
    val okSeries = seriesMap("www status 200 hit count")
    okSeries.stat.sd.floor   should equal(655d)
  }
  
  // ---------------------------------------------------------------------------  
  test("series read write 1") {
    val csv="""DateTime;A
              |2011/03/20 02:00:00;2
              |2011/03/20 03:00:00;6
              |2011/03/20 04:00:00;10
              |""".stripMargin
    val seriesList = CSV2Series.fromString(csv).values.toList
    seriesList.head.toList.map {_.value} should equal (List(2d,6d,10d))
  }

  // ---------------------------------------------------------------------------  
  test("series read write 2") {
    val csv="""DateTime;A; B; C
              |2011/03/20 02:00:00;2; ; 3
              |2011/03/20 03:00:00;6; ;
              |2011/03/20 04:00:00;10; 9; 8
              |""".stripMargin
    val seriesMap = CSV2Series.fromString(csv)

    seriesMap.get("A").get.toList.map {_.value} should equal (List(2d,6d,10d))
    seriesMap.get("B").get.toList.map {_.value} should equal (List(9d))
    seriesMap.get("C").get.toList.map {_.value} should equal (List(3d,8d))
  }
 
  
  // ---------------------------------------------------------------------------  
  test("Check the behavior with cell mutations : CountCell to CalcCell") {
      var w:Series[Cell] = Series[CountCell]("W") << 1->0
      
      w <<= 1->0
      w <<= 1->0
      w = w + 0
      
      w(0) should equal(CalcCell(1,3))
  }

  // ---------------------------------------------------------------------------  
  test("usage test 1") {
      var x = Series[CalcCell]("X")
      var y = Series[CountCell]("Y")

      x <<= CalcCell(1L, 10d)
      x <<= CalcCell(5L, 15)
      x <<= CalcCell(3L, 2)
      x <<= CalcCell(5, 8)
      x <<= 3->20

      x = x*2

      y <<= CountCell(3,20)
      y <<= CountCell(4,18)
      y <<= CountCell(4,2)

      val z:Series[CalcCell] = x<<<y

      val mySeries = List(x,y)

      mySeries match {
        case x:List[Series[Cell]] => 
        case _ => fail("Wrong type returned")
      }
  }

  // ---------------------------------------------------------------------------  
  test("more usage tests") {
    var series = Series[StatCell]("A", 1L)
    series = series << 1L->10d << 1L->20d << 2L->2d << 2L->4d << 2L->8d << 2L->6d
    info(series.toString)

    var seriesMax = series.extract(_.max)
    info(seriesMax.toString)
    seriesMax.values should equal(List(20, 8))

    var s = Series[AddCell]("B",10L)
    s = s << 1L->10d << 2L->20d << 10L->1d << 19L->5d
    var s2 = (s <<< s) << 22L->100d
    info(s2.toString)

    var s3 = s2 * 2 + 10
    info(s3.toString)
    info("max=%s min=%s sum=%f".format(s3.max, s3.min, 0d /*s3.sum*/))
    info("max=%s min=%s sum=%f".format(s3.stat.max, s3.stat.min, 0d/*s3.sum*/))

    s3.max.value should equal(s3.stat.max)
    s3.min.value should equal(s3.stat.min)
    
    s3 = -s3 / 2
    info(s3.toString)
  }
  
  // ---------------------------------------------------------------------------  
  test("output format ketchup, euhhhh, checkup") {
    var series = Series[StatCell]("A", 1L)
    series = series << 1L->10d << 1L->20d << 2L->2d << 2L->4d << 2L->8d << 2L->6d
    
    CSV2Series.toString(series) should include (";")
    
    val fmt = CSVFormat(separator="\t", dateTimeFormatPattern=None)

    CSV2Series.toString(series, fmt) should include ("\t")

    info(CSV2Series.toString(series))
    info(CSV2Series.toString(series, fmt))
  }
  
  // ---------------------------------------------------------------------------  
  test("some performances test") {
    var x = Series[CalcCell]("X", "5s")

    val (buildDuration, _) = durationAsDesc { (1 to 10000) foreach {i => x<<=CalcCell(i*10,i)} }
    info("Build duration : %s - size=%d".format(buildDuration, x.size))

    val (computeDuration, _) = durationAsDesc { (1 to 10) foreach {i =>  x += i.toDouble } }
    info("compute duration : %s - size=%d".format(computeDuration, x.size))

    val (compositionDuration, _) = durationAsDesc { (1 to 100) foreach {i =>  x<<<=x } }
    info("composition duration : %s - size=%d".format(compositionDuration, x.size))

    val (betterCompositionDuration, _) = durationAsDesc { x<<<= ((1 to 100) map {_ => x}).toList }
    info("better composition duration : %s - size=%d".format(betterCompositionDuration, x.size))
  }
 
  
  // ---------------------------------------------------------------------------  
  test("more performances test - 1.000.000 cells") {
    val i1 = howlong(3L) {()=>
      var series = Series[StatCell]("X", 1000L)
      (1L to 1000000L) foreach {t=> series <<=  t -> 10d}
    }
    
    info(i1)

    val i2 = howlong(3L) {() =>
      var series = Series[AddCell]("Y", 1000L)
      (1L to 1000000L) foreach {t => series <<= t->2d}
    }
    
    info(i2)
  }
  
  // ---------------------------------------------------------------------------
  test("Checking cell type conversion #1") {
    var w:Series[Cell] = Series[CountCell]("W") << 1->0
    w <<= 1->0
    w <<= 1->0
    w = w + 0  // To check the behavior because here the CountCell become a CalcCell
    w.size should equal(1)
    w.values should equal(List(3))
    assert(w(0).isInstanceOf[CalcCell])
  }
  
  // ---------------------------------------------------------------------------
  test("Checking cell type conversion #2") {
    val x=Series[StatCell]("x") << 1L->1 << 2L->2
    val y =Series[StatCell]("y") << 1L->4 << 2L->10
    
    val z=Series[AddCell]("z") <<< List(x,y)
    z(0) should equal (AddCell(1,5d))
    z(1) should equal (AddCell(2,12d))
  }
  // ---------------------------------------------------------------------------
  test("delta & cumulate") {
    val x=Series[CalcCell]("x") <<< List(1->10, 2-> 15, 3->20)
    val d=x.delta
    val c=x.cumulate
    
    d.values should equal(List(5d,5d))
    d.times should equal(List(2L,3L))
    
    c.values should equal(List(10,25,45))
    c.times should equal(List(1,2,3))
  }
  // ---------------------------------------------------------------------------
  test("compact") {
    val data=List(10, 10, 20, 10, 10, 10, 30, 30, 20, 20, 20).zipWithIndex map {case (x,y) => y->x}
    val x=Series[CalcCell]("x") <<< data
    val cx=x.compact
    
    cx.values should equal(List(10,20,10,30,20))
    cx.times should equal(List(0,2,3,6,8))
  }
  
  // ---------------------------------------------------------------------------
  test("using foldleft") {
    val i = howlong(3L) { () =>
      var series = Series[AddCell]("Y", 1000L)
      (1L to 1000000L) foreach {t => series <<= t->2d}
      val sum = ((0d /: series) {(sum, tup) => sum + tup.value})
      sum should equal(2000000d)
    }
    info(i)
  }

  // ---------------------------------------------------------------------------
  test("UnikSeries") {
    case class FirstName(name:String)
    var unik = UnikSeriesMaker[FirstName]("Unik first names", 10)
    unik = unik.manage(1, FirstName("Toto"))
    unik = unik.manage(1, FirstName("Toto"))
    unik = unik.manage(1, FirstName("Tata"))
    unik = unik.manage(11, FirstName("Tata"))
    unik = unik.manage(12, FirstName("Tata"))
    unik.series.values should equal (List(2,1))
  }
  // ---------------------------------------------------------------------------
  test("BusySeriesMaker (Parallel tasks) tests") {
    var mk = BusySeriesMaker("workers", 10L)
    
    def toTuples = (mk.series.times zip mk.series.values)
    
    mk = mk.manage(15,15)
    mk.series should have size(2)
    toTuples should equal(List( 10->0.5d, 20->1d))

    mk = mk.manage(12,5)
    mk.series should have size(2)
    toTuples should equal(List( 10->1d, 20->1d))
    
    mk = mk.manage(5,35)
    mk.series should have size(4)
    toTuples should equal(List(0->0.5d, 10->2d, 20->2d, 30->1d))
    
    mk = mk.manage(18,10)
    mk.series should have size(4)
    toTuples should equal(List(0->0.5d, 10->2.2d, 20->2.8d, 30->1d))
    
    mk = mk.manage(25,2)
    mk.series should have size(4)
    toTuples should equal(List(0->0.5d, 10->2.2d, 20->3d, 30->1d))
    
  }
  // ---------------------------------------------------------------------------
  ignore("load series from web - use case") {
    val allSeries = CSV2Series.fromURL("http://ichart.finance.yahoo.com/table.csv?s=GOOG")
    val closeSeries = allSeries("Close")
    info("GOOGLE stock summary")
    // Get higher quote value
    info("Higher : "+closeSeries.max)
    // Get lowest quote value
    info("Lowest : "+closeSeries.min)
    // Get overall quote trend
    info("Week Trend : "+closeSeries.stat.linearApproximation.daySlope*7)
    // Latest quote close value
    info("Latest : "+closeSeries.last)
  }

  // ---------------------------------------------------------------------------
  def seriesToTuples(mk:DistributionSeriesMaker) = (mk.series.times zip mk.series.values)
  def seriesToLongTuples(mk:DistributionSeriesMaker) = (mk.series.times zip mk.series.values.map(_.toLong))
  // ---------------------------------------------------------------------------
  test("DistributionSeriesMaker tests 1") {
    var mk = DistributionSeriesMaker("xyz", 10L)
	mk = mk.manage(10,20,1000) // 1000 ŕ distribuer sur 20ms
	mk.series should have size(2)
    seriesToTuples(mk) should equal(List(10->500, 20->500))
    mk = mk.manage(8,10,100)  // 100 ŕ distribuer sur 10ms
    mk.series should have size(3)
    seriesToTuples(mk) should equal(List(0->20, 10->580, 20->500))
  }
  // ---------------------------------------------------------------------------
  test("DistributionSeriesMaker tests 2") {
    var mk = DistributionSeriesMaker("xyz", "60s")	
	mk = mk.manage(120000,130000d,12288) // 12288 ŕ distribuer sur 130s
	mk.series should have size(3)
    seriesToLongTuples(mk) should equal(List(120000->5671,  180000->5671, 240000->945))    
  }
  // ---------------------------------------------------------------------------
  test("DistributionSeriesMaker tests 3") {
    var mk = DistributionSeriesMaker("xyz", "60s")
	mk = mk.manage(110000,130000d,12288) // 12288 ŕ distribuer sur 130s
	mk.series should have size(3)
    seriesToLongTuples(mk) should equal(List(60000->945, 120000->5671,  180000->5671))
  }
  // ---------------------------------------------------------------------------
  test("DistributionSeriesMaker tests 4") {
    var mk = DistributionSeriesMaker("xyz", "60s")
	mk = mk.manage(110000,150000d,987654321) // 987654321 ŕ distribuer sur 150s
	mk.series should have size(4)
    seriesToLongTuples(mk) should equal(List(60000->65843621, 120000->395061728, 180000->395061728, 240000->131687242))
  }
  // ---------------------------------------------------------------------------
  test("howlongFor tests") {
    val series = Series[StatCell]("x")  <<< 
    		List(10 -> 10,
    		     20 -> 5,
    		     30 -> 5,
    		     40 -> 10,
    		     50 -> 10,
    		     55 -> 10,
    		     60 -> 4
    		    )
    series.howlongFor(_>5) should equal(15)
    series.howlongFor(_>4) should equal(45)
  }
  
  // ---------------------------------------------------------------------------
  
}

