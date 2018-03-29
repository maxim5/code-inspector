package razie.assets.test

//import com.razie.pub.agent._
//import com.razie.pub.agent.test._
//import com.razie.agent._
//import com.razie.assets._
import com.razie.pub.base.data._
import com.razie.pub.base._
import com.razie.pub.base.log._
import com.razie.pub.comms._
import com.razie.pub.lightsoa._
import com.razie.pub.assets._
//import com.razie.pub.actionables._
//import com.razie.pub.actionables.library._
import org.scalatest.junit._
import razie._
import razie.assets._

object S {
   val (p, c1, c2) = (new TestParentC("p1"), new TestChildC("c1") , new TestChildC("c2"))
}

/** 
 * this is a blank asset - use it any way you see fit, to simplify testing - find its uses in our tests
 */
@SoaAsset(descr = "parent with reflected child", bindings=Array("http"))
class TestParentC (key:String) extends BlankAsset ("TestParentC", key, AA("kuku", "p1c")) with HasMeta {
   override def metaSpec = new MetaSpec (new Meta (AI cmdicon("TestParentC", "/public/pics/web.png"), null),
         List(new MetaAssoc("pcc", "TestParentC", "TestChildC", "composition", "1-*", "parent", "children")))
   def children = List (S.c1, S.c2)
}
   
/** 
 * this is a blank asset - use it any way you see fit, to simplify testing - find its uses in our tests
 */
@SoaAsset(descr = "parent with reflected child", bindings=Array("http"))
class TestChildC(key:String) extends BlankAsset ("TestChildC", key, null) with HasMeta {
   def parent = S.p
}
   
/** test asset definition samples, complete with bindings */
class TestXpAssets extends JUnit3Suite {

   override def setUp = {
      ExecutionContext.resetJVM
         AssetMgr.init (new InventoryAssetMgr)
      
         razie.Assets.manage(new BlankAsset ("blankie1", "first",  razie.AA("kuku", "kiki1")))
         razie.Assets.manage(new BlankAsset ("blankie1", "second", razie.AA("kuku", "kiki2")))
      
      // association stuff
         razie.Assets manage new BlankAsset ("TestParentB", "p1", razie.AA("kuku", "p1"))
         razie.Assets manage new BlankAsset ("TestChildB", "c1", razie.AA("kuku", "c1"))
         razie.Assets manage new BlankAsset ("TestChildB", "c2", razie.AA("kuku", "c2"))
         razie.Assets manage new BlankAsset ("TestFriendB", "f1", razie.AA("kuku", "f1"))
         razie.Assets manage new BlankAsset ("TestFriendB", "f2", razie.AA("kuku", "f2"))
         
         razie.Metas.addAssoc (new MetaAssoc("c", "TestParentB", "TestChildB", "composition", "1-*"))
         razie.Metas.addAssoc (new MetaAssoc("a", "TestChildB", "TestFriendB", "association", "*"))
         
         razie.Assets.associate (razie.Asset("TestParentB:p1"), razie.Asset("TestChildB:c1"))
         razie.Assets.associate (razie.Asset("TestParentB:p1"), razie.Asset("TestChildB:c2"))
         razie.Assets.associate (razie.Asset("TestChildB:c1"), razie.Asset("TestFriendB:f1"))
         razie.Assets.associate (razie.Asset("TestChildB:c2"), razie.Asset("TestFriendB:f2"))
         
         List (S.p, S.c1, S.c2).foreach(razie.Assets.manage(_))
//      } 
      }

   def testSimpleAsset = expect (2) {
      (Assets xpl ("/blankie1")).size 
   }

   def testSimpleCond = expect (Some("kiki1")) {
      val r = Assets xpe ("/blankie1[@kuku=='kiki1']")
      razie.AA.sa(r, "kuku")
   }

   def testAllCond = {
      expect ("kiki1") { Assets xpa ("/blankie1[@kuku!='kiki2']/@kuku") }
      expect ("kiki1") { Assets xpa ("/blankie1[@kuku~='kiki1']/@kuku") }
      expect ("kiki1") { Assets xpa ("/blankie1[@kuku=='kiki1']/@kuku") }
      expect ("kiki1") { Assets xpa ("/blankie1[@kuku == 'kiki1']/@kuku") }
      expect ("kiki1") { Assets xpa ("/blankie1[kuku=='kiki1']/@kuku") }
      expect ("kiki1") { Assets xpa ("/blankie1[kuku==kiki1]/@kuku") }
   }

   def testAttr = expect ("kiki1") { Assets xpa ("/blankie1[@kuku=='kiki1']/@kuku") }
  
   // key is a reserved attribute
   def testAttrKey = expect ("first") { Assets xpa ("/blankie1[key==first]/@key") }

   def testassoc1JustTestxpl = expect (2) { (Assets xpl "/TestParentB/TestChildB").size }
   def testassoc1 = expect (List("c1", "c2")) { Assets xpla "/TestParentB/TestChildB/@kuku" }
   def testassoc1r = expect (List("p1", "p1")) { Assets xpla "/TestChildB/TestParentB/@kuku" }
   def testassoc1a = expect (List("c1", "c2")) { Assets xpla "/TestParentB/{c}TestChildB/@kuku"  }
   
   def testassoc2 = expect ("f1") { Assets xpa "/TestChildB[kuku==c1]/TestFriendB/@kuku" }
   def testassoc2a = expect ("f1") { Assets xpa "/TestChildB[kuku==c1]/{a}TestFriendB/@kuku" }

   def testassoc3 = expect ("f1") { Assets xpa "/TestParentB/TestChildB[kuku==c1]/TestFriendB/@kuku" }
   def testassoc3a = expect ("f1") { Assets xpa "/TestParentB/{c}TestChildB[kuku==c1]/{a}TestFriendB/@kuku" }
   
   def testreflectedassoc1 = expect ("c1") {Assets xpa "/TestParentC/TestChildC[key==c1]/@key" }
   def testreflectedassoc11 = expect ("c1") { Assets xpa "/TestParentC/TestChildC[key==c1]/@key" }
   def testreflectedassoc12 = expect ("c1") { Assets xpa "/TestParentC/TestChildC[key==c1]/@key" }
   def testreflectedassoc2 = expect ("p1") { Assets xpa "/TestChildC[key==c1]/TestParentC/@key" }
   def testreflectedassoc3 = expect ("c1") { Assets xpa "/TestChildC[key==c1]/@key" }
   
   
   override def tearDown = {
//      super.tearDown
   }
}

/**
 * the expressions we support:
 * <ul>
 * <li> "/TestParentB/TestChildB" - must have exactly one TestParentB -> {composition} -> TestChildB
 * <li> "/TestChildB/TestFriendB" - must have exactly one TestChildB -> {association} -> TestFriendB
 * <li> "/TestChildB/{D}/TestFriendD" - must have exactly one TestChildB -> {association:name=D} -> TestFriendB
 */
