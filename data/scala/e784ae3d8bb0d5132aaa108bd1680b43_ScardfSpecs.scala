package net.croz.scardf

import org.joda.time.LocalDate
import org.specs._
import org.specs.runner.JUnit4
import PeopleVocabulary._

object AllSpecs extends Specification {
  "all of Scardf".isSpecifiedBy( 
    ScardfSpecs, LitSpec, NoVarSpec, PropPathSpec, QuerySpecs // add NodeBagSpec
  )
  reject( "skipped" )
}

class ScardfSpecsTest extends JUnit4(ScardfSpecs)

object ScardfSpecs extends Specification {
  "NodeBag" should {
    "throw exception on taking one node from empty bag" in {
      NodeBag().oneNode must throwA[ NoSuchElementException ]
    }
    "sort" in {
      NodeBag( Lit( "a" ), Lit( "b" ), Lit( "a" ) ).sorted must_== NodeBag( Lit( "a" ), Lit( "a" ), Lit( "b" ) )
    }
    "do equals" in {
      NodeBag( Lit( "a" ) ) must_== NodeBag( Lit( "a" ) )
      NodeBag( Lit( "a" ) ) must_!= NodeBag( Lit( "a" ), Lit( "a" ) )
      NodeBag( Lit( "a" ), Lit( "b" ), Lit( "a" ) ) must_== NodeBag( Lit( "a" ), Lit( "a" ), Lit( "b" ) )
    }
  }
  "Constructed graph" should {
    implicit val model = new Model() //withPrefix "example:"
    import PeopleVocabulary._
    val jdoe = Res( "jdoe" ) a Person state(
      Name -> Anon(
        Given -> "John",
        Family -> "Doe"
      ),
      Birthday -> "1977-07-27",
      Height -> 167,
      IsMale -> true,
      Likes -> All( Swimming, Science ),
      Children -> RdfList( Res( "anna" ), Res( "bob" ) )
    )
    "extract property value" in {
      Given( Name( jdoe ) ) must_== Lit( "John" )
    }
    "assign value to property" in {
      Weight( jdoe ) = 88
      Weight( jdoe ) must_== Lit( 88 )
      //(Name-Family)( Res( "anna" ) ) = "Doe"
    }
    "read path" in {
      jdoe/Name/Given/asString must_== "John"
      jdoe/Height/asInt must_== 167
    }
    "handle multiple-node results" in {
      jdoe/Spouse must beEmpty
      jdoe/Spouse/asRes.option must_== None
      jdoe/Spouse/Name/Family must beEmpty
      jdoe/Spouse/Name/Family/asString.default( "(unknown)" ) must_== "(unknown)"
      jdoe/Likes/asRes.set must_== Set( Swimming, Science )
    }
    "test boolean value" in {
      ( jdoe/IsMale? ) must_== true
      jdoe has Height -> 167 must_== true
      ( jdoe( Likes -> Science )? ) must_== true
    }
    "read date" in {
      jdoe/Birthday/asLocalDate must_== new LocalDate( 1977, 7, 27 )
    }
    "read collections" in {
      (jdoe/Children/asRdfList).toList must_== List( Res( "anna" ), Res( "bob" ) )
    }
    "sparql query heighest" in {
      val selectHighest = Sparql select 'person where( ('person, Height, 'h) ) orderBy desc( 'h ) limit 1
      val results = selectHighest from model
      results.solutions must_== List( Map( QVar( "person" ) -> jdoe ) )
    }
    "sparql query select X" in {
      Sparql selectX asRes where( (X, Height, 167) ) from model must_== Some( jdoe )
    }
  }
  /*
  "read graph" should {
    val turtleSrc = """
@prefix :        <person:> .
<example:jdoe>
      a :Person ;
      :Birthday "1977-07-27";
      :Children (<example:anna> <example:bob>) ;
      :Height 167;
      :IsMale true;
      :Likes  :Swimming , :Science ;
      :Name   [ :Family "Doe";
                :Given  "John"
              ] .
<example:anna> a :Person; :Name [:Family "Doe"; :Given "Anna"].
<example:bob> a :Person; :Name [:Family "Doe"; :Given "Bob"].
"""
    import PeopleVocabulary._
    val m = new Model
    m.jModel.read( new java.io.StringReader( turtleSrc ), null, "TURTLE" )
    "" in {
      val rlist = m.getRes( "example:jdoe" )/Children/asRdfList
      rlist.toList must_== List( m.getRes( "example:anna" ), m.getRes( "example:bob" ) )
    }
    "" in {
      println( m.getRes( "example:jdoe" )/Children/asRdfList/Name/Family )
    }
  }*/
}
