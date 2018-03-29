package net.croz.scardf

import org.joda.time.LocalDate
import org.specs.Specification
import PeopleVocabulary._
import FamilyVocabulary._

class QuerySpecsTest extends org.specs.runner.JUnit4(QuerySpecs)

object QuerySpecs extends Specification with specs.RdfMatchers {
  val data = FamilyVocabulary.model
  "Triplet factory" should {
    "make correct number of triplets from graph" in {
      implicit val tempGraph = new Model
      Anon( Likes -> Anon(), Spouse -> Anon( Likes -> Anon() ) )
      val ts = Set.empty ++ ( query.TripletFactory tripletsFrom tempGraph )
      ts.size must_== 3
      for ( t <- ts ) t must haveClass[Tuple3[Any, Any, Any]]
    }
  }
  "Query mechanism" should {
    "select with number literal in where triple's object" in {
      val person = QVar()
	  val personsHigh107 = 
	    Sparql select( person ) where( (person, Height, 107), (person, RDF.Type, Person) ) from data
	  personsHigh107.solutions must_== List( Map( person -> anna ) )
    }
    "select, order, offset and limit" in {
      val person, height = QVar()
      val selectPersonsByHeight = ( Sparql 
        select( person, height ) 
          where( (person, RDF.Type, Person), (person, Height, height) )
          orderBy( asc( height ) )
          limit 2 offset 1
      )
      ( selectPersonsByHeight from data ).solutions must_== List(
        Map( person -> anna, height -> Lit(107) ), Map( person -> jane, height -> Lit(150) )
      )
    }
    "select using symbols" in {
      val person, height = QVar()
      val selectPersonsByHeight = ( Sparql 
        select( person, height ) 
          where( (person, RDF.Type, Person), (person, Height, height) )
          orderBy( asc( height ) )
          limit 2 offset 1
      )
      ( selectPersonsByHeight from data ).solutions must_== List(
        Map( person -> anna, height -> Lit(107) ), Map( person -> jane, height -> Lit(150) )
      )
    }
    "select with/without DISTINCT" in {
      val person, hobby = QVar()
      val distinctHobbies = Sparql select distinct( hobby ) where( (person, Likes, hobby) ) from data
      distinctHobbies.solutions.map{ _(hobby) } must_== List( Swimming, Science )
      val allHobbiesResult = Sparql select hobby where( (person, Likes, hobby) ) from data
      val hobbies = allHobbiesResult.solutions.map{ _(hobby).asRes }
      hobbies.size must beGreaterThanOrEqualTo( 2 )
      hobbies must containAll( Set( Swimming, Science ) )
    }
    "select one X as option" in {
      Sparql selectX asRes where( (X, Likes, Science) ) from data must_== Some( john )
      Sparql selectX asInt where( (jane, Height, X) ) from data must_== Some( 150 )
      Sparql selectX asInt where( (jane, Weight, X) ) from data must_== None
    }
    "select all X as iterator" in {
      val iter = Sparql selectAllX asRes where( (X, Likes, Swimming) ) from data
      Set.empty ++ iter.toList must_== Set( anna, jane, john )
    }
    "select with optional constraints" in {
      val person, spouse = QVar()
      val selectPersonsWithSpouses = ( Sparql 
        select( person, spouse ) 
          where( (person, RDF.Type, Person) )
          optional( (person, Spouse, spouse) )
      )
      (selectPersonsWithSpouses from data).solutions == List(
        Map( person -> anna ), 
        Map( person -> bob ), 
        Map( person -> jane, spouse -> john ),
        Map( person -> john, spouse -> jane )
      )
    }
    "ask queries" in {
      Sparql ask( (john, Likes, Science) ) in data must_== true
      Sparql ask( (X, IsMale, false), (X, Likes, Science) ) in data must_== false
    }
    "construct graphs from template" in {
      import net.croz.scardf.query._
      val template = Blank( Likes -> Blank(), Spouse -> Blank( Likes -> Blank() ) ).toModel
      val constructedGraph = Sparql construct template from data
      val expectedGraph = new Model
      expectedGraph addAll List( 
        john( Spouse -> jane ), john( Likes -> Swimming ), john( Likes -> Science ),
        jane( Spouse -> john ), jane( Likes -> Swimming )
      )
      constructedGraph must be_=~( expectedGraph )
    }
    "construct graphs using predicate trees" in {
      val ptree = PredicateTree( Likes, Spouse~Likes )
      val constructedGraph = Sparql construct ptree from john
      val expectedGraph = new Model
      expectedGraph addAll List( 
        john( Spouse -> jane ), john( Likes -> Swimming ), john( Likes -> Science ),
        jane( Likes -> Swimming )
      )
      constructedGraph must be_=~( expectedGraph )
    }
//    "construct graphs using predicate trees, with missing nodes" in {
//      val ptree = PredicateTree( Likes, Spouse-Likes )
//      val constructedGraph = Sparql construct ptree from anna
//      val expectedGraph = new Model
//      expectedGraph addAll List( anna( Likes -> Swimming ) )
//      constructedGraph.dump
//      constructedGraph must be_=~( expectedGraph )
//    }
  }
}
