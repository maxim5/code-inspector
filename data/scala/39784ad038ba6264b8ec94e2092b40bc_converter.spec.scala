package org.scardf

import org.joda.time.LocalDate
import org.specs._
import org.specs.runner.JUnit4
import org.joda.time.LocalDate
import NodeConverter._
import PeopleVoc._
import Doe._

class ConverterSpecsTest extends JUnit4(ConverterSpecs)

object ConverterSpecs extends Specification {
  val g = Doe.graph
  "value converters" should {
    "convert typed literal to Scala value" in {
      TypedLiteral( "10", XSD.integer )/asInt must_== 10
    }
    "convert plain literal to typed Scala value" in {
      PlainLiteral( "10" )/asInt must_== 10
    }
    "convert bags to single value" in {
      g/john/spouse/height/asInt must_== 150
      g/john/birthday/asLocalDate must_== new LocalDate( 1977, 07, 27 )
      g/john/birthday/asString must_== "1977-07-27"
    }
  }
  "predicate-converters" should {
    "convert subject node to bag" in {
      g/john/height must_== NodeBag( List( TypedLiteral( "167", XSD.int ) ), g )
      g/john/likes must_== g.bagOf( swimming, science )
      g/bob/likes must_== g.bagOf()
    }
    ".option" in {
      g/bob/likes.option must_== None
    }
    "with .n modifier, get node" in {
      g/anna/likes.n must_== swimming
    }
    "with .v modifier, convert node to value" in {
      g/john/height.v must_== 167
    }
    "list all values for fixed predicate" in {
      g/-/height must_== g.bagOf( 99, 107, 150, 167 )
    }
    "list all values for fixed predicate, multiple values" in {
      g/-/likes must_== g.bagOf( swimming, swimming, swimming, science )
    }
    "distinct filter" in {
      g/-/likes/distinct must_== g.bagOf( swimming, science )
    }
    "with .set modifier, convert bag to Set" in {
      g/-/height.set must_== Set( 99, 107, 150, 167 )
    }
    "pred set converter" in {
      familyMembers/likes.set must_== Set( swimming(g), science(g) )
    }
  }
}
