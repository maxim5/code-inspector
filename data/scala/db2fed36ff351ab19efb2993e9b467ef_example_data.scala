package net.croz.scardf

object PeopleVocabulary extends Vocabulary( "http://person.eg#" ) {
  val Person = pRes( "Person" )
  val Name = pProp( "Name" )
  val Given = pProp( "Given" )
  val Family = pProp( "Family" )
  val Birthday = pProp( "Birthday" ) withRange XSD.date
  val IsMale = pProp( "IsMale" ) withRange XSD.boolean
  val Height = pProp( "Height" ) withRange XSD.int
  val Weight = pProp( "Weight" ) withRange XSD.int
  val Hobby = pRes( "Hobby" )
  val Likes = pProp( "Likes" ) withRange Hobby
  val Swimming = pRes( "Swimming" ) a Hobby
  val Science = pRes( "Science" ) a Hobby
  val Spouse = pProp( "Spouse" ) withRange Person
  val Children = pProp( "Children" )
  val Father = pProp( "Father" )
}

import PeopleVocabulary._

object FamilyVocabulary extends Vocabulary( "http://family.eg#" ) {
  private implicit val m = model
  private val aMale = IsMale -> true
  private val aFemale = IsMale -> false
  
  val anna = pRes( "anna" ) a Person state(
    Name -> Anon( Given -> "Anna" ),
    aFemale, Birthday -> "2004-04-14", Height -> 107,
    Likes -> Swimming
  )
  val bob = pRes( "bob" ) a Person state(
    Name -> Anon( Given -> "Bob" ),
    aMale, Birthday -> "2007-05-18", Height -> 87
  )
  val john = pRes( "jdoe" ) a Person state(
    Name -> Anon( Given -> "John" ),
    aMale, Birthday -> "1977-07-27", Height -> 167,
    Likes -> ( Swimming, Science ),
    Children -> RdfList( anna, bob ), Spouse -> pRes( "jane" )
  )
  val jane = pRes( "jane" ) a Person state(
    Name -> Anon( Given -> "Jane" ),
    aFemale, Birthday -> "1976-06-26", Height -> 150,
    Likes -> Swimming,
    Children -> RdfList( anna, bob ), Spouse -> john
  )
  List( anna, bob, jane, john ) foreach { (Name~Family)( _ ) = "Doe" }
  john/Children/asRdfList foreach { n => Father( n.asRes ) = john }
}
