package org.mybatis_scala.samples

import org.mybatis_scala.util.LogHelper
import org.specs.{HtmlSpecification,Specification}
import org.specs.literate.Textile
import org.mybatis_scala.model._
import java.util.{Date,ArrayList}
import org.joda.time.DateTime


class ModelSpec extends HtmlSpecification with Textile with LogHelper{
		
	"Sample Application Model Classes description" is <t>
	
	h2. Class description
	
	All Scala MyBatis samlples will be created around three enities: Genre, Book and Author.
	
	h3. Genre
	
	GenreRecord and Genre are business model classes for the book genre.
	GenreRecord is mutable bean-like class that represents a record in the GENRE table.
	Genre is  immutable class for the same purpose.

	Application should <ex>allow to create and check equivalence of GenreRecord</ex>
	{ eg {
			val gr1 = new GenreRecord 
			gr1.genreId = 1
			gr1.name = "Name1"
			val gr2 = new GenreRecord
			gr2.genreId = 1
			gr2.name = "Name1"
			lzDebug ("gr1: "+gr1+" gr2: "+gr2)		
			gr1 must_== gr2
		
			// modify mutable GenreRecord
			gr2.name  = "Name2"
			lzDebug ("modified g2: "+gr2)
		
			gr1 must_!= gr2
						
		}
	}

	Application should <ex>allow to create and check equivalence of Genre</ex>
	{ eg {
			val gr1 = new GenreRecord 
			gr1.genreId = 1
			gr1.name = "Name1"
			val gr2 = new GenreRecord
			gr2.genreId = 1
			gr2.name = "Name2"
			
			lzDebug ("gr1: "+gr1+" gr2: "+gr2)		
			val g1 = new Genre (1,"Name1")
			val g2 = new Genre (gr1)
			val g3 = new Genre (gr2)
		
			g1 must_== g2
			g1 must_!= g3						
			
		}
	}
	
	h3. Book
	
	BookRecord is bean-like class that represents a record in the BOOK table.
	Book is immutable class. Everybody knows what is the book.
	
	Application should <ex>allow to create and check equivalence of BookRecord</ex>
	{ eg {
			val g1 = new Genre (1, "Science Fiction")
	
			val b1 = new BookRecord 
			b1.title = "Title1"
			b1.isbn = "isbn1"
			b1.genre = g1
	
			val b2 = new BookRecord 
			b2.title = "Title1"
			b2.isbn = "isbn1"
			b2.genre = g1
			
			b1 must_== b2
			
			// change book genre
			b2.genre = new Genre (2, "Love Story")
			b1 must_!= b2
			
		}			
	}
	
	Application should <ex>allow to create and check equivalence of Book</ex>
	{ eg {
			val g1 = new Genre (1, "Science Fiction")
			val g2 = new Genre (1, "Love Store")
			
			val br1 = new BookRecord 
			br1.bookId = 1
			br1.title = "Title1"
			br1.isbn = "isbn1"
			br1.genre = g1

			val br2 = new BookRecord 
			br2.bookId = 2
			br2.title = "Title1"
			br2.isbn = "isbn1"
			br2.genre = g1
			
			val b1 = new Book (1,"Title1", "isbn1" , g1)
			val b2 = new Book (br1)
			val b3 = new Book (br2)
			
			b1 must_== b2
			b2 must_!= b3
		}
	}

	h3. Author
	
	AuthorRecord is bean-like class that reprsent a record n the Author table.
	Author is immutable class.
	Obviously, class Author represents the author of the book.
	
	Application should <ex>allow to create and check equivalence of AuthorRecord</ex>
	{ eg {
	
			val curDate = new DateTime(new Date())
	
			val b1 = predefinedBook1
			val b2 = predefinedBook1
		
			val bookList1 = new java.util.ArrayList[BookRecord]()
			bookList1.add (b1)
			bookList1.add (b2)
		
			val bookList2 = new java.util.ArrayList[BookRecord]()
			bookList2.add (b1)
			bookList2.add (b2)		
		
			val ar1 = new AuthorRecord 
			ar1.authorId = 1
			ar1.birthDay = curDate
			ar1.firstName = "firstName"
			ar1.lastName = "lastName"
			ar1.annotation = "annotation"
			ar1.books = bookList1

			val ar2 = new AuthorRecord 
			ar2.authorId = 1
			ar2.birthDay = curDate
			ar2.firstName = "firstName"
			ar2.lastName = "lastName"
			ar2.annotation = "annotation"
			ar2.books = bookList2

			ar1 must_== ar2
			
			// remove one book
			bookList2.remove(0)			
			ar1 must_!= ar2
			
		}
	}
	
	Application should <ex>allow to create and check equivalence of Author</ex>
	{ eg {
			val curDate = new DateTime(new Date())
		
			val b1 = predefinedBook1
			val b2 = predefinedBook2
	
			val bookList1 = new java.util.ArrayList[BookRecord]()
			bookList1.add (b1)
			bookList1.add (b2)
	
			val bookList2 = new java.util.ArrayList[BookRecord]()
			bookList2.add (b1)
			bookList2.add (b2)		
	
			val ar1 = new AuthorRecord 
			ar1.authorId = 1
			ar1.birthDay = curDate
			ar1.firstName = "firstName"
			ar1.lastName = "lastName"
			ar1.annotation = "annotation"
			ar1.books = bookList1		
												
			val a1 = new Author (1, curDate, "firstName", "lastName", "annotation", List ( Book(bookRecord = b1, authorId =1), Book (bookRecord = b2,authorId = 1)))
			val a2 = new Author (ar1)
			val a3 = new Author (1, curDate.minusYears(100), "firstName", "lastName", "annotation", List (Book(bookRecord = b1, authorId = 1),  Book (bookRecord = b2,authorId = 1)))

			lzDebug ("Author equivalence bookList1: "+bookList1)
			lzDebug ("Author equivalence a1: "+a1)
			lzDebug ("Author equivalence a2: "+a2)
			lzDebug ("Author equivalence a3: "+a3)
						
			a1 must_== a2
			a1 must_!= a3
			
		}
	}

	</t>
	
	/**
	* Set of predifened object aimed to reduce code
	*/
	def predefinedGenre1 : Genre =  new Genre (1, "Science Fiction")
	def predefinedGenre2 : Genre =  new Genre (2, "Love Story")

	def predefinedBook1 : BookRecord = {
		val g1 = predefinedGenre1

		val b1 = new BookRecord 
		b1.bookId = 1
		b1.title = "Title1"
		b1.isbn = "isbn1"
		b1.genre = g1
		b1		
	}
	
	def predefinedBook2 : BookRecord = {
		val g2 = predefinedGenre2

		val b2 = new BookRecord 
		b2.bookId = 2
		b2.title = "Title1"
		b2.isbn = "isbn1"
		b2.genre = g2
		b2		
	}	
}


