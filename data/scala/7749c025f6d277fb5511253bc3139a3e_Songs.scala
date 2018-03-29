package com.pocketchangeapp
package snippet

import java.util.Date
import net.liftweb.http.js.JsCmds
import scala.xml.{NodeSeq,Text}

import net.liftweb.http.js.JsCmds
import net.liftweb.common.{Logger, Box, Empty, Full}
import net.liftweb.http.{DispatchSnippet, RequestVar, S, SHtml}

// Import "bind", "chooseTemplate" and associated implicits
import net.liftweb.util.Helpers._

import com.pocketchangeapp.model.{Song,User, Source}
import util.Util

object Songs extends DispatchSnippet with Logger {
  def dispatch : DispatchIt = {
    case "manage" => manage _
    case "edit" => edit _
	case "detail" => detail _
	case "detaill" => detaill _
	case "all" => all _
  }
  
  def asLyrics(arg:String): NodeSeq = {
	val lines = arg.split("\n").toList
	
	lines.foldLeft[NodeSeq] (new Text(""))((seq: NodeSeq, e: String) => seq :+ (new Text(e)) :+ <br/>)
  }
  
  def all (xhtml : NodeSeq) : NodeSeq = {
    Song.all.flatMap({sng =>
        bind("sng", chooseTemplate("song", "entry", xhtml),
            // "title" -> Text(sng.title.is)
			"title"-> <a href={"/song/" + sng.title.is}> {sng.title.is}</a>)
	})
	}
	
  def manage (xhtml : NodeSeq) : NodeSeq = {
    def deleteSong (sng : Song) {
      sng.admins.foreach(_.delete_!)
      sng.viewers.foreach(_.delete_!)
      sng.delete_!
    }
	
	 

    User.currentUser.map({user =>
      user.songs.flatMap({sng =>
        bind("sng", chooseTemplate("song", "entry", xhtml),
             "title" -> Text(sng.title.is),
			 "time" -> Text(sng.time.is),
             "lyrics" -> Text(sng.lyrics.is),
			 //"lyrics" -> SHtml.textarea(noteText, noteText = _, "cols" -> "80", "rows" -> "8"),
			 "yearCreated" -> Text(sng.yearCreated.is),
             "tagCloud" -> Text(sng.tagCloud.is),
			 "narrator" -> Text(sng.narrator.is),
			 "narratorLocation" -> Text(sng.narratorLocation.is),			 
			 "musicBy" -> Text(sng.musicBy.is),
			 "lyricsBy" -> Text(sng.lyricsBy.is),
			 //"source" -> Text(sng.source.is),
			 "location" -> Text(sng.location.is),
			 "references" -> Text(sng.references.is),
			 "actions" -> { SHtml.link("/editSongs", () => sng.delete_!, Text("Dz?st")) ++ Text(" ") ++
                            SHtml.link("/editSng", () => currentSongVar(sng), Text("Redi??t")) })
      })
		}) openOr Text("You're not logged in")
  }
  
    def detaill (xhtml: NodeSeq) : NodeSeq = S.param("name") match {
    case Full(sngTitle) => {
      Song.findByTitle(sngTitle) match {
        case sng :: Nil => {
          val tags = <a href={"/songg/" + sng.title.is}>All tags</a> ++ Text(" ") ++
            sng.tags.flatMap({tag => <a href={"/songg/" + sng.title.is + "/" + tag.name.is}>{tag.name.is}</a> ++ Text(" ")})
			//val sT : Box[Source] = 
			val tag = S.param("tag")
			//var startDate : Box[Date] = Empty
			//var endDate : Box[Date] = Empty
			def entryTableS = buildSourceTable(Source.getBySng(sng), tag, xhtml)
			def updateTable() = {
            JsCmds.SetHtml("entry_table_source", entryTableS)
          }
			
			bind("sng", xhtml,
               "atomLink" -> <link href={"/api/songg/" + sng.id} type="application/atom+xml" rel="alternate" title={sng.title + " feed"} />,
               "title" -> sng.title.asHtml,
               "time" -> sng.time.asHtml,
               "lyrics" -> asLyrics(sng.lyrics),
               "yearCreated" -> sng.yearCreated.asHtml,
               "tagCloud" -> sng.tagCloud.asHtml,
			   "narrator" -> sng.narrator.asHtml,
			 "narratorLocation" -> sng.narratorLocation.asHtml,			 
			 "musicBy" -> sng.musicBy.asHtml,
			 "lyricsBy" -> sng.lyricsBy.asHtml,
			// "source" -> sng.source.asHtml,
			 "location" -> sng.location.asHtml,
			 "references" -> sng.references.asHtml,
			 "table" -> entryTableS)
        }
        case _ => warn("Couldn't locate song \"%s\"".format(sngTitle)); Text("Could not locate song 1" + sngTitle)
      }
    }
    case _ => Text("No song name provided")
  }
  
  def detail (xhtml: NodeSeq) : NodeSeq = S.param("name") match {
    case Full(sngTitle) => {
      Song.findByName(User.currentUser.open_!, sngTitle) match {
        case sng :: Nil => {
          val tags = <a href={"/song/" + sng.title.is}>All tags</a> ++ Text(" ") ++
            sng.tags.flatMap({tag => <a href={"/song/" + sng.title.is + "/" + tag.name.is}>{tag.name.is}</a> ++ Text(" ")})
			//val sT : Box[Source] = 
			val tag = S.param("tag")
			//var startDate : Box[Date] = Empty
			//var endDate : Box[Date] = Empty
			def entryTableS = buildSourceTable(Source.getBySng(sng), tag, xhtml)
			def updateTable() = {
            JsCmds.SetHtml("entry_table_source", entryTableS)
          }
			
			bind("sng", xhtml,
               "atomLink" -> <link href={"/api/song/" + sng.id} type="application/atom+xml" rel="alternate" title={sng.title + " feed"} />,
               "title" -> sng.title.asHtml,
               "time" -> sng.time.asHtml,
               "lyrics" -> asLyrics(sng.lyrics),
               "yearCreated" -> sng.yearCreated.asHtml,
               "tagCloud" -> sng.tagCloud.asHtml,
			   "narrator" -> sng.narrator.asHtml,
			 "narratorLocation" -> sng.narratorLocation.asHtml,			 
			 "musicBy" -> sng.musicBy.asHtml,
			 "lyricsBy" -> sng.lyricsBy.asHtml,
			// "source" -> sng.source.asHtml,
			 "location" -> sng.location.asHtml,
			 "references" -> sng.references.asHtml,
			 "table" -> entryTableS)
        }
        case _ => warn("Couldn't locate song \"%s\"".format(sngTitle)); Text("Could not locate song 1" + sngTitle)
      }
    }
    case _ => Text("No song name provided")
  }


  object currentSongVar extends RequestVar[Song]({
    Song.create.owner(User.currentUser.open_!)
  })

  def currentSong = currentSongVar.is

  def edit (xhtml : NodeSeq) : NodeSeq = {
    def doSave () = {
      currentSong.validate match {
        case Nil =>
          currentSong.save
          S.redirectTo("/editSongs")
        case x => S.error(x)
      }
    }
    val sng = currentSong
    bind("sng", xhtml,
        "id" -> SHtml.hidden(() => currentSongVar(sng)),
        "title" -> SHtml.text(currentSong.title.is, currentSong.title(_)),
        "time" -> SHtml.text(currentSong.time.is, currentSong.time(_)),
        "lyrics" -> SHtml.textarea(currentSong.lyrics.is, currentSong.lyrics(_), "cols" -> "80", "rows" -> "4"),
		"yearCreated" -> SHtml.text(currentSong.yearCreated.is, currentSong.yearCreated(_)),
		"tagCloud" -> SHtml.text(currentSong.tagCloud.is, currentSong.tagCloud(_)),
		"narrator" -> SHtml.text(currentSong.narrator.is, currentSong.narrator(_)),
		"narratorLocation" -> SHtml.text(currentSong.narratorLocation.is, currentSong.narratorLocation(_)),		 
		"musicBy" -> SHtml.text(currentSong.musicBy.is, currentSong.musicBy(_)),
		"lyricsBy" -> SHtml.text(currentSong.lyricsBy.is, currentSong.lyricsBy(_)),
		"location" -> SHtml.text(currentSong.location.is, currentSong.location(_)),
		"references" -> SHtml.text(currentSong.references.is, currentSong.references(_)),
		"save" -> SHtml.submit("Saglab?t", doSave))
  }
  
  

  def buildSourceTable(entries : List[Source], tag : Box[String], template : NodeSeq) = {
    val filtered = tag match {
      case Full(name) => entries.filter(_.tags.exists(_.name == name)) // Can probably be made more efficient
      case _ => entries
    }

    filtered.flatMap({ entry =>
      val sTitle  =
	      {
	        Text(entry.sourceTitle.is)
	      }
	
      bind("entry", chooseTemplate("sng", "tableEntry", template),
	         "rtype" -> Text(entry.sourceType.is),
	         "sTitle" -> sTitle,
	         "sYearCreated" -> Text(entry.sourceYearCreated.is),
	         "sPublisher" -> Text(entry.sourcePublisher.is))
		})
  }
  
}
