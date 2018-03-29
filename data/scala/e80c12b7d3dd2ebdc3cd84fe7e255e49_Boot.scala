package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import provider.HTTPCookie

import net.liftweb.json.JsonAST._
import model.MongoConfig
import code.model.db.{User,NMap}

import code.lib.daemons._
import code.lib.{ThumbnailGenerator,S3Manager}


/**
* A class that's instantiated early and run.  It allows the application
* to modify lift's environment
*/
class Boot {
	def boot {
		LiftRules.addToPackages("code")

		val activeNotice = If(() => User.activated_?, "You cannot access that page because your account has not been activated. Check your email.")

		def sitemap = SiteMap(
			Menu.i("Home") / "index" >> Hidden,
			Menu.i("Browse") / "maps",
			Menu.i("Login") / "login" >> Hidden >> Unless(() => User.loggedIn_?,
				"You are already logged in."),
			Menu.i("Sign Up") / "signup" >> Hidden >> Unless(() => User.loggedIn_?,
				"You are already logged in."),
			Menu.i("Import Log") / "import_log" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in.") >> activeNotice,
			Menu.i("Import") / "import" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in.") >> activeNotice,
			Menu.i("Single Map") / "single_map" >> Hidden,
			Menu.i("Edit Map") / "edit_map" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in."),
			Menu.i("Submit") / "submit" >> If(() => User.loggedIn_?,
				"You must be logged in.") >> activeNotice >>
				Unless(() => User.throttled_?, "You have reached your map limit for today."),
			Menu.i("Search") / "search" >> Hidden,
			Menu.i("Log Out") / "logout" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in to log out.") >>
					EarlyResponse(() => Full(RedirectWithState("/",
						RedirectState(() => User.logOut(User.loggedInUser.open_!)))
					)),
			Menu.i("Single User") / "user" >> Hidden,
			Menu.i("Edit Avatar") / "edit_avatar" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in."),
			Menu.i("Manage Users") / "manage" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in.") >> If(() => User.loggedInUser.open_!.isAdmin_?,
				"You must be an administrator to view that page."),
			Menu.i("Edit User") / "edit_user" >> Hidden >> If(() => User.loggedIn_?,
				"You must be logged in."),
			Menu.i("About") / "about",
			Menu.i("Validate") / "validate" >> Hidden >> Unless(() => User.activated_?,
				"Your account has already been activated."),
			Menu.i("Bitesize") / "bitesize" >> Hidden >> If(() => User.loggedIn_?, "You must be logged in.") >>
				If(() => User.loggedInUser.open_!.isBitesizer_?, "You do not have bitesize privileges.")
		)

		LiftRules.setSiteMap(sitemap)

		LiftRules.earlyInStateful.append(User.checkCookie)
		
		LiftRules.statelessRewrite.append {
			case RewriteRequest(ParsePath(List("maps", id),_,_,_),_,_) => 
				RewriteResponse("single_map" :: Nil, Map("id" -> id))
			case RewriteRequest(ParsePath(List("users", uname),_,_,_),_,_) =>
				RewriteResponse("user" :: Nil, Map("username" -> uname))
			case RewriteRequest(ParsePath(List("users", uname, "edit"),_,_,_),_,_) =>
				RewriteResponse("edit_user" :: Nil, Map("username" -> uname))
			case RewriteRequest(ParsePath(List("users", uname, "avatar"),_,_,_),_,_) =>
				RewriteResponse("edit_avatar" :: Nil, Map("username" -> uname))
			case RewriteRequest(ParsePath(List("maps", id, "edit"),_,_,_),_,_) =>
				RewriteResponse("edit_map" :: Nil, Map("id" -> id))
			case RewriteRequest(ParsePath(List("manage", username),_,_,_),_,_) =>
				RewriteResponse("manage" :: Nil, Map("username" -> username))
			case RewriteRequest(ParsePath(List("maps", id, "bitesize"),_,_,_),_,_) =>
				RewriteResponse("bitesize" :: Nil, Map("id" -> id))
		}
		
		LiftRules.statelessDispatchTable.append(ThumbnailGenerator)

		// Use jQuery 1.4
		LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

		//Show the spinny image when an Ajax call starts
		LiftRules.ajaxStart =
		Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

		// Make the spinny image go away when it ends
		LiftRules.ajaxEnd =
		Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

		// Force the request to be UTF-8
		LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
		
		LiftRules.handleMimeFile = OnDiskFileParamHolder.apply

		MongoConfig.init
		
		NMap.ensureIndex(JObject(JField("tags", JInt(1)) ::
			JField("mapdata_hash", JInt(1)) :: Nil))
			
		User.ensureIndex(JObject(JField("username", JInt(1)) :: Nil))
		
		RatingCalc.start
		MapImporter.start
		MapScraper.start
		MapDownloader.start
		CommentDownloader.start
		
		S3Manager.init(Props.get("aws.accesskey", "foo"), Props.get("aws.secretkey", "bar"))
	}
}
