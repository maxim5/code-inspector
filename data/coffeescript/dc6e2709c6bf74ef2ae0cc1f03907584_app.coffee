
###
Module dependencies.
###
express = require("express")
http = require("http")
https = require("https")
path = require("path")
passport = require('passport')
querystring = require('querystring')
GoogleStrategy = require('passport-google').Strategy;
GoogleOAuth2Strategy = require('passport-google-oauth').OAuth2Strategy;
mongoose = require('mongoose')
bcrypt = require('bcrypt')
util = require("util")
exec = require("child_process").exec
child = undefined
Encryption = require('./encryption').Encryption
SALT_WORK_FACTOR = 10;
GoogleCalendar = require('google-calendar')
calendar = new GoogleCalendar.GoogleCalendar(
  '314528618328-v4k0lve6dl83dboanp04d85n7u9mldd6.apps.googleusercontent.com',
  'zH8LPkdnVmawAePGthgJ_cRo',
  'http://calendarsync.dadams.c9.io/auth/google/callback')
  

db = mongoose.createConnection('mongodb://david:1qazxsw2@alex.mongohq.com:10041/calendarsync')
db.on "error", console.error.bind(console, "connection error:")

#child = exec 'python calendarsync.py', (error, stdout, stderr) ->
  #console.log "stdout: " + stdout
  #console.log "stderr: " + stderr
  #console.log "exec error: " + error  if error isnt null

#

encryption = new Encryption()

#console.log 'salting'
#console.log bcrypt.genSaltSync SALT_WORK_FACTOR
#password = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
#input = "hello world"
#encryption.encrypt input, password, (encoded) ->
  #console.log encoded
  #encryption.decrypt encoded, password, (output) ->
    #console.log output
    
userSchema = new mongoose.Schema(
     # _id:mongoose.Types.ObjectId
      displayName:String
      email:
        type: String, unique:true
      firstname: String
      lastname:String
      sonausername: String
      sonapassword: String
      sonavalid: Boolean
      experiments:  Array
      calendarnames: Array
      calendarids: Array
      salt: String
      gid:
        type: String, unique:true
      accessToken: 
        type: String, unique:true)

User = db.model 'users', userSchema
      
experimentSchema = new mongoose.Schema(
      _id:String
      Date:String
      Date2:String
      starttime:String
      endtime:String
      ParticipantSigned:String
      StudentTimeSlot:String
      sonanumber:Number
      experiment:String
      calendarid:String)
      
Experiment = db.model 'experiments', experimentSchema

#david = new User
  #name: 'David'
  #email: 'blah@blah.com'
  #
#david.save (err)->
  #console.log("error saving") if err
  #
#Experiment.find (err,result)->
  #console.log 'finding things'
  #if(err)
    #console.log(err)
  #else
    #console.log(result)
  
  
passport.serializeUser (user, done) ->
  if user.identifier
    done null, user.identifier
  if user.id
    done null, user.id
  #console.log 'name ' +  user.name
  #console.log ' email ' + user.emails
  #User.findOne 'gid':user.identifier, (err,u)->
    #console.log 'serializeUser ' + user.identifier 
    #if u
      #console.log 'found old user' + u
      #done(null,u)
    #else
      #console.log 'serializeUser no user found'
      #u['gid'] = user.identifier
      #u.save (err)->
        #console.log("error saving") if err
      #done null, u

passport.deserializeUser (id, done) ->
  #console.log 'deserializeUser ' + id
  User.findOne 'gid':id, (err, user)->
    if err 
      console.log 'user not found ' + id
    done err, user
###  
passport.use new GoogleStrategy(
  returnURL: "http://calendarsync.dadams.c9.io/auth/google/return"
  realm: "http://calendarsync.dadams.c9.io/",
  (identifier, profile, done) ->
    # asynchronous verification, for effect...
    process.nextTick ->
    #To keep the example simple, the user's Google profile is returned to
    #represent the logged-in user.  In a typical application, you would want
    #to associate the Google account with a user record in your database,
    #and return that user instead.
      profile.identifier = identifier
      User.findOne 'gid':identifier, (err, user)->
        if !user
          console.log 'creating new user'
          u = new User()
          u.emails = profile.emails.value
          u.firstname = profile.name.givenName
          u.lastname = profile.name.familyName
          u.displayName = profile.displayName
          u.gid = identifier
          u.save()
      
      #console.log(profile)
      #console.log(identifier)
      done null, profile
    
)
###

passport.use new GoogleOAuth2Strategy(
  clientID: '314528618328-v4k0lve6dl83dboanp04d85n7u9mldd6.apps.googleusercontent.com'
  clientSecret: 'zH8LPkdnVmawAePGthgJ_cRo'
  callbackURL: "http://calendarsync.dadams.c9.io/auth/google/callback",
  (accessToken, refreshToken, profile, done) ->
    #console.log accessToken
    #console.log refreshToken
    #console.log profile._json
    json = JSON.parse(JSON.stringify(profile._json))
    console.log json
    User.findOne 'gid':json.id, (err, user)->
      if !user
        console.log 'creating new user'
        u = new User()
        u.email = json.email
        u.firstname = json.given_name
        u.lastname = json.family_name
        u.displayName = json.name
        u.gid = json.id
        u.sonavalid = false
        u.sonapassword='default'
        u.accessToken = accessToken
        u.save (err)->
          console.log(err) if err
      else
        user.accessToken = accessToken
        user.save (err)->
          console.log(err) if err
        
      done err, profile
    #User.findOrCreate
      #googleId: profile.id,
      #(err, user) ->
      #done err, user

)
  
app = express()
app.configure ->
  app.set "port", process.env.PORT or 80
  app.set "views", __dirname + "/views"
  app.set "view engine", "jade"
  app.use express.favicon()
  app.use express.logger("dev")
  app.use express.cookieParser() 
  app.use express.bodyParser()
  app.use express.methodOverride()
  app.use express.session { secret: 'keyboard cat' }
  #app.use express.compiler(src: __dirname + '/public/lib', enable: ['less'] )
  #app.use express.compiler(
  #    src: __dirname + "/public"
  #    enable: ["less"])
  app.use passport.initialize()
  app.use passport.session()
  app.use app.router
  app.use express.static(path.join(__dirname, "public"))

app.configure "development", ->
  app.use express.errorHandler()

app.get "/", (req, res) ->
    res.render "index",
      user: req.user
      title: 'Calendarsync'

app.get "/account", ensureAuthenticated, (req, res) ->
  res.render "account",
    user: req.user


app.get "/auth/google", 
  passport.authenticate("google", 
    scope: ['https://www.googleapis.com/auth/calendar',
          'https://www.googleapis.com/auth/userinfo.profile',
          'https://www.googleapis.com/auth/userinfo.email']
    failureRedirect: "/"), 
  (req, res) ->
    res.redirect "/"

app.get "/auth/google/return", passport.authenticate("google",  failureRedirect: "/"), 
  (req, res) ->
    res.redirect "/manage"

app.get "/auth/google/callback", passport.authenticate("google",  failureRedirect: "/"), 
  (req, res) ->
    # Successful authentication, redirect home.
    res.redirect "/"

app.get "/logout", (req, res) ->
  req.logout()
  res.redirect "/"
  
app.get "/manage", (req, res) ->
    res.render "manage",
      user: req.user
    

       
app.post "/manage", (req, res) ->
  sonalogin = req.param 'sona_login', null
  sonapass = req.param 'sona_password', null
  #console.log sonalogin
  #console.log sonapass
  if sonapass
    req.user.sonapassword=sonapass
  if sonalogin
    req.user.sonausername=sonalogin
  req.user.save (err) ->
    console.log("error saving sona data " + err) if err
  res.redirect '/update'

app.post "/update/:index?", (req, res) ->
  calendarname = req.param 'calendar_name', null
  index = req.params.index
  calendar.listCalendarList req.user.accessToken, (err, calendarList) ->
    if err
      console.log err
    else
      calendarList.items.forEach (cal) ->
        console.log "Calendar : " + cal.id
        #console.log "index : " + index
        #console.log calendarname
        if calendarname == cal.summary
          Experiment.find 
            'experiment': req.user.experiments[index] 
            (err,result)->
              #console.log result
              for exp in result
                console.log exp
                if exp.ParticipantSigned == '1'
                  event = 
                    "status": 'confirmed'
                    "summary": exp.StudentTimeSlot
                    "description": exp.experiment
                    "start":
                        "dateTime": exp.starttime
                    "end":
                        "dateTime": exp.endtime
                        
                  console.log event
                  if exp.calendarid
                    calendar.updateEvent req.user.accessToken, cal.id, exp.calendarid,(err, event) ->
                      if err
                        console.log err
                      else
                        console.log 'updated existing event ' + event.id
                  else
                    calendar.insertEvent req.user.accessToken, cal.id,event,(err, event) ->
                      if err
                        console.log err
                      else
                        console.log 'added new event ' + event.id
                        exp.calendarid = event.id
                        #exp.save (err)->
                          #console.log err if err
                        #e = new Experiment(exp)
                        #e.save (err)->
                          #console.log err if err
                        Experiment.findByIdAndUpdate exp._id,
                          'calendarid': event.id, (err)->
                            console.log err if err
                else if exp.ParticipantSigned == '0' and exp.calendarid
                  calendar.deleteEvent  req.user.accessToken, cal.id, exp.calendarid,(err, event) ->
                    if err
                      console.log err
                    else
                      console.log 'deleted existing event ' + event.id
                      event.id = null
                      exp.save (err)->
                        console.log err if err
                        
    #Events.list
  res.redirect '/'

###
app.post "/update", (req, res) ->
  options = querystring.stringify 
    'host': 'www.googleapis.com'
    'path': '/calendar/v3/users/me/calendarList?key=zH8LPkdnVmawAePGthgJ_cRo'
    'method': 'GET'
    'authorization': 'Bearer ' + req.user.accessToken
    'referer': 'http://calendarsync.dadams.c9.io'
  console.log options
  request = https.request(options, (response) ->
    console.log "statusCode: ", response.statusCode
    console.log "headers: ", JSON.stringify(response.headers)
    response.on "data", (d) ->
      console.log d
  
  )
  request.end()
  request.on "error", (e) ->
    console.error e
  res.redirect '/'
###


app.get "/update", (req, res) ->
  child = exec 'python calendarsync.py', (error, stdout, stderr) ->
    console.log "stdout: " + stdout
    console.log "stderr: " + stderr
    console.log "exec error: " + error  if error isnt null
    User.findOne 'gid':req.user.gid, (err, user)->
      req.user = user
  if req.user.sonavalid
    res.redirect "/experiments"
  else
    res.redirect '/manage'

app.get "/experiments/:experiment", (req, res) ->
  console.log "experiment number " +  req.params.experiment
  console.log req.user.experiments[parseInt req.params.experiment] 
  Experiment.find 
    'experiment': req.user.experiments[parseInt req.params.experiment] 
    'ParticipantSigned': '1', 
    (err,result)->
      console.log 'finding experiments'
      if(err)
        console.log(err)
        res.redirect '/experiments'
      else
        res.render "calendar"
          user: req.user
          experiments: result
          title: req.user.experiments[parseInt req.params.experiment] 
          index: parseInt req.params.experiment
        
    
#app.get '/calendar', (req, res) ->
   #res.render "calendar"
   
app.get "/experiments", (req, res) ->
  if req.user.sonavalid
    res.render "experiments",
      title: 'Experiments'
      user: req.user
  else
    res.redirect '/manage'

ensureAuthenticated = (req, res, next) ->
  return next()  if req.isAuthenticated()
  res.redirect "/"


userSchema.pre "save", (next) ->
  user = this
  # only hash the password if it has been modified (or is new)
  return next()  unless user.isModified("sonapassword")
  
  # generate a salt
  salt = bcrypt.genSaltSync SALT_WORK_FACTOR
  console.log user
  encryption.encrypt user.sonapassword, salt, (encoded) ->
    #console.log 'encrypted ' + encoded
    user.sonapassword = encoded
    #console.log encoded
    user.salt = salt
    next()
      #encryption.decrypt encoded, salt, (output) ->
        #next()
    
      # override the cleartext password with the hashed one
      
      


http.createServer(app).listen app.get("port"), ->
  console.log "Express server listening on port " + app.get("port")
  
  