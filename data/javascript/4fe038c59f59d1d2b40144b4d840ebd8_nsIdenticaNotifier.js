//
// Implementation of IdentiFox main component
//
//
const Cc = Components.classes;
const Ci = Components.interfaces;
const IDENTIFOX_UUID = "identicanotifier@uncryptic.com";
const CLASS_ID = Components.ID("3667967C-044D-11DE-B122-E76256D89593");
const CLASS_NAME = "Identica Notifier";
const CONTRACT_ID = "@uncryptic/identicanotifier;1";

const MAX_STORED_MESSAGES = 40;

//
// cached sql statements
// 
var insert_status_stmt = null;
var find_status_stmt = null;

const IDENTICA_API_URL = "https://identi.ca/api/";
const NETWORK_TIMEOUT_TIME = 120;
const APP_NAME = "IdentiFox";

/////////////////////////////////////////////////////////////////////////
//
// Identica status model
//
function Status(json)
{
  for (var i in json) {
    this[i] = json[i];
  }
}

function stringFromJson(json)
{
    jsonstring = '';

    for(var i in json) {
	jsonstring = jsonstring + i +' : '+json[i]+ " / ";
    }
    return jsonstring;
}

function RestoreStatusesFromDB(db, type, count) 
{
  var results = [];

  var stmt = db.prepare("SELECT * FROM statuses WHERE type = ?1 ORDER BY id DESC LIMIT ?2");

  stmt.bindInt32Parameter(0, type);
  stmt.bindInt32Parameter(1, count);

  while (stmt.executeStep()) {
    var status = new Status();
    status.id                      = stmt.getInt64(0);
    status.type                    = stmt.getInt32(1);
    status.user_id                 = stmt.getInt32(2);
    status.text                    = stmt.getString(3);
    status.created_at              = stmt.getInt32(4);
    status.source                  = stmt.getString(5);
    status.favorited               = stmt.getInt32(6);
    status.truncated               = stmt.getInt32(7);
    status.in_reply_to_status_id   = stmt.getInt64(8);
    status.in_reply_to_user_id     = stmt.getInt32(9);
    status.in_reply_to_screen_name = stmt.getString(10);

    for (var i in status) {
      //      dump(i + ":" + status[i] + "\n");
    }
    
    results.push(status);
  }
  stmt.finalize();

}

Status.prototype = {
  insertIntoDB: function(db) {
    if (insert_status_stmt == null) {
      insert_status_stmt = db.prepare("INSERT INTO statuses VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)");
    }
    var stmt = insert_status_stmt;
    stmt.bindInt64Parameter(  0, this.id);
    stmt.bindInt32Parameter(  1, this.type);
    stmt.bindInt32Parameter(  2, this.user_id);
    stmt.bindStringParameter( 3, this.text);
    stmt.bindInt32Parameter(  4, new Date(this.created_at));
    stmt.bindStringParameter( 5, this.source);
    stmt.bindInt32Parameter(  6, this.favorited);
    stmt.bindInt32Parameter(  7, this.truncated);
    stmt.bindInt64Parameter(  8, this.in_reply_to_status_id);
    stmt.bindInt32Parameter(  9, this.in_reply_to_user_id);
    stmt.bindStringParameter(10, this.in_reply_to_screen_name);
    try {
      stmt.execute();
    }
    catch (e) {
      dump("Insert DB error: " + e.message);
    }
    stmt.reset();
  },

  deleteFromDB: function() {
    var stmt = this.DB().prepare("DELETE FROM statuses WHERE id = ?1");
    stmt.bindInt64Parameter(0, id);
    stmt.exec();
    stmt.finalize();
  },
};

//
// Identica direct message model
//
function Message(json)
{
  for (var i in json) {
    this[i] = json[i];
  }
}

/////////////////////////////////////////////////////////////////////////
//
// IdentiFox main component.
//
function IdenticaNotifier() {
  var obs = Components.classes["@mozilla.org/observer-service;1"]
    .getService(Components.interfaces.nsIObserverService);

  obs.addObserver(this, "xpcom-shutdown", false);
  obs.addObserver(this, "identicanotifier-command", false);

  // Setup sound notification
  this._sound = Components.classes["@mozilla.org/sound;1"].createInstance(Components.interfaces.nsISound);
}

// This is the implementation of your component.
IdenticaNotifier.prototype = {
  _timer: null,
  _accounts: [],
  _sessions: [],
  _newMessages: [],
  _allMessages: {},
  _tinyURLs: [],
  _decodedTinyURLs: [],
  _getDirectMessage: true,

  //
  // commands
  //
  createRequest: function(func, param, isPost) {
    var ns = func.split(".");
    var requestURL = "";
    var requestParam = "";

    var request = Cc["@uncryptic.com/identifox-http-request;1"].createInstance(Ci.nsIIdentiFoxHttpRequest);

    if (ns.length > 1) {
      request.callback = ns[0] + "_" + ns[1];
      requestURL       = ns.join("/");
    }
    else {
      request.callback = func;
      requestURL       = func;
    }

    requestURL = IDENTICA_API_URL + requestURL + ".json";

    if (!param) param = {};

    for (var attr in param) {
      if (requestParam) requestParam += "&";
      requestParam += attr + "=" + encodeURIComponent(param[attr]);
    }

    // Setup callbacks
    //
    request.setBasicAuth(this._user, this._pass);
    //request.delegate = this;
    var target = this;
    request.onload    = function(p) {target.onLoad(request);}
    request.onerror   = function(p) {target.onError(request);}
    request.ontimeout = function(p) {target.onTimeout(request);}
    
    // send async request
    if (isPost) {
      request.setURL(requestURL);
      request.setPostData(requestParam);
    }
    else {
      request.setURL(requestURL + '?' + requestParam);
    }

    this.log(requestURL);
    request.asyncOpen();

    this.reportError(requestURL);

    return request;
  },

  get: function(method, params) {
    if (this._user == null) {
      this.reportError("No account information");
      return;
    }
    this._req = this.createRequest(method, params, false);
  },

  post: function(method, params) {
    if (this._user == null) {
      this.reportError("No account information");
      return;
    }
    this.createRequest(method, params, true);
  },

  initSession: function(user) {


    if (this._accounts[this._user] == null || this._accounts[this._user] == 'undefined') {
      if (this._timer) {
        this._timer.cancel();
      }

      this._user = user.user;
      this._pass = user.pass;

      this.updatePref();
      this.verifyCredentials();
    }
    else {
      // Send username and turn on identifox icon
      this.notifyStatus("updateUsername", this._accounts[this._user]);
    }

    if (this._db == null) {
      this._db = Cc["@uncryptic.com/identifox-database;1"].getService(Components.interfaces.nsIIdentiFoxDatabase);
      this._db.openDatabase();
      RestoreStatusesFromDB(this._db, 0, 20);
    }

  },

  updatePref: function() {
    var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);

    this._interval = pref.getIntPref("extensions.identicanotifier.interval") * 60 * 1000;
    // fail safe
    if (this._interval < 180 * 1000) {
      this._interval = 180 * 1000;
    }

    if (this._rateLimit) {
      this.setInterval();
    }

    var session = pref.getCharPref("extensions.identicanotifier.session").split(/;/);
    for (var i in session) {
      var ids = session[i].split(/,/);
      if (ids[0]) {
        this._sessions[ids[0]] = {
          timeline: ids[1] || 0,
          messages: ids[2] || 0,
          replies:  ids[3] || 0,
        };
      }
    }
  },

  setNextTimer: function() {
    this._timer = this.setDelayTask(this._interval, "updateDents", null);
  },

  verifyCredentials: function() {
    this.get("account.verify_credentials");
  },

  updateDents: function() {
    if (this._user == null) return;

    if (this._timer) {
      this._timer.cancel();
    }
    this._newMessages = [];
    this.get("statuses.friends_timeline");
  },

  getReplies: function() {
    this.get("statuses.replies");
  },

  getDirectMessages: function() {
    if (this._getDirectMessage) {
      this.get("direct_messages");
    }
    else {
      this.updateTimeline();
    }
    this._getDirectMessage = !this._getDirectMessage;
  },

  getRecent: function(obj) {
    var type = obj.type;
    if (this._accounts[this._user]) {
      var msg = this.getNumUnread();
      msg['msgs'] = this._accounts[this._user][type];
      msg['type'] = type;
      this.notifyStatus("showPopup", msg);
    }
  },

  getNumUnread: function() {
    if (!this._accounts[this._user]) return 0;

    var result = {};
    var types = ['timeline', 'replies', 'messages'];
    for (var i in types) {
      result[types[i]] = this.countUnread(this._accounts[this._user][types[i]]);
    }
    return result;
  },

  getUnreadCount: function() {
    var ret = this.getNumUnread();
    var unread = 0;
    for (var i in ret) {
      unread += ret[i];
    }
    return unread;
  },

  markRead: function(obj) {
    var type = obj.type;
    for (var i in this._accounts[this._user][type]) {
      this._accounts[this._user][type][i].unread = false;
    }
  },

  markAllRead: function() {
    var types = ['timeline', 'replies', 'messages'];
    for (var i in types) {
      this.markRead({type:types[i]});
    }
  },

  countUnread: function(msgs) {
    var count = 0;
    var username = this._accounts[this._user].screen_name.toLowerCase();
    for (var i in msgs) {
      var user = msgs[i].user ? msgs[i].user : msgs[i].sender;
      try {
        if (msgs[i].unread && user && 
            user.screen_name.toLowerCase() != username) {
          ++count;
        }
      }
      catch (e) {}
    }
    return count;
  },

  sendMessage: function(msg) {
    var arr = /^d\s+(\S+)\s+(.*)/(msg.status);
    if (arr && arr.length == 3) {
      this.post("direct_messages.new", {user: arr[1], text: arr[2]});
    }
    else {
      msg.source = APP_NAME;
      var status = {status:msg.status, source:APP_NAME};
      if (msg.inReplyTo) {
        status["in_reply_to_status_id"] = msg.inReplyTo;
      }
      this.post("statuses.update", status);
    }
  },

  setFavorite: function(msg) {
    this.post("favorites."  + msg.method + "." + msg.id);
  },

  deleteDent: function(obj) {
    if (obj.type == 'timeline' || obj.type == 'replies') {
      this.post("statuses.destroy", {id:obj.id});
    }
    else {
      this.post("direct_messages.destroy", {id:obj.id});
    }
  },

  changeAccount: function(account) {

    // reset session
    this.logout();
    
    if (!this._accounts[account.user]) {
      this.initSession(account);
    }
    else {
      this._user = account.user;
      this._pass = account.pass;

      this.notifyStatus("updateUsername", {screen_name: this._accounts[account.user].screen_name,
            profile_image_url: this._accounts[account.user].profile_image_url});
      this.notifyStatus("accountChanged");
      this.updateDents();
    }
  },

  logout: function() {
    if (this._timer) {
      this._timer.cancel();
    }

    if (this._req) {
      this._req.abort();
    }

    // save last access time
    if (this._accounts[this._user]) {
      this._accounts[this._user].lastAccess = this._lastAccess;
    }

    this._user = null;
    this._pass = null;
    this._hasError = false;
    this._lastAccess  = {};
    this._allMessages = {}
    this._getDirectMessage = true;
  },

  //
  // Private methods.
  //
  destroy: function(e) {
    if (this._timer) {
      this._timer.cancel();
      this._timer = null;
    }
    if (this._req) {
      this._req.abort();
    }

    var obs = Components.classes["@mozilla.org/observer-service;1"]
      .getService(Components.interfaces.nsIObserverService);

    obs.removeObserver(this, "xpcom-shutdown");
    obs.removeObserver(this, "identicanotifier-command");

    if (this._db) {
      this._db.close();
    }
 },

  sortById: function(a, b) {
    return b.id - a.id;
  },

  sortByDate: function(a, b) {
    var ta = new Date(a.created_at);
    var tb = new Date(b.created_at);
    return tb - ta;
  },

  retrieveTimeline: function(obj, req, method) {
    if (obj) {
      this.convertTinyURL(obj);

      var hash = {};

      var stored = this._accounts[this._user][method];
      
      // Avoid duplicate messages.
      for (var i in stored) {
        hash[stored[i].id] = 1;
      }

      // Added 'unread' flag if the message is new in any of stored messages
      for (var i in obj) {
        if (typeof obj[i].id != 'undefined' && 
            !hash[obj[i].id]) {

          var status = new Status(obj[i]);
          //status.insertIntoDB(this._db);

          if (obj[i].id > this._sessions[this._user][method] && 
              !this._allMessages[obj[i].id]) {
            obj[i].unread = true;
            this._newMessages.push(obj[i]);
          }
          stored.unshift(obj[i]);
        }
        try {
          this._allMessages[obj[i].id] = 1;
        }
        catch (e) {}
      }
      stored.sort(this.sortByDate);

      while (stored.length > MAX_STORED_MESSAGES) {
        if (!stored[stored.length - 1].unread) {
          stored.pop();
        }
        else {
          break;
        }
      }
    }
    if (req.date) {
      this._lastAccess[req.callback] = req.date;
    }
  },

  updateTimeline: function() {
    if (this._newMessages.length) {
      // Play sound if this window is active
      this.playSound();
      this.notifyStatus("updateFriendsTimeline", this._newMessages);
    }
    else {
      if (!this._hasError) {
        this.notifyStatus("noUpdate");
      }
    }
    // reset error flag
    this._hasError = false;

    this.decodeTinyURL();
    this.storeSession();

    this._newMessages = [];
    this.setNextTimer();
  },

  playSound: function() {
    var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);

    if (pref.getBoolPref("extensions.identicanotifier.sound")) {

      try {
        var IOService = Components.classes['@mozilla.org/network/io-service;1'].getService(Components.interfaces.nsIIOService);
        var localFile = Components.classes['@mozilla.org/file/local;1'].createInstance(Components.interfaces.nsILocalFile);
        var url = pref.getCharPref("extensions.identicanotifier.soundFile");
      
        localFile.initWithPath(url);

        this._sound.play(IOService.newFileURI(localFile));
      }
      catch (e) {
      }
    }
  },

  //
  // Handling tinyURL
  //
  getDecodedTinyURL: function(url) {
    return this._decodedTinyURLs[url];
  },

  convertTinyURL: function(msg) {
    var re = /http:\/\/(tinyurl.com|is.gd|bit.ly)\/\w+/;

    for (var i in msg) {

      var text = msg[i].text;
      while(re.exec(text)) {
        var url = RegExp.lastMatch;
        text = RegExp.rightContext;
        this._tinyURLs.push(url);
      }
    }
  },

  decodeTinyURL: function() {

    while (1) {

      if (this._tinyURLs.length == 0) {
        return;
      }
      var url = this._tinyURLs[0];

      if (!this._decodedTinyURLs[url]) {
        break;
      }
      this._tinyURLs.shift();
    }

    var target = this;
    var req = Cc["@uncryptic.com/identifox-http-request;1"].createInstance(Ci.nsIIdentiFoxHttpRequest);

    req.setURL(url);
    req.setRedirectLimitation(0);
    req.onerror = function(e) {target.onErrorDecodeTinyURL(req, url);};
    req.asyncOpen();
  },

  onErrorDecodeTinyURL: function(req, url) { 
    var loc = req.httpChannel().getResponseHeader("Location");
    this._decodedTinyURLs[url] = loc;
    this._tinyURLs.shift();
    this.setDelayTask(1 * 1000, "decodeTinyURL", null);
  },

  storeSession: function() {

    var account = this._accounts[this._user];

    var types = ['timeline', 'replies', 'messages'];
    for (var i in types) {
      if (account[types[i]].length) {
        this._sessions[this._user][types[i]] = account[types[i]][0].id;
      }
    }

    var storage = [];

    for (var i in this._sessions) {
      storage.push([i, 
                    this._sessions[i].timeline,
                    this._sessions[i].messages, 
                    this._sessions[i].replies].join(','));
    }
    var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);

    pref.setCharPref("extensions.identicanotifier.session", storage.join(';'));
  },

  deleteMessage: function(obj, type) {
    var msgs = this._accounts[this._user][type];
    for (var i = 0; i < msgs.length; ++i) {
      if (msgs[i].id == obj.id) {
        this._accounts[this._user][type].splice(i, 1);
        break;
      }
    }
  },

  // Network handler
  //
  onLoad: function(req) {
    if (this._user == null) {
      this.reportError("No account information");
      return;
    }

    switch (Number(req.status)) {
    case 400:
      this.rateLimitExceeded(req);
      break;

    case 401:
      if (req.callback == "statuses_friends_timeline" ||
          !this.dispatchError(req)) {
        this._accounts[this._user] = null;
        this._user = null;
        this._pass = null;
        this.notifyStatus("authFail");
      }
      break;

    case 403:
    case 404:
    case 500:
    case 502:
    case 503:
      this.handleError(req, "Identica server responded with an error (" + req.status + ") " + req.__url);
      break;
 
    case 200:
    case 304:
    default:
      var resp = null;
      if (!req.responseText.match(/^\s*$/)) {
        var JSONString = req.responseText.replace(/Couldn\'t find Status with ID=\d+,/, '');
        try {
          var resp = eval('(' + JSONString + ')');
        }
        catch (e) {
          this.reportError("An error occurred while requesting " + req.__url);
          this.log("Response text: " + e.message);
          this.handleError(req, "Can't parse JSON. Identica server responded with an error.");
        }
      }

      if (resp == null || (resp && resp.error)) {
        if (resp && resp.error) {
          this.notifyStatus("internalError", resp.error);
          this.setNextTimer();
        }
        else {
          this.handleError(req, "Identica server responded with an error");
        }
      }
      else {
        this[req.callback](resp.statuses ? resp.statuses : resp, req);
      }
      break;
    }
  },

  handleError: function(req, msg) {
    if (this[req.callback + "_error"]) {
      this[req.callback + "_error"](req);
    }
    else {
      if (!this.dispatchError(req)) {
        this.notifyStatus("internalError", msg);
        this.setNextTimer();
      }
    }
  },

  dispatchError: function(req) {

    this._hasError = true;

    switch (req.callback) {
    case "direct_messages":
      this.updateTimeline();
      break;
    
    case "statuses_replies":
      this.getDirectMessages();
      break;
    
    case "statuses_friends_timeline":
      this.getReplies();
      break;
    
    default:
      return false;
    }
    return true;
  },

  onError: function(req) {

    this.notifyStatus("internalError", "Network error with " + req.callback);
    if (this.dispatchError(req)) return;

    if (this._timer) {
      this._timer.cancel();
    }
    this.setNextTimer();
  },

  onTimeout: function (req) {
    this.notifyStatus("internalError", "Request timeout with " + req.callback);
    if (this.dispatchError(req)) return;
    this.setNextTimer();
  },

  rateLimitExceeded: function(req) {
    try {
      var resp = eval('(' + req.responseText + ')');
      this.notifyStatus("internalError", resp.error);
    }
    catch (e) {}

    this.get('account.rate_limit_status');
  },

  setInterval: function() {
    var interval = this._rateLimit / 3;
    interval = Math.ceil((60 / interval) * 60 * 1000);
    
    if (this._interval < interval) {
      this._interval = interval;
    }

    // fail safe
    if (this._interval < 180 * 1000) {
      this._interval = 180 * 1000;
    }
  },

  //
  // IdenticaAPI callbacks.
  //
  users_show: function(obj, req) {
    this._accounts[this._user] = obj;
    this._accounts[this._user].timeline = [];
    this._accounts[this._user].messages = [];
    this._accounts[this._user].replies  = [];

    if (!this._sessions[this._user]) {
      this._sessions[this._user] = {timeline: 0, messages: 0, replies: 0};
    }

    this.notifyStatus("updateUsername", obj);
	
    this.updateDents();
  },

  account_verify_credentials: function(obj, req) {
    this.get("users.show." + this._user);
  },

  account_rate_limit_status: function(obj, req) {
    this._rateLimit = obj.hourly_limit;
    this.log("Rate limit is " + this._rateLimit + " requests per hour");
    if (this._rateLimit <= 3) {
      this._rateLimit = 3;
    }

    this.setInterval();

    var nextTime = new Date(obj.reset_time) - Date.now();
    if (nextTime < 180 * 1000) {
      nextTime = 180 * 1000;
    }
    else if (nextTime > 3600 * 1000) {
      nextTime = 3600 * 1000;
    }
    this._timer = this.setDelayTask(nextTime, "updateDents", null);

    var msg = "Identica lower rate limit (less than " + this._rateLimit + 
              " requests per hour), request interval has been changed to " + this._interval / 60 / 1000 + " minutes"; 
    this.notifyStatus("showMessage", msg);
    this.log(msg);
    this.log("Auto rate limit control:" + obj.remaining_hits + "/" + obj.hourly_limit + 
             ", reset time: " + obj.reset_time + "(" + Math.ceil(nextTime / 1000 / 60) + " minutes)");
  },

  direct_messages: function(obj, req) {
    this.retrieveTimeline(obj, req, "messages");
    this.updateTimeline();
  },

  statuses_replies: function(obj, req) {
    this.retrieveTimeline(obj, req, "replies");
    this.getDirectMessages();
  },

  statuses_friends_timeline: function(obj, req) {
    this.retrieveTimeline(obj, req, "timeline");
    this.getReplies();
  },

  statuses_user_timeline: function(obj) {
    try {
      this._accounts[this._user].username = obj[0].user.screen_name;
      this.notifyStatus("updateUsername", obj[0].user.screen_name);
      this.updateDents();
    }
    catch (e) {
      this.setNextTimer();
    }
  },

  statuses_destroy: function(obj) {
    this.notifyStatus("messageDeleted", {id:obj.id});
    this.deleteMessage(obj, 'timeline');
    this.deleteMessage(obj, 'replies');
  },

  favorites_destroy: function(obj) {
    this.toggleFavorite(obj.id, null);
    this.notifyStatus("updateFavorite", {id: obj.id, state:null});
  },

  favorites_create: function(obj) {
    this.toggleFavorite(obj.id, true);
    this.notifyStatus("updateFavorite", {id: obj.id, state:true});
  },

  toggleFavorite: function(id, flag) {
    var types = ['timeline', 'replies'];
    for (var i in types) {
      var msgs = this._accounts[this._user][types[i]];
      for (var j in msgs) {
        if (msgs[j].id == id) {
          msgs[j].favorited = flag;
          break;
        }
      }
    }
  },

  statuses_update: function(obj, req) {
    if (obj.id) {
      this._accounts[this._user].timeline.unshift(obj);
      this.notifyStatus("sentMessage", obj);
    }
    else {
      this.notifyStatus("errorOnSendMessage");
    }
  },

  statuses_update_error: function(req) {
    this.reportError("Send error occurred: " + req.status);
    this.log(req.responseText);
    this.notifyStatus("errorOnSendMessage");
  },

  statuses_update_timeout: function() {
    this.reportError("Send timeout occurred");
    this.notifyStatus("errorOnSendMessage");
  },

  direct_messages_new: function(obj, req) {
    if (obj.id) {
      this.notifyStatus("sentMessage", obj);
    }
    else {
      this.notifyStatus("errorOnSendMessage");
    }
  },

  direct_messages_destroy: function(obj) {
    this.notifyStatus("messageDeleted", {id:obj.id});
    this.deleteMessage(obj, 'messages');
  },

  direct_messages_new_error: function(req) {
    this.reportError("Send error occurred: " + req.status);
    this.log(req.responseText);
    this.notifyStatus("errorOnSendMessage");
  },

  direct_messages_new_timeout: function() {
    this.reportError("Send timeout occurred");
    this.notifyStatus("errorOnSendMessage");
  },

  // Utilities
  //
  handleCommand: function(data) {
    var msg = eval('(' + data + ')');
    this[msg.command](msg);
  },

  notifyStatus: function(sts, obj) {

    var msg = {"state": sts, "data": obj};

    Components.classes["@mozilla.org/observer-service;1"]
        .getService(Components.interfaces.nsIObserverService)
    .notifyObservers(null, "identicanotifier-status", msg.toSource());
  },

  setDelayTask: function(delay, func, data, type) {
    var timer = Components.classes["@mozilla.org/timer;1"] 
      .createInstance(Components.interfaces.nsITimer); 

    var target = this;

    if (type == null) {
      type = Components.interfaces.nsITimer.TYPE_ONE_SHOT;
    }

    timer.initWithCallback({
      notify: function() {
          target[func](data);
        }
      },
      delay,
      type);
    return timer;
  },

  reportError: function(msg) {
    var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);

    if (pref.getBoolPref("extensions.identicanotifier.debug")) {
      Components.utils.reportError(msg);
    }
    this.log(msg);
  },

  log: function(msg) {
    var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);

    if (pref.getBoolPref("extensions.identicanotifier.debug")) {
      if (this._console == null) 
        this._console = Components.classes["@mozilla.org/consoleservice;1"].getService(Components.interfaces.nsIConsoleService);
      this._console.logStringMessage(msg);
      dump(msg + "\n");
    }
  },

  // for nsISupports
  //
  QueryInterface: function(aIID) {
    // add any other interfaces you support here
    if (!aIID.equals(Components.interfaces.nsISupports) && 
        !aIID.equals(Components.interfaces.nsIObserver) &&
        !aIID.equals(Components.interfaces.nsIIdentiFoxHttpRequestCallback) &&
        !aIID.equals(Components.interfaces.nsIIdenticaNotifier))
        throw Components.results.NS_ERROR_NO_INTERFACE;
    return this;
  },

  // for nsIObserver
  //
  observe: function(subject, topic, data) { 
    switch (topic) {

    case "identicanotifier-command":
      this.handleCommand(data);
      break;

    case "xpcom-shutdown":
      this.destroy();
      break;
    }
  }
}

//=================================================
// Note: You probably don't want to edit anything
// below this unless you know what you're doing.
//
// Singleton
var gIdenticaNotifier = null;

// Factory
var IdenticaNotifierFactory = {
  createInstance: function (aOuter, aIID)
  {
    if (aOuter != null)
      throw Components.results.NS_ERROR_NO_AGGREGATION;
    if (gIdenticaNotifier === null) {
      gIdenticaNotifier = new IdenticaNotifier().QueryInterface(aIID);
    }
    return gIdenticaNotifier;
  }
};

// Module
var IdenticaNotifierModule = {
  registerSelf: function(aCompMgr, aFileSpec, aLocation, aType) {
    aCompMgr = aCompMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    aCompMgr.registerFactoryLocation(CLASS_ID, CLASS_NAME, CONTRACT_ID, aFileSpec, aLocation, aType);

    Components.classes["@mozilla.org/categorymanager;1"]
      .getService(Components.interfaces.nsICategoryManager)
        .addCategoryEntry("app-startup", 
                          CLASS_NAME,
                          "service," + CONTRACT_ID,
                          true, true);
  },

  unregisterSelf: function(aCompMgr, aLocation, aType)
  {
    aCompMgr = aCompMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    aCompMgr.unregisterFactoryLocation(CLASS_ID, aLocation);        

    Components.classes["@mozilla.org/categorymanager;1"]
      .getService(Components.interfaces.nsICategoryManager)
        .deleteCategoryEntry("app-startup", 
                             CLASS_NAME,
                             true);
  },
  
  getClassObject: function(aCompMgr, aCID, aIID)  {
    if (!aIID.equals(Components.interfaces.nsIFactory))
      throw Components.results.NS_ERROR_NOT_IMPLEMENTED;

    if (aCID.equals(CLASS_ID))
      return IdenticaNotifierFactory;

    throw Components.results.NS_ERROR_NO_INTERFACE;
  },

  canUnload: function(aCompMgr) { return true; }
};

//module initialization
function NSGetModule(aCompMgr, aFileSpec) { return IdenticaNotifierModule; }

