//
// Implementation of SahanaFox main component
//
//

const Cc = Components.classes;
const Ci = Components.interfaces;
const IDENTIFOX_UUID = "skbohra123@sahana.lk";
const CLASS_ID = Components.ID("3667967C-044D-11DE-B122-E76256D89593");
const CLASS_NAME = "Identica Notifier";
const CONTRACT_ID = "@uncryptic/identicanotifier;1";
const NETWORK_TIMEOUT_TIME = 120;
const APP_NAME = "IdentiFox";

var db = null;


/////////////////////////////////////////////////////////////////////////
//
// SahanaFox main component.
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
  _response: null,
  url: null,
  total_entries: null,
  _urllist: [],
  
  //
  // commands
  //

  initSession: function() {
	this.updatePref();
	
  },

  updatePref: function() {
    var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);
	var disable = pref.getBoolPref("extensions.identicanotifier.disable");
	if(disable=="false")
	{
    this._interval = pref.getIntPref("extensions.identicanotifier.interval") * 60 * 1000;
    // fail safe
    if (this._interval < 180 * 1000) {
      this._interval = 180 * 1000;
    }

    if (this._rateLimit) {
      this.setInterval();
    }
    var type = Components.interfaces.nsITimer.TYPE_REPEATING_SLACK;
    this.setDelayTask(this._interval,"updateDents",null,type);
	}
	else
	{
		return true;
		
	}
  },

  setNextTimer: function() {
    this._timer = this.setDelayTask(this._interval, "updateDents", null);
  },

 
 //this function convert the xml into an associative array
  
   convertXmlToArray: function(obj){
  	var parser = Components.classes["@mozilla.org/xmlextras/domparser;1"]
            .createInstance(Components.interfaces.nsIDOMParser);
	
	var xmlDoc=parser.parseFromString(obj,"text/xml");
	var response = new Array();
	
	var i = 0;
	var length = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry").length;
	
	
	while(i < length){
		try{
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("title")[0].childNodes[0].nodeValue!=null)
	var title = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("title")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("updated")[0].childNodes[0].nodeValue!=null)
	var updated = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("updated")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("title")[0].childNodes[0].nodeValue!=null)
	var title = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("title")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("id")[0].childNodes[0].nodeValue!=null)
	var id = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("id")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "areaDesc")[0].childNodes[0].nodeValue!=null)
	var area = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "areaDesc")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "status")[0].childNodes[0].nodeValue!=null);
	var status = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "status")[0].childNodes[0].nodeValue; 
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "severity")[0].childNodes[0].nodeValue!=null)
	var severity = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "severity")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "certainty")[0].childNodes[0].nodeValue!=null)
	var certainty = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "certainty")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "urgency")[0].childNodes[0].nodeValue!=null)
	var urgency = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "urgency")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "category")[0].childNodes[0].nodeValue!=null)
	var category = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "category")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "status")[0].childNodes[0].nodeValue!=null)	
	var type = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("urn:oasis:names:tc:emergency:cap:1.1", "status")[0].childNodes[0].nodeValue;
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("link")[0].getAttribute("href")!=null)
	var link = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagName("link")[0].getAttribute("href");
	if(xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("http://www.alerting.net/namespace/index_1.0", "latLonBox")[0].childNodes[0].nodeValue!=null)	
	var latlong = xmlDoc.getElementsByTagName("feed")[0].getElementsByTagName("entry")[i].getElementsByTagNameNS("http://www.alerting.net/namespace/index_1.0", "latLonBox")[0].childNodes[0].nodeValue;
	
	
		}
		catch(e)
		{
		}
	response[i]= { "title": title, 
	"updated": updated,
	"id": id,
	"area": area,
	"status": status,
	"severity": severity,
	"certainty": certainty,
	"urgency": urgency,
	"category": category,
	"type": type,
	"link": link,
	"latlong": latlong};
	
	i++;
	}
	response["total_entries"] = length;
	return response;
	
  },
  
  // this function is called when alerts are due to be updated
  updateDents: function() {
  	
  	 if(db==null){
		db = Cc["@uncryptic.com/identifox-database;1"].getService(Components.interfaces.nsIIdentiFoxDatabase);
		db.openDatabase();
	}
  var total_entries; var i =0; var url; 
  var stmt = db.prepare("SELECT * FROM subscriptions WHERE enabled = 'true' ");
  	  
    while (stmt.executeStep()){
    	url = stmt.getString(1);
    	total_entries = stmt.getString(4);
    	this._urllist.push({"url": url, "total_entries": total_entries});
    	i++;
    	
    }
    stmt.finalize();  	
  this.fetchOneByOne();  
  },
  
  //
  //sending alert notifications one by one
  //
  
  fetchOneByOne: function(){
    
	var data = this._urllist.pop();
	if(data)
	{
	this.url = data["url"];
	this.total_entries = data["total_entries"];
	
	
  	var request = Cc["@uncryptic.com/identifox-http-request;1"].createInstance(Ci.nsIIdentiFoxHttpRequest);
  	var target = this;
    request.onerror   = function(p) {target.onError(request);}
    request.ontimeout = function(p) {targetonTimeout(request);}
    request.onload    = function(p) {target.onLoad(request,target.url, target.total_entries);}
    request.setURL(this.url);
    request.asyncOpen();
	}
  },
  
  //
  //function checks for any new alert
  //
  
  getRecent: function() {
  	
  	
  	 if(db==null){
  		db = Cc["@uncryptic.com/identifox-database;1"].getService(Components.interfaces.nsIIdentiFoxDatabase);
		db.openDatabase();
  		}
  	    
     var alert = new Array();
  	 var i=0;
  	 var stmt3 = db.prepare("SELECT * FROM alerts ORDER BY updated DESC");
  	 while (stmt3.executeStep()){
  	 alert[i] = {"id": stmt3.getString(0),
  	 "title": stmt3.getString(1),
  	 "area": stmt3.getString(2),
  	 "type": stmt3.getString(3),
  	 "severity": stmt3.getString(4),
  	 "link": stmt3.getString(5),
  	 "status": stmt3.getString(6),
  	 "updated": stmt3.getString(7),
  	 "category": stmt3.getString(8),
  	 "urgency": stmt3.getString(9),
  	 "certainity": stmt3.getString(10),
  	 "longlat": stmt3.getString(11)};
  	 i++;
  	 }
    stmt3.finalize();
    this.notifyStatus("showPopup",alert)
  },

  
  markRead: function(obj) {
   
  },

  markAllRead: function() {
   
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
  // Network handler
  //
  onLoad: function(request, url, total_entries) {
  	     
   switch (Number(request.status)) 
   {
    case 400:
    
    return;
    break;

    case 401:
    break;

    case 403:
    case 404:
    case 500:
    case 502:
    case 503:
  
    break;
 
    case 200:
    case 304:
    default:
    this.notifyStatus("updateToolTip");
    var i = 0;
    var dataArray = this.convertXmlToArray(request.responseText);
	var messages = new Array();
     var pref = Components.classes['@mozilla.org/preferences-service;1']
      .getService(Components.interfaces.nsIPrefBranch);

    
     var diff = dataArray["total_entries"]-total_entries;
        if(diff< 0)
        {
        	var stmt1 = db.prepare("UPDATE subscriptions SET total_entries = ?1 WHERE feed_url = ?2");
        	
      		stmt1.bindStringParameter( 0, 0);
      		stmt1.bindStringParameter(1, url);
        
   		try {
   		stmt1.execute();
   		}
		   		   
   		catch (e) {
    	dump("Insert DB error: " + e.message);
  	    }
    	
        }
        if(diff>0) 	
        { 
        	var stmt1 = db.prepare("UPDATE subscriptions SET total_entries = ?1 WHERE feed_url = ?2");
        	
      		stmt1.bindStringParameter( 0, dataArray["total_entries"]);
      		stmt1.bindStringParameter(1, url);
        
   		try {
   		stmt1.execute();
   		}
		   		   
   		catch (e) {
    	dump("Insert DB error: " + e.message);
  	    }
    	stmt1.finalize();
        var k = 0;
        while(k < diff)
        	{
        		 
        var stmt2 = db.prepare("INSERT INTO alerts VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12)");	
		stmt2.bindStringParameter(0, dataArray[k]["id"]);
		stmt2.bindStringParameter(1, dataArray[k]["title"]);
		stmt2.bindStringParameter(2, dataArray[k]["area"]);
		stmt2.bindStringParameter(3, dataArray[k]["type"]);
		stmt2.bindStringParameter(4, dataArray[k]["severity"]);
		stmt2.bindStringParameter(5, dataArray[k]["link"]);
		stmt2.bindStringParameter(6, dataArray[k]["status"]);
		stmt2.bindStringParameter(7, dataArray[k]["updated"]);
		stmt2.bindStringParameter(8, dataArray[k]["category"]);
		stmt2.bindStringParameter(9, dataArray[k]["urgency"]);
		stmt2.bindStringParameter(10, dataArray[k]["certainty"]);
		stmt2.bindStringParameter(11, dataArray[k]["latlong"]);
		try {
   		stmt2.execute();
   		}
		   		   
   		catch (e) {
    	dump("Insert DB error: " + e.message);
  	    }
    	stmt2.finalize();
		messages[k] = dataArray[k];
        
        k++;
        
        }	
        this.playSound();
        this.notifyStatus("updateBalloon",messages);
        	
        	}
      }
      
  	 
	this.fetchOneByOne();
      
  	
  	
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
