/* 
	MANAGE THE BRANCHS, THE REFERENCES TO "PREF" (SHARED BY ALL THE WINDOWS) AND 
	DISPATCH THE EVENT ONPREFERENCESET FOR N ADD-ONS
	Tito Bouzout <extensiondevelopment@gmail.com>
*/

const nsIPreferencesManager = Components.interfaces.nsIPreferencesManager;
const nsISupports = Components.interfaces.nsISupports;
const nsIObserver = Components.interfaces.nsIObserver;

const CLASS_ID = Components.ID("{aa5d0540-9c94-11df-981c-0800200c9a66}");
const CLASS_NAME = "Manage preferences and the preferences observer of a browser instance for N add-ons";
const CONTRACT_ID = "@particle.universe.tito/PreferencesManager;7";

function PreferencesManager(){this.wrappedJSObject = this;}

PreferencesManager.prototype = 
{
	classID : CLASS_ID,
	classDescription : CLASS_NAME,
	contractID : CONTRACT_ID,
	
	debugingThisFile : false, 
	consoleService : Components.classes["@mozilla.org/consoleservice;1"].
						getService(Components.interfaces.nsIConsoleService),
	branchs : [],
	prefs : [],
	observers : [],
	observe: function(aSubject, aTopic, aData)
	{
	},
	//returns a reference for a branch ( it creates one if not exists )
	getBranch: function(anExtension)
	{
		//this.dump('getBranch:anExtension:'+anExtension);
		
		if(this.branchs[anExtension])
		{
			//this.dump('getBranch:anExtension:'+anExtension+':exists');
		}
		else
		{
			//this.dump('getBranch:anExtension:'+anExtension+':noExists');
			
			this.branchs[anExtension] = Components.classes["@mozilla.org/preferences-service;1"].
											getService(Components.interfaces.nsIPrefService).getBranch("extensions."+anExtension+".");
			this.branchs[anExtension].QueryInterface(Components.interfaces.nsIPrefBranch2);

			this.observers[anExtension] = 
			{
				//called when the preferences for anExtension change
					observe : function(aSubject, aTopic, aPreferenceName)
					{
						var PreferencesManager = Components.classes['@particle.universe.tito/PreferencesManager;7']
														.getService().wrappedJSObject;

						if(aTopic == "nsPref:changed")
						{
							PreferencesManager.loadChangedPreference(anExtension, aPreferenceName);
						}
					}
			}
			this.prefs[anExtension] = [];
			this.addObserver(anExtension);
		}
		return this.branchs[anExtension];
	},
	getDefaultBranch: function(anExtension)
	{
		return Components.classes["@mozilla.org/preferences-service;1"].
			getService(Components.interfaces.nsIPrefService).getDefaultBranch("extensions."+anExtension+".");	
	},
	//returns a reference for an extension preference
	getPref: function(anExtension)	
	{
		//this.dump('getPref:anExtension:'+anExtension);
		return this.prefs[anExtension];
 	},
	//updates a preference that change the value and dispatch the event onPreferenceSet
	loadChangedPreference : function (anExtension, aPreferenceName)
	{
		//this.dump('loadChangedPreference:anExtension:'+anExtension+':aPreferenceName:'+aPreferenceName);
			
			try
			{
				this.prefs[anExtension][aPreferenceName] = this.branchs[anExtension].getBoolPref(aPreferenceName);
			}
			catch(e)
			{
				try
				{
					this.prefs[anExtension][aPreferenceName] = this.branchs[anExtension].getCharPref(aPreferenceName);
				}
				catch(e)
				{
					try
					{
						this.prefs[anExtension][aPreferenceName] = this.branchs[anExtension].getIntPref(aPreferenceName);
					}
					catch(e)
					{
						//this.dump('loadChangedPreference:anExtension:'+anExtension+':preferenceNotFound:'+aPreferenceName+':aType:'+this.branchs[anExtension].getPrefType(aPreferenceName));
					}
				}
			}

			//try to use the most recent window
			var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]  
						.getService(Components.interfaces.nsIWindowMediator);  
			var win = wm.getMostRecentWindow('navigator:browser');
	
			//waiting for the extension to load
			if(win && (anExtension in win) && win[anExtension].extensionHasBeenLoaded)
			{
				win[anExtension].dispatchEvent('onPreferenceSet', aPreferenceName, this.prefs[anExtension][aPreferenceName]);
			}
			else
			{
				//try in other window
				var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
							.getService(Components.interfaces.nsIWindowMediator);
				var enumerator = wm.getEnumerator('navigator:browser');
				
				while(enumerator.hasMoreElements())
				{
					var win = enumerator.getNext();// win is [Object ChromeWindow] (just like window), do something with it
	
					//waiting for the extension to load
					if(win && (anExtension in win) && win[anExtension].extensionHasBeenLoaded)
					{
						win[anExtension].dispatchEvent('onPreferenceSet', aPreferenceName, this.prefs[anExtension][aPreferenceName]);
						return;
					}
				}
			}
	},
	//adds the observer looking for changes in preferences
	addObserver:function(anExtension)
	{
		this.removeObserver(anExtension);
		//this.dump('addObserver:anExtension:'+anExtension);
		this.branchs[anExtension].addObserver("", this.observers[anExtension], false);
	},
	//removes the observer
	removeObserver:function(anExtension)
	{
		//this.dump('removeObserver:anExtension:'+anExtension);
		try{this.branchs[anExtension].removeObserver("", this.observers[anExtension], false);}catch(e){}
	},
	//output to the console messages
	dump : function(something)
	{
		if(this.debugingThisFile)
		{
			if(typeof(something) == 'string' || typeof(something) == 'number')
				this.consoleService.logStringMessage('XPCOM:PreferencesManager:'+something);
			else if(typeof(something) == 'undefined' )
				this.consoleService.logStringMessage('XPCOM:PreferencesManager:undefined');
			else if(something == null)
				this.consoleService.logStringMessage('XPCOM:PreferencesManager:null');
			else
				this.consoleService.logStringMessage('XPCOM:PreferencesManager:'+something.toSource());
		}
	},


   QueryInterface: function(aIID)
  {
    if (
		!aIID.equals(nsIPreferencesManager)
		&& !aIID.equals(nsISupports)
		&& !aIID.equals(nsIObserver)
		)
      throw Components.results.NS_ERROR_NO_INTERFACE;
    return this;
  }

};

/***********************************************************
class factory

This object is a member of the global-scope Components.classes.
It is keyed off of the contract ID. Eg:


***********************************************************/
var PreferencesManagerFactory = {
  createInstance: function (aOuter, aIID)
  {
    if (aOuter != null)
      throw Components.results.NS_ERROR_NO_AGGREGATION;
    return (new PreferencesManager()).QueryInterface(aIID);
  }
};

/***********************************************************
module definition (xpcom registration)
***********************************************************/
var PreferencesManagerModule = {
  registerSelf: function(aCompMgr, aFileSpec, aLocation, aType)
  {
    aCompMgr = aCompMgr.
        QueryInterface(Components.interfaces.nsIComponentRegistrar);
    aCompMgr.registerFactoryLocation(CLASS_ID, CLASS_NAME, 
        CONTRACT_ID, aFileSpec, aLocation, aType);
  },

  unregisterSelf: function(aCompMgr, aLocation, aType)
  {
    aCompMgr = aCompMgr.
        QueryInterface(Components.interfaces.nsIComponentRegistrar);
    aCompMgr.unregisterFactoryLocation(CLASS_ID, aLocation);        
  },
  
  getClassObject: function(aCompMgr, aCID, aIID)
  {
    if (!aIID.equals(Components.interfaces.nsIFactory))
      throw Components.results.NS_ERROR_NOT_IMPLEMENTED;

    if (aCID.equals(CLASS_ID))
      return PreferencesManagerFactory;

    throw Components.results.NS_ERROR_NO_INTERFACE;
  },

  canUnload: function(aCompMgr) { return true; }
};

/***********************************************************
module initialization

When the application registers the component, this function
is called.
***********************************************************/
//http://forums.mozillazine.org/viewtopic.php?f=19&t=1957409
try
{
  Components.utils.import("resource://gre/modules/XPCOMUtils.jsm");
}
catch(e) { }

/**
* XPCOMUtils.generateNSGetFactory was introduced in Mozilla 2 (Firefox 4).
* XPCOMUtils.generateNSGetModule is for Mozilla 1.9.1 (Firefox 3.5).
*/

if ("undefined" == typeof XPCOMUtils) // Firefox <= 2.0
{
  function NSGetModule(aComMgr, aFileSpec)
  {
    return PreferencesManager;
  }
}
else if (XPCOMUtils.generateNSGetFactory)
  var NSGetFactory = XPCOMUtils.generateNSGetFactory([PreferencesManager]);
else
  var NSGetModule = XPCOMUtils.generateNSGetModule([PreferencesManager]);