--------------------------------------------------------------------------------
--/  MODULE: luabot.interface
--/
--/  An interface to the irc connection that can be supplied to plugins. Allows
--/  them to register callbacks for certain events (text matching certain
--/  patterns, joins/parts, etc), and to send output back to the connection
--/  (messages, nick changes, and the like).
--/
--------------------------------------------------------------------------------


local global		= _G
local next			= global.next
local pairs			= global.pairs
local require		= global.require
local string		= global.string
local tostring		= global.tostring
local unpack		= global.unpack

local Debug			= require( "common.debug" ).Client( )
local ModuleUtils	= require( "common.moduleutils" )
local Object		= require( "common.object" )

module( "luabot.interface", Object.Class, ModuleUtils.DisallowIndexNewIndex )
local Interface = _M


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Declare private methods:
--
local InitCallbacks
local InitHooks
local RegisterCallback_Internal
local UpdateCallbacks


-- A table of all valid event types:
--
local g_tEventData = {
	Message		= { "TODO", },
	Action		= { "TODO", },
	Nick		= { "TODO", },
} -- local g_tEventData


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--/ @brief Instantiates the interface.
--/ @param _bot The luabot.bot instance that owns this interface.
--/ @param _connection The irc connection the interface wraps.
--/ @returns true if the interface instantiates successfully, false otherwise.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:Init( _bot, _connection )
	Debug.Trace( Debug.Level.Info, "Initialising plugin/connection interface." )
	
	self.bot				= _bot				-- The instance owning this interface.
	self.connection 		= _connection		-- The irc connection the interface wraps.
	
	-- A table of all callbacks. Contains subtables for each callback event (e.g.
	-- "Message", "Action", "Nick") with key the event and value another table
	-- containing the callbacks themselves.
	self.tCallbacks		= {}
	-- A table of new callbacks to be added to self.tCallbacks when safe. Has the
	-- same structure as self.tCallbacks.
	self.tNewCallbacks	= {}
	-- A table of which plugins have locked which events, if any.
	self.tCallbackLocks	= {}
	
	InitCallbacks( self )
	InitHooks( self )
	
	Debug.Trace( Debug.Level.Info, "Interface initialised." )
	return true
end -- Interface:Init


--------------------------------------------------------------------------------
--/ @brief Advances the interface, updating any newly added/removed callbacks.
--/ @param _nDeltaTime The time since the last advance.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:Advance( _nDeltaTime )
	UpdateCallbacks( self )
end -- Interface:Advance


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--/ @brief Sends a message to a given channel (or user).
--/ @param _sChannel The channel/user to send the message to.
--/ @param ... The message (in string.format format) to send.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:Say( _sChannel, ... )
	self.connection:sendChat( _sChannel, string.format( ... ) )
end -- Output:Say


--------------------------------------------------------------------------------
--/ @brief Sends a private notice to a given channel (or user).
--/ @param _sChannel The channel/user to send the notice to.
--/ @param ... The message (in string.format format) to send.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:Notice( _sChannel, ... )
	self.connection:sendNotice( _sChannel, string.format( ... ) )
end -- Output:Notice


--------------------------------------------------------------------------------
--/ @brief Sends a raw IRC command to the server.
--/ @details Intended primarily for development purposes. Any commands that need
--/          to be carried out with sufficient frequency should be given their
--/          own interface methods.
--/ @param ... The command (in string.format format) to send.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:Raw( ... )
	self.connection:send( ... )
end -- Interface:Raw


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--/ @brief Locks the interface, preventing events from being passed to any
--/        plugins other than the one that locks it.
--/ @details Can be used for plugins that allow the user to issue commands over
--/          multiple lines. Once the interface is locked, other plugins will
--/          not start getting in the way until it is unlocked again.
--/ @param _sEvent The event to be locked (e.g. "Message", "Action", etc).
--/ @param _plugin The plugin trying to lock the callbacks.
--/ @returns True if successful, false otherwise (e.g. if another plugin has
--/          already locked the interface).
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:LockCallbacks( _sEvent, _plugin )
	local tCallbackLocks = self.tCallbackLocks
	if ( tCallbackLocks[ _sEvent ] ) then
		Debug.Trace( Debug.Level.Warning, "Plugin %s trying to lock callbacks for '%s'; already locked by %s.", _plugin._NAME, _sEvent, tCallbackLocks[ _sEvent ]._NAME )
		return false
	else
		tCallbackLocks[ _sEvent ] = _plugin
		return true
	end
end -- Interface:LockCallbacks


--------------------------------------------------------------------------------
--/ @brief Unlocks the interface, allowing all plugins access to events again.
--/ @param _sEvent The event to be unlocked.
--/ @param _plugin The plugin that has previously locked these callbacks.
--/ @returns True if successful, false otherwise (e.g. if the interface is not
--/          locked, or if a different plugin has locked it).
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:UnlockCallbacks( _sEvent, _plugin )
	local tCallbackLocks = self.tCallbackLocks
	if ( tCallbackLocks[ _sEvent ] == _plugin ) then
		tCallbackLocks[ _sEvent ] = nil
		return true
	else
		Debug.Trace( Debug.Level.Warning, "Plugin %s trying to unlock callbacks for '%s'; lock currently owned by %s.", _plugin._NAME, _sEvent, tCallbackLocks[ _sEvent ]._NAME )
		return false
	end
end -- Interface:UnlockCallbacks


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--/ @brief Registers a callback to receive messages sent to a channel (or
--/        private messages sent to the bot directly).
--/ @details Callbacks may be registered with a text pattern or without. If a
--/          pattern is supplied, then the callback function will be called only
--/          for messages that match that pattern, passing in  the user's nick,
--/          the channel, and any captures from the pattern (or the whole
--/          message if no captures are specified). If no pattern is supplied,
--/          then the callback function is called for all messages, passing in
--/          the nick, the channel, and the whole message.
--/ @param _plugin The plugin instance to receive the callback.
--/ @param _fnFunction The member function to call on the plugin.
--/ @param _sPattern A pattern string to match against the incoming message. May
--/        be nil.
--/ @returns An opaque token that may be later passed to UnregisterMsgCallback
--/          to retract the callback again.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:RegisterMsgCallback( _plugin, _fnFunction, _sPattern )
	Debug.Trace( Debug.Level.Info, "Registering message callback for plugin %s with pattern '%s'.", tostring( _plugin ), _sPattern or "nil" )
	Debug.Assert( Debug.Level.Error, ( _plugin ~= nil ), "No plugin supplied!" )
	Debug.Assert( Debug.Level.Error, ( _fnFunction ~= nil ), "No callback function supplied!" )
	
	local tCallback = {
		plugin		= _plugin,		-- Must be non-nil.
		fnFunction	= _fnFunction,	-- Must be non-nil.
		sPattern	= _sPattern,	-- May be nil.
	}
	RegisterCallback_Internal( self, "Message", tCallback )
	
	-- Return an opaque key that can be later used to identify the callback, if
	-- we need to remove it.
	return tCallback
end -- Interface:RegisterMsgCallback


-- Temporary functions to present a more unified 'RegisterCallback' interface to
-- plugins. Internal implementation of this can be streamlined later when I know
-- what I'm doing with it.
function Interface:RegisterCallback( _sEvent, _plugin, _fnFunction, ... )
	if ( _sEvent == "Message" ) then
		return self:RegisterMsgCallback( _plugin, _fnFunction, ... )
	else
		Debug.Assert( Debug.Level.Error, false, "Bad callback!" )
	end
end -- Interface:RegisterCallback


--------------------------------------------------------------------------------
--/ @brief Unregisters a previously registered callback.
--/ @param _sEvent Name of the event for which to cancel the callback.
--/ @param _tCallback The token previously returned by RegisterCallback.
--/ @author jchilcott
--/ @date 28/09/2010
--------------------------------------------------------------------------------
function Interface:UnregisterCallback( _sEvent, _tCallback )
	Debug.Assert( Debug.Level.Error, ( self.tCallbacks[ _sEvent ] ~= nil ), "Cannot unregister callback for unknown event '%s'.", _sEvent )
	Debug.Assert( Debug.Level.Warning, ( self.tNewCallbacks[ _sEvent ][ _tCallback ] or self.tCallbacks[ _sEvent ][ _tCallback ] ) ~= nil, "Cannot unregister callback for event %s; not currently registered.", _sEvent )
	
	self.tNewCallbacks[ _sEvent ][ _tCallback ] = nil
	self.tCallbacks[ _sEvent ][ _tCallback ] = nil
end -- Interface:UnregisterCallback


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
function InitCallbacks( self )
	local tCallbacks	= self.tCallbacks
	local tNewCallbacks	= self.tNewCallbacks
	for sEvent, tEventData in pairs( g_tEventData ) do
		tCallbacks[ sEvent ]	= {}
		tNewCallbacks[ sEvent ]	= {}
	end
end -- function InitCallbacks


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
function InitHooks( self )
	-- Prepare 'Message' event callbacks.
	self.connection:hook( "OnChat", function( _tUser, _sChannel, _sMessage )
		Debug.Trace( Debug.Level.Always, "[%s] %s: %s", _sChannel, _tUser.nick, _sMessage )
		
		local lock = self.tCallbackLocks[ "Message" ]
		for _, tCallback in pairs( self.tCallbacks[ "Message" ] ) do
			if ( ( lock == nil ) or ( lock == tCallback.plugin ) ) then
				local sPattern = tCallback.sPattern
				if ( sPattern ) then
					-- Try to match the input string against the callback pattern,
					-- and pass its captures (or the whole message) to the plugin
					-- if it matches.
					local tMatches = { _sMessage:match( sPattern ) }
					if ( tMatches[1] ) then
						tCallback.fnFunction( tCallback.plugin, _tUser.nick, _sChannel, unpack( tMatches ) )
					end
				elseif ( sPattern == nil ) then
					-- This callback specified no pattern and just wants to be
					-- informed of all messages, regardless of content.
					tCallback.fnFunction( tCallback.plugin, _tUser.nick, _sChannel, _sMessage ) 
				end
			end -- If the callback is unlocked (or locked by this plugin)...
		end -- For every callback registered...
	end )
end -- function InitHooks


--------------------------------------------------------------------------------
--/ @brief Registers a callback token for some arbitrary event.
--------------------------------------------------------------------------------
function RegisterCallback_Internal( self, _sEvent, _tCallback )
	Debug.Assert( Debug.Level.Error, ( self.tNewCallbacks[ _sEvent ][ _tCallback ] or self.tCallbacks[ _sEvent ][ _tCallback ] ) == nil, "Cannot register callback for event %s; already registered.", _sEvent )
	self.tNewCallbacks[ _sEvent ][ _tCallback ] = _tCallback
end -- function RegisterCallback_Internal


--------------------------------------------------------------------------------
--/ @brief Updates the callback lists, preparing any newly added ones to receive
--/        events.
--------------------------------------------------------------------------------
function UpdateCallbacks( self )
	local tCallbacks	= self.tCallbacks
	local tNewCallbacks	= self.tNewCallbacks
	for sEvent, tNewEventCallbacks in pairs( tNewCallbacks ) do
		for key, tCallback in pairs( tNewEventCallbacks ) do
			tCallbacks[ sEvent ][ key ] = tCallback
			tNewEventCallbacks[ key ] = nil
		end
	end
end -- function UpdateCallbacks

