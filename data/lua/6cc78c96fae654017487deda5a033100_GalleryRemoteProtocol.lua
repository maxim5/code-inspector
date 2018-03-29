--[[----------------------------------------------------------------------------
	Implementation of the Gallery Remote Protocol.

	Copyright (C) 2007-2009 Arnaud Mouronval <arnaud.mouronval@yahoo.com>
	Copyright (C) 2007 Moritz Post <mail@moritzpost.de>
	Released under the GNU GPL.
-----------------------------------------------------------------------------]]--

require 'TableUtils'
require 'StringUtils'

  -- Lightroom SDK
local LrView = import 'LrView'
local LrFunctionContext = import 'LrFunctionContext'
local LrDialogs = import 'LrDialogs'
local LrLogger = import 'LrLogger'
local LrHttp = import 'LrHttp'
local LrTasks = import 'LrTasks'
local LrStringUtils = import 'LrStringUtils'
local LrPathUtils = import 'LrPathUtils'

local prefs = import 'LrPrefs'.prefsForPlugin()

local bind = LrView.bind
local share = LrView.share

-- Setup logger
local log = LrLogger( 'LR2Gallery' )

GalleryRemoteProtocol = {}

----------------------------------------------------------------------------------

-- Parses the gallery responds by creating a table 
-- and mapping each gallery data set (key=value) to a table
-- by inserting a { key=galleryKey, value=galleryValue }
local function parseGalleryResponse( raw )
	log:trace("Calling GalleryRemoteProtocol.parseGalleryResponse( Raw Message )")
	log:debug("Raw Message:\n"..raw)
	
	local flag = "#__GR2PROTO__"
	local result = {}
	local parsing = false
	local flagJustFound = false
	
	local lines = string.split( raw, "[^\n]+" )
	
	-- loop over each line
	for i,v in ipairs(lines) do
		if string.sub( v, -13, -1 ) == flag then
			parsing = true
			flagJustFound = true
		end 
		
		if parsing and not flagJustFound then
			-- create entry for result table
			local splitted = string.split( lines[i], "[^=]+" )
			table.insert( result,
				{ key = splitted[1], value = splitted[2] or "" }
			)
		end
		flagJustFound = false
	end
	return result
end

-- Get a value from the parsed response table
local function getResponse( responseTable, key )
	log:trace("Calling GalleryRemoteProtocol.getResponse( Key: "..key.." )")
	for i,v in ipairs(responseTable) do
		if responseTable[i].key == key then
			return responseTable[i].value
		end 
	end
	return nil
end

-- Returns the proper URL for Gallery
function GalleryRemoteProtocol.getGalleryURL( server )
	if server ~= nil then
		log:trace("Calling GalleryRemoteProtocol.getGalleryURL( "..server.." )")
	else
		log:trace("Calling GalleryRemoteProtocol.getGalleryURL( nil )")
	end

	-- the url to construct
	local url = ""

	server = LrStringUtils.trimWhitespace( server )
	
	-- add the http:// if required
	if string.sub( server, 1, 7 ) ~= 'http://' then
		url = 'http://'
	end
	-- add the trailing / if required
	if string.sub( server, -1 ) ~= '/' then
		url = url..server..'/'
	else
		url = url..server
	end
	
	return url
end

-- Returns the proper URL for the Gallery Remote controller
function GalleryRemoteProtocol.getGalleryRemoteURL( server )
	if server ~= nil then
		log:trace("Calling GalleryRemoteProtocol.getGalleryRemoteURL( "..server.." )")
	else
		log:trace("Calling GalleryRemoteProtocol.getGalleryRemoteURL( nil )")
	end
	
	-- the url to construct
	local url = ""
	
	if GalleryRemoteProtocol.galleryVersion == '1' then
		-- Complete the URL
		url = url..GalleryRemoteProtocol.getGalleryURL( server ).."gallery_remote2.php"
	else
		-- Complete the URL
		url = url..GalleryRemoteProtocol.getGalleryURL( server ).."main.php?g2_controller=remote:GalleryRemote"
	end
	
	return url
end

-- Check the remote server by sending a no-op cmd to the remote Gallery 
function GalleryRemoteProtocol.isRemoteProtocolEnabled(serverId)
	log:trace("Calling GalleryRemoteProtocol.isRemoteProtocolEnabled( "..prefs.serverTable[serverId].label.." )")
	
	-- get server
	local server = prefs.serverTable[serverId].server

	-- check the current version of Gallery
	if prefs.serverTable[serverId].version ~= nil then
		GalleryRemoteProtocol.galleryVersion = prefs.serverTable[serverId].version
	else
		GalleryRemoteProtocol.galleryVersion = '2'
	end
	
	local serverStatus = '0'
	if GalleryRemoteProtocol.galleryVersion ~= '1' then
		-- construct operation
		local operation =
			{ { name='g2_form[cmd]', value='no-op' }
			}
		
		-- send POST request to the gallery server
		local response, debug = LrHttp.postMultipart( GalleryRemoteProtocol.getGalleryRemoteURL(server), operation )
		
		-- parse reponse into easy to digest table format
		responseTable = parseGalleryResponse( response )
		
		-- get server response status
		serverStatus = getResponse( responseTable, 'status' )
		
		-- show error when the connection could not be established
		if serverStatus ~= '0' then
			LrDialogs.message(LOC "$$$/GalleryUpload/Dialog/NoConnectionPossible/Message=Could not establish a connection to the remote Gallery.",
							  LOC "$$$/GalleryUpload/Dialog/NoConnectionPossible/Info=Also make sure the remote module is enabled in the Gallery installation.",
							  "critical")
		end
	end

	return serverStatus
end

-- Login to the remote Gallery 
function GalleryRemoteProtocol.login(serverId)
	log:trace("Calling GalleryRemoteProtocol.login( "..prefs.serverTable[serverId].label.." )")
	
	-- reset the authentication token
	GalleryRemoteProtocol.authToken = ""
	
	-- get server
	local server = prefs.serverTable[serverId].server

	local response, debug
	if GalleryRemoteProtocol.galleryVersion == '1' then
		-- send GET request to the gallery server
		response, debug = LrHttp.get( GalleryRemoteProtocol.getGalleryRemoteURL(server).."?cmd=login&protocol_version=2.0&uname="..prefs.serverTable[serverId].username.."&password="..prefs.serverTable[serverId].password )
	else	
		-- construct operation
		local operation =
			{ { name='g2_form[cmd]', value='login' },
			  { name='g2_form[protocol_version]', value='2.0' },
			  { name='g2_form[uname]', value=prefs.serverTable[serverId].username },
			  { name='g2_form[password]', value=prefs.serverTable[serverId].password }
			}
			
		-- send POST request to the gallery server
		response, debug = LrHttp.postMultipart( GalleryRemoteProtocol.getGalleryRemoteURL(server), operation )
	end
	-- parse reponse into easy to digest table format
	responseTable = parseGalleryResponse( response )
	
	-- get server response status
	local serverStatus = getResponse( responseTable, 'status' )
	log:debug("Request Status: "..serverStatus)
	
	-- show error when the connection could not be established
	if serverStatus ~= '0' then
		if serverStatus == '201' then
		LrDialogs.message(LOC ("$$$/GalleryUpload/Dialog/201Error/Message=Could not log in to the remote server '^1'", server),
						  LOC "$$$/GalleryUpload/Dialog/201Error/Info=The login and/or password are incorrect.",
						  "critical")
		elseif serverStatus == '202' then
		LrDialogs.message(LOC ("$$$/GalleryUpload/Dialog/202Error/Message=Could not log in to the remote server '^1'", server),
						  LOC "$$$/GalleryUpload/Dialog/202Error/Info=The login and/or password are empty.",
						  "critical")
		else
		gallery:error ("Authentication: Unmanaged error code "..serverStatus.." from the remote server")
		LrDialogs.message(LOC ("$$$/GalleryUpload/Dialog/LoginDetailsFaulty/Message=Could not log in to the remote server '^1'", server),
						  LOC "$$$/GalleryUpload/Dialog/LoginDetailsFaulty/Info=Make sure the login details and the server url are valid.",
						  "critical")
		end
	else
		if GalleryRemoteProtocol.galleryVersion ~= '1' then
			-- store current auth_token
			GalleryRemoteProtocol.authToken = getResponse( responseTable, 'auth_token' )
			log:debug("Authentication Token: "..GalleryRemoteProtocol.authToken)
		end
	end
	
	return serverStatus
end


-- Get the list of albums from the server 
function GalleryRemoteProtocol.getAlbumList(serverId)
	log:trace("Calling GalleryRemoteProtocol.getAlbumList( "..prefs.serverTable[serverId].label.." )")
	
	-- get server
	local server = prefs.serverTable[serverId].server
	
	local response, debug
	if GalleryRemoteProtocol.galleryVersion == '1' then
		response, debug = LrHttp.get( GalleryRemoteProtocol.getGalleryRemoteURL(server).."?cmd=fetch-albums-prune&protocol_version=2.2&no_perms=yes" )
	else
		-- construct operation
		local operation =
			{ { name='g2_form[cmd]', value='fetch-albums-prune' },
			  { name='g2_form[protocol_version]', value='2.2' },
			  { name='g2_form[no_perms]', value='yes' },
			  { name='g2_authToken', value=GalleryRemoteProtocol.authToken },
			}	
	 
		-- send POST request to the gallery server
		response, debug = LrHttp.postMultipart( GalleryRemoteProtocol.getGalleryRemoteURL(server), operation )
	end
	-- parse reponse into easy to digest table format
	responseTable = parseGalleryResponse( response )
	
	-- get server response status
	local serverStatus = getResponse( responseTable, 'status' )
	log:debug("Gallery Command Status: "..serverStatus)
	
	-- build a list (table) of nodes from the data
	local albumList = {}
	
	-- show error when the connection could not be established
	if serverStatus ~= '0' then
		LrDialogs.message(LOC ( "$$$/GalleryUpload/Dialog/CouldNotFetchAlbums/Message=Could not fetch the list of albums from the server '^1'", server ),
						  LOC "$$$/GalleryUpload/Dialog/CouldNotFetchAlbums/Info=Make sure you have sufficient rights on the server.",
						  "critical")
	else
		local albumCount = getResponse( responseTable, 'album_count' )
		log:debug("Album Count: "..albumCount)
		
		-- create nodes
		for i = 1, albumCount do
			log:debug("inserting album \""..getResponse(responseTable, "album.name."..i).."\" to albumList")
			table.insert(albumList, { name = getResponse(responseTable, "album.name."..i),
								  title = getResponse(responseTable, "album.title."..i),
									  summary = getResponse(responseTable, "album.summary."..i),
									  parent = getResponse(responseTable, "album.parent."..i),
									  children = {}
									}
						 )
		end
		
		-- set children nodes
		for k,album in pairs(albumList) do
			log:debug("Looking for the parent album of album #"..k)
			-- find parent node
			for l,pAlbum in pairs(albumList) do
				log:debug("Checking if album \""..pAlbum.name.. "\" is a match")
				if pAlbum.name == album.parent then
					-- add the current node id to the parent node as a child
					table.insert(albumList[l].children, k)
					log:debug("inserting album #"..k.." as child of album \""..albumList[l].name.."\"")
				end	
			end
		end
	end
	
	if GalleryRemoteProtocol.galleryVersion ~= '1' then
		-- store current auth_token
		GalleryRemoteProtocol.authToken = getResponse(responseTable, 'auth_token')
	end
	
	return serverStatus, albumList
end

-- Add a new album to the server 
function GalleryRemoteProtocol.addAlbum(serverId, parentAlbum, albumName, albumTitle, albumDescription)
	log:trace("Calling GalleryRemoteProtocol.addAlbum( "..prefs.serverTable[serverId].label..", "..parentAlbum..", "..albumName..", "..albumTitle..", "..albumDescription.." )")
	
	-- get server
	local server = prefs.serverTable[serverId].server
	
	local response, debug
	if GalleryRemoteProtocol.galleryVersion == '1' then
		response, debug = LrHttp.get( GalleryRemoteProtocol.getGalleryRemoteURL(server).."?cmd=new-album&protocol_version=2.1&set_albumName="..parentAlbum.."&newAlbumName="..albumName.."&newAlbumTitle="..albumTitle.."&newAlbumDesc="..albumDescription )
	else
		-- construct operation
		local operation =
			{ { name='g2_form[cmd]', value='new-album' },
			  { name='g2_form[protocol_version]', value='2.1' },
			  { name='g2_form[set_albumName]', value=parentAlbum },
			  { name='g2_form[newAlbumName]', value=albumName },
			  { name='g2_form[newAlbumTitle]', value=albumTitle },
			  { name='g2_form[newAlbumDesc]', value=albumDescription },
			  { name='g2_authToken', value=GalleryRemoteProtocol.authToken },
			}	
		
		-- send POST request to the gallery server
		response, debug = LrHttp.postMultipart( GalleryRemoteProtocol.getGalleryRemoteURL(server), operation )
		end
	
	-- parse reponse into easy to digest table format
	responseTable = parseGalleryResponse( response )
	
	-- get server response status
	local serverStatus = getResponse( responseTable, 'status' )
	
	return serverStatus, getResponse( responseTable, 'album_name' )
end

-- Upload file to Gallery 
function GalleryRemoteProtocol.uploadImage(serverId, album, imagePath, caption)
	log:trace("Calling GalleryRemoteProtocol.uploadImage( "..prefs.serverTable[serverId].label..", "..album..", "..imagePath..", "..caption.." )")
	
	-- get server
	local server = prefs.serverTable[serverId].server
	
	local filename = LrPathUtils.leafName( imagePath )
	
	local response, debug, content
	if GalleryRemoteProtocol.galleryVersion == '1' then
		-- construct operation
		content =
			{ { name='cmd', value='add-item' },
			  { name='protocol_version', value='2.0' },
			  { name='set_albumName', value=album },
			  { name='caption', value=caption },
			  { name='userfile_name', value=filename },
			  { name = 'userfile',
				fileName = filename,
				filePath = imagePath,
				contentType = 'multipart/form-data'
			  }
			}
	else
		-- construct operation
		content =
			{ { name='g2_form[cmd]', value='add-item' },
			  { name='g2_form[protocol_version]', value='2.0' },
			  { name='g2_form[set_albumName]', value=album },
			  { name='g2_form[caption]', value=caption },
			  { name='g2_form[force_filename]', value=filename },
			  { name='g2_authToken', value=GalleryRemoteProtocol.authToken },
			  { name = 'g2_userfile',
				fileName = filename,
				filePath = imagePath,
				contentType = 'multipart/form-data'
			  }
			}
	end
	-- send POST request to the gallery server
	response, debug = LrHttp.postMultipart( GalleryRemoteProtocol.getGalleryRemoteURL(server), content )
	
	-- parse reponse into easy to digest table format
	responseTable = parseGalleryResponse( response )
		
	-- get server response status
	local serverStatus = getResponse( responseTable, 'status' )
	
	return serverStatus
end