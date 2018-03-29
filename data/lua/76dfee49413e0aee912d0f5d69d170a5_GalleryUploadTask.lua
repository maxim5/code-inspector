--[[----------------------------------------------------------------------------
	Implementation of the upload facilities to the remote Gallery.

	Copyright (C) 2007-2009 Arnaud Mouronval <arnaud.mouronval@yahoo.com>
	Copyright (C) 2007 Moritz Post <mail@moritzpost.de>
	Released under the GNU GPL.
-----------------------------------------------------------------------------]]--

-- Lightroom API
local LrPathUtils = import 'LrPathUtils'
local LrFtp = import 'LrFtp'
local LrFileUtils = import 'LrFileUtils'
local LrErrors = import 'LrErrors'
local LrDialogs = import 'LrDialogs'
local LrHttp = import 'LrHttp'
local LrLogger = import 'LrLogger'

local prefs = import 'LrPrefs'.prefsForPlugin()

-- setup logger
local log = LrLogger( 'LR2Gallery' )

--============================================================================--

GalleryUploadTask = {}

--------------------------------------------------------------------------------

function GalleryUploadTask.processRenderedPhotos( functionContext, exportContext )
	log:trace ("Calling GalleryUploadTask.processRenderedPhotos( functionContext, exportContext )")

	-- Make a local reference to the export parameters.
	local exportSession = exportContext.exportSession
	local exportParams = exportContext.propertyTable
	
	-- Set progress title.
	local nPhotos = exportSession:countRenditions()

	-- get server
	serverId = exportParams.serverValue
	
	-- get album
	albumName = exportParams.albumValue

	-- get metadata settings
	captionSetting = exportParams.caption
	
	local progressScope = exportContext:configureProgress{
						title = nPhotos > 1
							and LOC( "$$$/GalleryUpload/Upload/Progress=Uploading ^1 photos to the Gallery", nPhotos )
							or LOC "$$$/GalleryUpload/Upload/Progress/One=Uploading one photo to the Gallery",
					}
	
	-- Iterate through photo renditions.
	local failures = {}

	for i, rendition in exportContext:renditions{ stopIfCanceled = true } do
	
		-- Get next photo.
		local photo = rendition.photo
		local success, pathOrMessage = rendition:waitForRender()
		
		-- Check for cancellation again after photo has been rendered.
		if progressScope:isCanceled() then
			break
		 end
		
		if success then

			local filename = LrPathUtils.leafName( pathOrMessage )

			local caption = ''
	
			-- Set the caption
			if captionSetting == 'none' then
				caption = ''
			elseif captionSetting == 'filename' then
				caption = filename
			elseif captionSetting == 'title' then
				photo.catalog:withCatalogDo( function() 
					caption = photo:getFormattedMetadata ( 'title' )
				end ) 
			elseif captionSetting == 'caption' then
				photo.catalog:withCatalogDo( function() 
					caption = photo:getFormattedMetadata ( 'caption' )
				end ) 
			end	
			
			-- upload file to gallery
			success = GalleryRemoteProtocol.uploadImage( serverId, albumName, pathOrMessage, caption )
			
			if success ~= '0' then
				-- if we can't upload that file, log it.  For example, maybe user has exceeded disk
				-- quota, or the file already exists and we don't have permission to overwrite, or
				-- we don't have permission to write to that directory, etc....
				table.insert( failures, filename )
			end
					
			-- When done with photo, delete temp file. There is a cleanup step that happens later,
			-- but this will help manage space in the event of a large upload.
			LrFileUtils.delete( pathOrMessage )
		end
	end

	if #failures > 0 then
		local message
		if #failures == 1 then
			message = LOC "$$$/GalleryUpload/Upload/Errors/OneFileFailed=A file failed to upload correctly."
		else
			message = LOC ( "$$$/GalleryUpload/Upload/Errors/SomeFileFailed=^1 files failed to upload correctly.", #failures )
		end
		LrDialogs.message( message, table.concat( failures, "\n" ) )
	else
		-- show album in browser if desired
		if exportParams.showInBrowser == true then
			local server = prefs.serverTable[serverId].server
			if prefs.serverTable[serverId].version == '1' then
				LrHttp.openUrlInBrowser( GalleryRemoteProtocol.getGalleryURL( server ).."view_album.php?set_albumName="..albumName )
			else
				LrHttp.openUrlInBrowser( GalleryRemoteProtocol.getGalleryURL( server ).."?g2_itemId="..albumName )
			end
		end
	end
end
