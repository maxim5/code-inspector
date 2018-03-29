--[[----------------------------------------------------------------------------
	Implementation of the sections shown in the export dialog.

	Copyright (C) 2007-2009 Arnaud Mouronval <arnaud.mouronval@yahoo.com>
	Copyright (C) 2007 Moritz Post <mail@moritzpost.de>
	Released under the GNU GPL.
-----------------------------------------------------------------------------]]--

require 'GalleryUploadDialogs'
require 'GalleryRemoteProtocol'
require 'TableUtils'

-- Lightroom SDK
local LrBinding = import 'LrBinding'
local LrView = import 'LrView'
local LrDialogs = import 'LrDialogs'
local LrTasks = import 'LrTasks'
local LrFunctionContext = import 'LrFunctionContext'
local LrLogger = import 'LrLogger'

local prefs = import 'LrPrefs'.prefsForPlugin()

-- setup logger
local log = LrLogger( 'LR2Gallery' )

--============================================================================--

GalleryUploadExportDialogSections = {}

-------------------------------------------------------------------------------

-- a deep copy routine for copying tables
local deepcopy = function (object)
    local lookup_table = {}
    local function _copy(object)
        if type(object) ~= "table" then
            return object
        elseif lookup_table[object] then
            return lookup_table[object]
        end
        local new_table = {}
        lookup_table[object] = new_table
        for index, value in pairs(object) do
            new_table[_copy(index)] = _copy(value)
        end
        return setmetatable(new_table, getmetatable(object))
    end
    return _copy(object)
end

local buildServerItems = function( properties ) 
	log:trace ("Calling buildServerItems( properties )")
	local serverItems = {}
	
	if prefs == nil then
		log:info("No preferences")
	else
		if prefs.serverTable ~= nil and #prefs.serverTable > 0 then
			for i,v in ipairs( prefs.serverTable ) do 
				table.insert( serverItems,
					{ title = v.label, value = i }
				)
			end
			properties.serverItems = serverItems
			-- select the last used server
			if prefs.serverValue ~= nil and #serverItems >= prefs.serverValue then
				properties.serverValue = prefs.serverValue
			end
		else
			properties.serverItems = serverItems
			-- select no server
			properties.serverValue = 0
		end
	end
end

local buildAlbumItems = function( properties, albumList )
	log:trace("Calling buildAlbumItems( properties, albumList )")
	-- reset the album items
	properties.albumItems = {}
	
	if albumList ~= nil and #albumList > 0 then
		if prefs.serverTable[properties.serverValue].version ~= '1' then
			-- find root album id
			local rootAlbumName = properties.rootAlbumValue -- name (actually a number) of the parent gallery album
			local rootAlbumId = -1 -- index in the array
			
			for i, album in pairs( albumList ) do
				if album.name == rootAlbumName then
					rootAlbumId = i
					break
				end
			end
			
			if rootAlbumId ~= -1 then
				-- attach children to the topmost album
				attachChildren( properties.albumItems, albumList, rootAlbumId, "", 100 )
			end
		else
			for i,album in pairs( albumList ) do
				-- Find root album ids
				if album.parent == "0" then
					-- attach children to the topmost album
					attachChildren( properties.albumItems, albumList, i, "", 100 )
				end
			end
		end
	end
	
	if #properties.albumItems > 0 then
		properties.albumsEnabled = true
		-- ensure notification mechanism
		properties.albumItems = properties.albumItems
		properties.galleryAccountStatus = LOC ( "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/LoggedIn=Connected" )
	end
	
end


local buildRootAlbumItems = function( properties, albumList )
	log:trace("Calling buildRootAlbumItems( properties, albumList )")
	-- reset the album items
	properties.rootAlbumItems = {}
	properties.albumList_ = deepcopy(albumList)
	
	if albumList ~= nil and #albumList > 0 then
		if prefs.serverTable[properties.serverValue].version ~= '1' then
			-- find root album id
			local rootAlbumName = 999999999999 -- name (actually a number) of the parent gallery album
			local rootAlbumId = -1 -- index in the array
			
			for i,album in pairs( albumList ) do
				local currentParentName = tonumber( album.parent )
				if currentParentName < rootAlbumName then
					rootAlbumName = currentParentName
					rootAlbumId = i
				end
			end
			
			if rootAlbumId ~= -1 then
				-- attach children to the topmost album
				attachChildren( properties.rootAlbumItems, albumList, rootAlbumId, "", 2 )
			end
		else
			for i,album in pairs( albumList ) do
				-- Find root album ids
				if album.parent == "0" then
					-- attach children to the topmost album
					attachChildren( properties.rootAlbumItems, albumList, i, "", 2 )
				end
			end
		end
	end

	if #properties.rootAlbumItems == 0 then
		properties.albumsEnabled = false
		properties.rootAlbumItems = nil
		properties.albumItems = nil
		properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/NotLoggedIn=Not logged in"
	else
		properties.albumsEnabled = true
		-- ensure notification mechanism
		properties.rootAlbumItems = properties.rootAlbumItems
		properties.galleryAccountStatus = LOC ( "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/LoggedIn=Connected" )
		buildAlbumItems(properties, albumList)
	end
end

-- Attach children to the items list
attachChildren = function( albumItems, albumList, id, indentation, degree )
	if degree < 0 then
		return
	end
	
	log:trace("Calling attachChildren( albumItems, albumList, "..id..", "..indentation.." )")
	local albumName = albumList[id].title
	
	-- clean special characters
	albumName = string.gsub( albumName, '&amp;', "&" )
	albumName = string.gsub( albumName, '\\n', " " )
	albumName = string.gsub( albumName, '\\:', ":" )
	albumName = string.gsub( albumName, '\\!', "!" )
	albumName = string.gsub( albumName, '\\#', "#" )
	albumName = string.gsub( albumName, "\\\\", "\\" )

	table.insert( albumItems, { title = indentation..albumName,
										   name = albumName,
										   value = albumList[id].name } 
	)

	-- update the indentation level
	indentation = indentation.."  |	 "
	
	-- attach subalbums
	for i,v in ipairs( albumList[id].children ) do
		log:debug("attaching child #"..v)
		attachChildren( albumItems, albumList, tonumber(v), indentation, degree - 1 )
	end
end

updateServerSelection = function( properties )
	log:trace ("Calling updateServerSelection( properties )")
	if properties.albumItems ~= nil and properties.albumItems ~= 0 then
		-- disable the album creation button
		properties.albumSelected = false
		-- reinitialize the album list
		properties.albumsEnabled = false
		properties.albumItems = nil
		properties.albumValue = nil
		properties.subAlbumItems = nil
		properties.subAlbumValue = nil
		-- Disable the export button
		properties.LR_canExport = false
	end
	
	-- change the connexion status
	properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/NotLoggedIn=Not logged in"
end

updateServerSelectionStatus = function( properties )
	log:trace ("Calling updateServerSelectionStatus( properties )")
	if properties.serverValue ~= 0 then
		properties.serverSelected = true
		if properties.serverValue > 0 then
			properties.serverSynopsis = properties.serverItems[properties.serverValue].title.." - "..properties.galleryAccountStatus
		end 
	else
		properties.serverSelected = false
		properties.albumsEnabled = false
		properties.albumItems = nil
		properties.subAlbumItems = nil
		properties.serverSynopsis = LOC "$$$/GalleryUpload/ExportDialog/ServerSynopsis/NoServer=No Gallery registered server"
		updateRootAlbumSelectionStatus( properties )
	end
end

updateRootAlbumSelectionStatus = function( properties )
	log:trace ("Calling updateRootAlbumSelectionStatus( properties )")
	
	if properties.rootAlbumValue == nil or properties.rootAlbumValue == 0 then
		-- disable the album creation
		properties.albumSelected = false
		-- disable export button
		properties.LR_canExport = false
		properties.albumSynopsis = nil
	else
		buildAlbumItems(properties, properties.albumList_)
	end
end

updateAlbumSelectionStatus = function( properties )
	log:trace ("Calling updateAlbumSelectionStatus( properties )")
	if properties.albumValue == nil or properties.albumValue == 0 then
		-- disable the album creation
		properties.albumSelected = false
		-- disable export button
		properties.LR_canExport = false
		properties.albumSynopsis = nil
	else
		-- enable the album creation
		properties.albumSelected = true
		-- enable export button
		properties.LR_canExport = true
		-- set album synopsis
		local albumName = nil
		for i in pairs( properties.albumItems ) do
			if properties.albumItems[i].value == properties.albumValue then
				albumName = properties.albumItems[i].name
				break
			end
		end
		properties.albumSynopsis = albumName
	end
end

updateCaptionStatus = function( properties )
	log:trace ("Calling updateCaptionStatus( properties )")
	properties.metadataSynopsis = LOC "$$$/GalleryUpload/ExportDialog/Metadata=Metadata: "
	if properties.caption == 'none' or properties.caption == nil then
		properties.metadataSynopsis = properties.metadataSynopsis..LOC "$$$/GalleryUpload/ExportDialog/TitleSetToNone=None"
	elseif properties.caption == 'filename' then
		properties.metadataSynopsis = properties.metadataSynopsis..LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromFilename=Filename"
	elseif properties.caption == 'title' then
		properties.metadataSynopsis = properties.metadataSynopsis..LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromTitle=Photo Title"
	elseif properties.caption == 'caption' then
		properties.metadataSynopsis = properties.metadataSynopsis..LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromCaption=Photo Caption"
	elseif properties.caption == 'headline' then
		properties.metadataSynopsis = properties.metadataSynopsis..LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromHeadline=Photo Headline"
	end
end

-------------------------------------------------------------------------------

function GalleryUploadExportDialogSections.startDialog( properties )
	log:trace("Calling GalleryUploadExportDialogSections.startDialog( properties )")
	
	-- disable export button
	properties.LR_canExport = false
	
	-- clear the album list
	properties.albumsEnabled = false
	properties.rootAlbumItems = nil
	properties.albumItems = nil
	properties.albumList_ = nil

	-- add observers
	properties:addObserver( 'serverValue', updateServerSelectionStatus )
	properties:addObserver( 'serverValue', updateServerSelection )
	properties:addObserver( 'galleryAccountStatus', updateServerSelectionStatus )
	properties:addObserver( 'rootAlbumValue', updateRootAlbumSelectionStatus )
	properties:addObserver( 'albumValue', updateAlbumSelectionStatus )
	properties:addObserver( 'caption', updateCaptionStatus )

	-- initialize dialog elements
	buildServerItems( properties )
	buildRootAlbumItems( properties, {} )
	updateRootAlbumSelectionStatus( properties )
	updateCaptionStatus( properties )
	
	properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/NotLoggedIn=Not logged in"
end

function GalleryUploadExportDialogSections.endDialog( properties, why )
	log:trace("Calling GalleryUploadExportDialogSections.endDialog( properties, why )")
	
	-- save the last used server
	if properties.serverValue ~= 0 then
		prefs.serverValue = properties.serverValue
	end
end

-------------------------------------------------------------------------------

function GalleryUploadExportDialogSections.sectionsForTopOfDialog( f, properties )
	log:trace("Calling GalleryUploadExportDialogSections.sectionsForTopOfDialog( f, properties )")
	local f = LrView.osFactory()
	local bind = LrView.bind
	local share = LrView.share

	local result = {
		{
			title = LOC "$$$/GalleryUpload/ExportDialog/GallerySectionTitle=Gallery Accounts",
			
			synopsis = bind { key = 'serverSynopsis', object = properties },
			
			f:row {
				f:static_text {
					title = LOC "$$$/GalleryUpload/ExportDialog/Server=Server:",
					alignment = 'right',
					width = share 'labelWidth'
				},
				
				f:popup_menu {
					fill_horizontal = 1,
					enabled = bind 'serverSelected',
					items = bind 'serverItems',
					value = bind 'serverValue',
				},
				
				f:push_button {
					title = LOC "$$$/GalleryUpload/ExportDialog/LogIn=Log In",
					enabled = bind 'serverSelected',
					width = share "rightButtonWidth",
					action = function( button )
						LrTasks.startAsyncTask( function()
							properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/ContactingGallery=Contacting Gallery"
							local status = GalleryRemoteProtocol.isRemoteProtocolEnabled(properties.serverValue)
							
							if status == '0' then
								properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/LoggingIn=Logging in"
								status = GalleryRemoteProtocol.login(properties.serverValue)
							end
							
							albumList = {}
							if status == '0' then
								properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/FetchingAlbumList=Fetching album list"
								status, albumList = GalleryRemoteProtocol.getAlbumList(properties.serverValue)
							end
							
							if status == '0' then
								if #albumList > 0 then
									buildRootAlbumItems(properties, albumList)
									properties.albumsEnabled = true
									properties.rootAlbumValue = properties.rootAlbumItems[1].value
								else
									properties.galleryAccountStatus = LOC "$$$/GalleryUpload/ExportDialog/GalleryAccountStatus/NoAlbumFetched=No album fetched"
									properties.albumsEnabled = false
									properties.rootAlbumItems = nil
								end
							else
								properties.albumsEnabled = false
								properties.rootAlbumItemslbumItems = nil
							end
						end )
					end
				},
			},
			
			f:row { 
				f:spacer {
					width = share 'labelWidth',
				},
				
				f:push_button {
					title = LOC "$$$/GalleryUpload/ExportDialog/AddServer=Add Server",
					action = function( button )
						local result = GalleryUploadDialogs.showManageServers()
						buildServerItems( properties )
						
						-- select the just added server in the popup_menu
						if result ~= "cancelled" then
							properties.serverValue = #prefs.serverTable
						end
						
						updateServerSelectionStatus( properties )
						
						-- ensure notification mechanism
						properties.serverItems = properties.serverItems
						
						buildRootAlbumItems( properties, {} )
					end
				},
				
				f:push_button {
					title = LOC "$$$/GalleryUpload/ExportDialog/ModifyServer=Modify Server",
					enabled = bind 'serverSelected',
					action = function( button )
						GalleryUploadDialogs.showManageServers(properties.serverValue)
						buildServerItems( properties )
						updateServerSelectionStatus( properties )
						
						buildRootAlbumItems( properties, {} )
					end
				},
				
				f:push_button {
					title = LOC "$$$/GalleryUpload/ExportDialog/DeleteServer=Delete Server",
					enabled = bind 'serverSelected',
					action = function( button )
						log:info("Removing server no "..properties.serverValue.." from serverTable")
						table.remove(prefs.serverTable, properties.serverValue)
						
						-- ensure storage in prefs
						prefs.serverTable = prefs.serverTable
						
						buildServerItems( properties )
						
						-- set selected item in popup_menu
						if properties.serverValue > 1 then
							properties.serverValue = properties.serverValue - 1
						elseif #properties.serverTable == 1 then
							properties.serverValue = 1
						else
							properties.serverValue = 0
						end
						
						updateServerSelectionStatus( properties )
						
						-- ensure notification mechanism
						properties.serverItems = properties.serverItems
						
						buildRootAlbumItems( properties, {} )
					end
				}
			},
			f:row { 
				f:static_text {
					title = LOC "$$$/GalleryUpload/ExportDialog/Status=Status:",
					alignment = 'right',
					width = share 'labelWidth',
				},
				
				f:static_text {
					title = bind 'galleryAccountStatus',
					fill_horizontal = 1,
				},
			},
		},
		{
			title = LOC "$$$/GalleryUpload/ExportDialog/AlbumSectionTitle=Gallery Albums",
			
			synopsis = bind { key = 'albumSynopsis', object = properties },
			
			f:row {
				
				f:static_text {
					title = LOC "$$$/GalleryUpload/ExportDialog/Albums=Albums:",
					alignment = 'right',
					width = share 'labelWidth',
					enabled = bind 'albumsEnabled',
				},
				
				f:popup_menu {
					fill_horizontal = 1,
					enabled = bind 'albumsEnabled',
					items = bind 'rootAlbumItems',
					value = bind 'rootAlbumValue',
				},
				
				f:popup_menu {
					fill_horizontal = 1,
					enabled = bind 'albumsEnabled',
					items = bind 'albumItems',
					value = bind 'albumValue',
				},
				
				f:push_button {
					title = LOC "$$$/GalleryUpload/ExportDialog/AddAlbum=Add Album",
					enabled = bind 'albumSelected',
					width = share "rightButtonWidth",
					action = function( button )
						LrFunctionContext.postAsyncTaskWithContext ( 'AddAlbum', function( context )
						
							resultAlbumName, resultAlbumTitle, resultAlbumDesc = GalleryUploadDialogs.showAddAlbum(context)
							
							if resultAlbumName ~= 'cancelled' then
								local status, actualAlbumName = GalleryRemoteProtocol.addAlbum( properties.serverValue, properties.albumValue, resultAlbumName, resultAlbumTitle, resultAlbumDesc )
								
								local albumList = {}
								if status == '0' then
									status, albumList = GalleryRemoteProtocol.getAlbumList(properties.serverValue)
								end
								
								if status == '0' and #albumList > 0 then
									buildRootAlbumItems(properties, albumList)
									properties.albumsEnabled = true
									properties.albumValue = actualAlbumName
								else
									properties.albumsEnabled = false
									properties.albumItems = nil
								end 
							end
						end )
					end
				},
				
			},
			f:row {
				f:spacer {
					width = share 'labelWidth',
				},
				f:checkbox {
					title = LOC "$$$/GalleryUpload/ExportDialog/ShowInBrowser=Show Album in Browser After Upload",
					enabled = bind 'albumsEnabled',
					value = bind 'showInBrowser',
				},
			},
		},
		{
			title = LOC "$$$/GalleryUpload/ExportDialog/GalleryMetadataSectionTitle=Gallery Metadata",
			
			synopsis = bind { key = 'metadataSynopsis', object = properties },
			
			f:row {
				f:static_text {
					title = LOC "$$$/GalleryUpload/ExportDialog/ItemTitle=Item Title:",
					alignment = 'right',
					width = share 'labelWidth',
				},
				
				f:popup_menu {
					fill_horizontal = 1,
					items = {
						{ title = LOC "$$$/GalleryUpload/ExportDialog/TitleSetToNone=None", value = 'none' },
						{ title = LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromFilename=Filename", value = 'filename' },
						{ title = LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromTitle=Photo Title", value = 'title' },
						{ title = LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromCaption=Photo Caption", value = 'caption' },
						{ title = LOC "$$$/GalleryUpload/ExportDialog/TitleSetFromHeadline=Photo Headline", value = 'headline' },
					},
					value = bind 'caption',
				},
			},
		},
	}
	
	return result
end