
data.gametypes=data.gametypes or {}


--[[ debugging, clear out old data
if data.gametypes["zeegrind"] then
	remove_update( data.gametypes["zeegrind"] ) -- remove for debug niceness
	data.gametypes["zeegrind"]=nil
end
]]

local gtab=data.gametypes["zeegrind"] or {}
data.gametypes["zeegrind"]=gtab -- update, keep unique gtab table

gtab.name="zeegrind"

gtab.room_names=gtab.room_names or {} -- keeps track of active zeegrind room names

gtab.zds=gtab.zds or {} -- vroom state data, keeps track of game items etc

gtab.time=gtab.time or 0


dofile("spew_games_zeegrind_data.lua")
dofile("spew_games_zeegrind_npc.lua")


-----------------------------------------------------------------------------
-- 
-- which vroom should we put this user in
-- 
-----------------------------------------------------------------------------
gtab.vroom_which = function(room,user)

local vuser=data.ville.users[string.lower(user.name)]
	
local vroom=vroom_obtain(user.room)

--local vroom=data.ville and data.ville.rooms and data.ville.rooms[room.name]


	if not vroom then return vroom end -- stuff is not setup

local zd=gtab.manifest_zd(room,vroom)

local zone=tonumber( day_flag_get(user.name,"zeegrind.zone") or 0 )

if vuser.owner=="xix" or vuser.owner=="shi" then -- test
	zone=11
end

	if vuser.propdata.root > gtab.now then -- punish if you reaload whilst rooted
	
		zone=zone-1
	end
	
	if zone<0 then zone=0 end -- sanity

	if vroom.rooms[ tostring(zone) ] then -- goto room?
	
		vroom = vroom.rooms[ tostring(zone) ]
		if vroom then vroom=data.ville.vobjs[vroom] end -- convert id to room
	end
	
	if not vroom then -- last chance to get a vroom
		vroom=vroom_obtain(user.room)
	end

--dbg(user.name," : ",tostring(zone),"\n")

	return vroom
end

-----------------------------------------------------------------------------
-- 
-- someone has joined this vroom
-- 
-----------------------------------------------------------------------------
gtab.vroom_join = function(vroom,vuser)

local room=vroom_get_room(vroom)
local zd=gtab.manifest_zd(room,vroom)
local name=vuser.owner

local zroom=gtab.rooms[vroom.class]

local ret={}

	gtab.npc_create_pc(zd,name,vroom,vuser)

	local zone=gtab.npc_loadzone_name(vuser.npc)
	
	local doordest
	
	local p={x=0,y=0,z=0}
	
	if zroom and zroom.level>=1 and zroom.level<=11 then -- pick door
	
		if zone=="entrance" then
			doordest="./"..tostring(zroom.level-1)
		else
			doordest="./"..tostring(zroom.level+1)
		end
	end
	
	if doordest=="./0" then doordest="." end -- ./0 doesnt exist, replace it with .
	
	if doordest then
		for n,b in pairs(vroom.contents) do
		
			local vobj=data.ville.vobjs[ n ]
			
			if vobj and vobj.type=="door" then
			
				if vobj.props.dest==doordest then -- the door we want
			
					local po=propdata_get_xyzt(vobj,lanes.now_secs())
					p.x=po.x
					p.y=po.y
					p.z=po.z
					
					vroom_clip_xyzt(vroom,p,nil,edge)
					
					break
				end
			end
		end
	else
		p=vroom_door_xyzt(vroom,vuser)
	end
	
	
	propdata_set_xyzt(vuser,p,0)
	propdata_send_xyzt(vuser,"force")

	ret.walkto=vroom_random_xyzt(vroom,zone,{0,1,0.5,1})
	
--	propdata_set_xyzt(vuser,vroom_random_xyzt(vroom,gtab.npc_loadzone_name(vuser.npc),{0,1,0.5,1}),0)
--	propdata_send_xyzt(vuser,"force")

	
-- check helper
--[[
	local npc=zd.npcs[1]
	
	if npc and npc.zom and npc.zom.start_state=="help" then -- the helper bot, we are in room 0
	
		local vo=vobj_get_vobj_str(npc.id)
		
		if vo then
			vuser_cast(vuser, vobj_build_say_msg(vo,"Welcome to ZeeGrind, I love you. Please click me for more help.") )
		end
	end
]]
	
--	vobj_set(vuser,"xyz",vroom_random_xyz(vroom,"entrance"))
	return ret
	
end

-----------------------------------------------------------------------------
-- 
-- someone has left this vroom
-- 
-----------------------------------------------------------------------------
gtab.vroom_part = function(vroom,vuser)

local room=vroom_get_room(vroom)
local zd=gtab.manifest_zd(room,vroom)
local npc=zd.pcs[vuser.owner]

	gtab.npc_remove_pc(zd,npc)
	
end

-----------------------------------------------------------------------------
-- 
-- update this users balloon (take away their real one and replace with the game one)
-- 
-----------------------------------------------------------------------------
gtab.check_balloon=function(vroom,vuser)

local npc=vuser.npc

	if not vroom or not vuser then return end
	
--dbg("checking balloon ",vuser.id,"\n")

	if vuser.npc and not vuser.npc.user then -- check for an npc balloon
	
	elseif vuser.brain then -- check for a bot balloon
	
	else -- check for a user balloon

local weapon_name=day_flag_get(vuser.owner,"zeegrind.balloon") or "none"
local weapon

if vuser.owner=="xix" or vuser.owner=="shi" then -- test
--	weapon_name="banana:5:42"
end

local item

	if weapon_name=="none" then -- pull in a weapon from the really real balloon?
	
		item=get_item_by_home(string.lower(vuser.owner).."/balloon") -- are we holding a balloon?
			
		if item and item.props.type=="zeegrind" then -- we carried a weapon in
		
			local lev=force_floor((item.props.size-10)/5) -- level depends on size outside of zeegrind
			if lev<1 then lev=1 end

			weapon_name=item_name_balloon(item.props) .. ":"..(lev)..":100"
			
			feats_signal_zeegrind(vuser,"byob",{weapon_name=weapon_name}) -- we brought a useful balloon in to use
		else
		
			item=nil -- forget we had an item
		end
		
	end


	if weapon_name then
	
		weapon=gtab.get_weapon_info( weapon_name )
	
	end

local balloon="0:float:0:0:0"
local speed=100

	if weapon then
	
		weapon.item=item -- remember this is a real item
		
		if npc then npc.weapon=weapon end
		
		balloon=weapon.balloon
		speed=weapon.walkspeed
	
	else
	
		if npc then npc.weapon=nil end
	
	end


	vobj_set(vuser,"balloon", balloon ) -- place it in hand
	vobj_set(vuser,"speed", speed )

	
	end
	
	return true
end

-----------------------------------------------------------------------------
--
-- return the menu prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function gtab.door_menu(vobj)
	
	return "base/use/restart"
end

-----------------------------------------------------------------------------
--
-- return the balloon prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function gtab.door_use(vobj,dd,prop,val,user)

--dbg("door use "..val.."\n")

	local vuser=data.ville.users[string.lower(user.name)]
			
	local vals=str_split(":",val)
	
local vroom=vobj_get_vroom(vobj)

local zroom=gtab.rooms[vroom.class]
if not zroom then zroom=gtab.rooms[0] end


	if vals[1]=="base use" then
	
		if zroom.name=="Level 0" then -- dont let them out till we have a weapon
			if vobj.props.dest=="./1" then
			
				if vuser and vuser.npc and vuser.npc.weapon then
				
					return nil,false
				
				else
				
				vuser_cast(vuser, vobj_build_say_msg(vuser,"I'm not going out there without a balloon. Click and hold on a balloon hook then select TAKE and release the mouse button to take a balloon.") )
		
					return nil,true
				end

			end
		
		end
	
	elseif vals[1]=="base restart" then
	
		if dd > 80*80 then -- not close enough to walk through
		
		else
		
			day_flag_set(user.name,"zeegrind.zone",1) -- reset zone, so we go back to the start

		end
	
		return ".",false

	end

end

-----------------------------------------------------------------------------
--
-- return the menu prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function zeegrind_hook_menu(vobj)
	
	return "base/cancel/examine/compare/about/take"
end

-----------------------------------------------------------------------------
--
-- return the balloon prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function zeegrind_hook_balloon(vobj)

local vroom=vobj_get_vroom(vobj)

local zroom=gtab.rooms[vroom.class]
if not zroom then zroom=gtab.rooms[0] end

local weapon=gtab.get_weapon_info( zroom.weapons[ tonumber(vobj.props.id) ] )

	return weapon.balloon

end

-----------------------------------------------------------------------------
--
-- return the balloon prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function zeegrind_hook_examine(user,vobj)

local vroom=vobj_get_vroom(vobj)

local zroom=gtab.rooms[vroom.class]
if not zroom then zroom=gtab.rooms[0] end

local weapon=gtab.get_weapon_info( zroom.weapons[ tonumber(vobj.props.id) ] )

	userqueue(user,{cmd="note",note="act",arg1=weapon.desc})

end

-----------------------------------------------------------------------------
--
-- return the balloon prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function zeegrind_hook_about(user,vobj)

local vroom=vobj_get_vroom(vobj)

local zroom=gtab.rooms[vroom.class]
if not zroom then zroom=gtab.rooms[0] end

local weapon=gtab.get_weapon_info( zroom.weapons[ tonumber(vobj.props.id) ] )

	userqueue(user,{cmd="note",note="act",arg1=weapon.about})

end

-----------------------------------------------------------------------------
--
-- return the balloon prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function zeegrind_hook_compare(user,vobj)

local vuser

	if user then vuser=data.ville.users[string.lower(user.name)] end -- get vuser if a user is operating this

	if vuser and vuser.npc and vuser.npc.weapon then
	
		userqueue(user,{cmd="note",note="act",arg1="You are now holding : "..vuser.npc.weapon.desc})
		
	else
	
		userqueue(user,{cmd="note",note="act",arg1="You are not holding a balloon."})
	
	end

end

-----------------------------------------------------------------------------
--
-- return the balloon prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function zeegrind_hook_take(user,vobj)

local vroom=vobj_get_vroom(vobj)

local zroom=gtab.rooms[vroom.class]
if not zroom then zroom=gtab.rooms[0] end

local weapon=gtab.get_weapon_info( zroom.weapons[ tonumber(vobj.props.id) ] )

local vuser

	if user then vuser=data.ville.users[string.lower(user.name)] end -- get vuser if a user is operating this

	day_flag_set(user.name,"zeegrind.balloon",weapon.fullname)

	gtab.check_balloon(vroom,vuser)
	
--	userqueue(user,{cmd="note",note="act",arg1="An excellent choice sir, or should I say madam?"})
end

-----------------------------------------------------------------------------
--
-- this expects to be wrapped in a coroutine so it can yield
-- run game logic for every vroom
--
-----------------------------------------------------------------------------
gtab.update_co_vroom = function(room,vroom)

if vroom.voyeurs_count == 0 then return end -- do nothing if the vroom is empty


local zd=gtab.manifest_zd(room,vroom)

	if zd.npc_wakeup <= gtab.now then -- thunk npcs

		zd.npc_wakeup = gtab.now + 60 -- dont sleep for more than 1 min
		
		gtab.zd_thunk_npcs(zd,room,vroom) -- update npcs, creating them first if they have gone missing
		
	end

end

-----------------------------------------------------------------------------
--
-- this expects to be wrapped in a coroutine so it can yield
-- run game logic for every room
--
-----------------------------------------------------------------------------
gtab.update_co_room = function(room)

room.ville_game=gtab -- call us when handling ville details in this room

if room.user_count == 0 then return end -- do nothing if the room is empty

local vroom=vroom_obtain(room)

	if vroom then
		gtab.update_co_vroom(room,vroom)
	end
	
	for i,v in pairs(vroom.rooms) do
	
		local sub_vroom=data.ville.vobjs[v]
	
		if sub_vroom then
		
			gtab.update_co_vroom(room,sub_vroom)
		
		end
		
	end

end

-----------------------------------------------------------------------------
--
-- this expects to be wrapped in a coroutine so it can yield
--
-----------------------------------------------------------------------------
gtab.update_co = function()

gtab.now=lanes.now_secs() -- remember a single global fixed time during this entire update cycle so things are synced


	for n,b in pairs(gtab.room_names) do

		local room=get_room(n) -- get room from name
		
		if not room then
		
			gtab.room_names[n]=nil
			
		else
		
			gtab.update_co_room(room) -- update all zeegrind rooms
		
		end
		
	
	end

end


-----------------------------------------------------------------------------
--
-- a local update function, sits in a coroutine handling requests
--
-- this functon is called whenever we get a chance
--
-----------------------------------------------------------------------------
gtab.update = function()

	if data.ville.deville then return end -- dont try and update when we are reloading ville data

	if type(gtab.co)~="thread" then gtab.co=nil end -- no co yet
	if gtab.co and coroutine.status(gtab.co)=="dead" then gtab.co=nil end -- the co died
	
	if not gtab.co then -- need to create a new co
	
		gtab.co=coroutine.create(gtab.update_co)
	
	end

	local ret,_ret=coroutine.resume(gtab.co)
	if ret~=true then
		dbg('\n'.._ret..'\n')
		gtab.co=nil
	end

end

-----------------------------------------------------------------------------
--
-- Build and check environment
--
-- called after a spew: reload to make sure everything is setup OK
--
-----------------------------------------------------------------------------
gtab.build_and_check_environment = function()

dbg("checking zeegrind deville data\n")

	for n,zd in pairs(gtab.zds) do
	
		local vroom=get_vroom(zd.name)
		local room=vroom_get_room(vroom)
		
		if zd then
		
			for i,npc in ipairs(zd.npcs) do -- remove each npc
			
				local vuser=vobj_get_vobj_str(npc.id)
				
				if vuser then
				
					vroom_leave(vuser)
					vuser_destroy(vuser)

				end
			
			end
			
			zd.npcs={}
			
		end
		
		if not room then -- room has escaped us so kill the zd
			gtab.destroy_zd(zd)
		end
	
	end
	
-- add zeegrind game to each room named *.zeegrind.*

	gtab.room_names={}
	for n,r in pairs(data.rooms) do
	
		local aa=str_split(".",n)
		
		if aa[2]=="zeegrind" then
			
			gtab.room_names[n]=true
			r.ville_game=gtab
			r.novillebot=true
			r.locked="ville"
			r.max_users=16

		end
	end
	
	queue_update( gtab ) -- queueing again will just replace the old one so its safe to do multiple times
	
	gtab.update() -- create anything we need
end

-----------------------------------------------------------------------------
--
-- reset stuff on the day tick
--
-----------------------------------------------------------------------------
gtab.day_tick = function()

	gtab.roll_levels() -- create new settings for today
--[[	
	for n,b in pairs(gtab.room_names) do

		local room=get_room(n) -- get room from name
		
		if room then
		
			for v,b in pairs(room.users) do
	
				if v.client then
			
					if v.room==room then
					
						local vuser=data.ville.users[string.lower(v.name)]
						local vroom=get_vroom(room.name)
						
						if vuser and vroom then -- reset everyone to initial room
							vroom_join(vroom,vuser)
						end
					end
				end
			end
		end
	end
]]
end

-----------------------------------------------------------------------------
--
-- add zeegrind to this newly created room
--
-----------------------------------------------------------------------------
gtab.add_to_room = function(room)

	room.ville_game=gtab
	room.novillebot=true
			
	gtab.room_names[room.name]=true
	gtab.update()

end
