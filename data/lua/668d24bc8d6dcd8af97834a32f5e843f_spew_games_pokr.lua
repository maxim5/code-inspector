
data.gametypes=data.gametypes or {}


--[[ debugging, clear out old data
if data.gametypes["zeegrind"] then
	remove_update( data.gametypes["zeegrind"] ) -- remove for debug niceness
	data.gametypes["zeegrind"]=nil
end
]]

local gtab=data.gametypes["pokr"] or {}
data.gametypes["pokr"]=gtab -- update, keep unique gtab table
data.gametypes["poker"]=gtab -- also known as

gtab.name="pokr"

gtab.room_names=gtab.room_names or {} -- keeps track of active room names

gtab.zds=gtab.zds or {} -- vroom state data, keeps track of the game in this room

gtab.time=gtab.time or 0


dofile("spew_games_pokr_data.lua")


-- over ride with check for bot ( name of "*" )

local old_get_user=get_user
local function get_user(n)

	if n=="*" then -- get whale
	
	local zd=gtab.zds["public.pokr/play"]

	return zd.whale_user
	
	else return old_get_user(n) end
end

local old_get_vuser=get_vuser
local function get_vuser(n)

	if n=="*" then -- get whale
	
	local zd=gtab.zds["public.pokr/play"]
	
	return zd.whale_vuser
	
	else return old_get_vuser(n) end
end



-----------------------------------------------------------------------------
-- 
-- get or create the zd data for this vroom
-- 
-----------------------------------------------------------------------------
gtab.manifest_zd = function(room,vroom)

local zd=gtab.zds[vroom.name]

	if zd then return zd end
	
	zd={}
--dbg(vroom.name,"\n")
	gtab.zds[vroom.name]=zd
	
	zd.name=vroom.name -- name of vroom which can be used to find the zroom and room if all we have is a zd
	
	zd.wakeup=0 -- the next server time that things needs to be processed.
	
	room.ville_game=gtab -- link room to us
	gtab.room_names[room.name]=true -- remember to update this room
	
	zd.users={} -- users in this room, name->vuser
	zd.players={} -- users in playing the current hand of poker, name->vuser
	zd.winners={} -- who won the last round
	zd.losers={}
	zd.salutations={}
	
	zd.whale_cookies=80000
	
	zd.time=0 -- time for next state to run out
	zd.pot=0 -- room pot
	zd.bet=0 -- room bet level
	
	zd.state="wait" -- game state

	return zd
end

-----------------------------------------------------------------------------
-- 
-- destroy this zd data
-- 
-----------------------------------------------------------------------------
gtab.destroy_zd = function(zd)

	gtab.zds[zd.name]=nil

end


-----------------------------------------------------------------------------
-- 
-- which vroom should we put this user in
-- 
-----------------------------------------------------------------------------
gtab.vroom_which = function(room,user)

local vuser=data.ville.users[string.lower(user.name)]
	
local vroom=vroom_obtain(user.room)

	return vroom
end

-----------------------------------------------------------------------------
-- 
-- someone has joined this vroom
-- 
-----------------------------------------------------------------------------
gtab.vroom_join = function(vroom,vuser)

local ret={}
local room=vroom_get_room(vroom)

local zd=gtab.manifest_zd(room,vroom)

	zd.users[ vuser.owner ]=get_user(vuser.owner)
	
	zd.salutations[ vuser.owner ]=true

--	gtab.npc_create_pc(zd,name,vroom,vuser)

--	local zone=gtab.npc_loadzone_name(vuser.npc)
	
	local doordest
	
	local p={x=0,y=0,z=0}
	
	p=vroom_door_xyzt(vroom,vuser)
	
	propdata_set_xyzt(vuser,p,0)
	propdata_send_xyzt(vuser,"force")

	ret.walkto=vroom_random_xyzt(vroom)

	
--	gtab.zd_send_refresh(zd)
	
	return ret
	
end

-----------------------------------------------------------------------------
-- 
-- someone has left this vroom
-- 
-----------------------------------------------------------------------------
gtab.vroom_part = function(vroom,vuser)


local zd=gtab.zds[vroom.name]

	if zd.whale_vuser and zd.whale_user and zd.whale_vuser == vuser then -- when the whake keaves
	
		zd.whale_cookies = zd.whale_cookies + zd.whale_user.cookies -- save whale cookies on exit
		zd.whale_user.cookies=0
		
	end

	zd.salutations[ vuser.owner ]=nil
	
	zd.users[ vuser.owner ]=nil

--	zd.players[ vuser.owner ]=nil

end

-----------------------------------------------------------------------------
-- 
-- update this users balloon (take away their real one and replace with the game one)
-- 
-----------------------------------------------------------------------------
gtab.check_balloon=function(vroom,vuser)

if not vuser then return end

local room=vroom_get_room(vroom)
	if not room then return end
local zd=gtab.manifest_zd(room,vroom)
	if not zd then return end
	
local user=get_user(vuser.owner)

local balloon="0:float:0:0:0"
local speed=100

local room_bot=vobj_get_vobj_str(vroom.uniques.room_bot)

local check_name

--dbg("check balloon ",room.name,"\n")

	if room_bot==vuser then -- this is the rooms bot
		if room.name=="public.pokr" then -- the whale
		
			check_name="*"
			user=get_user("*")
		end

	elseif user then -- users display their cookies
	
		check_name=vuser.owner
		
	end
	
	if check_name then
		if zd.players[check_name] or zd.winners[check_name] then -- only players display cookies
		
			local size=user.cookies

--			if size>0 then
			
				size=40+force_floor((size/4000)*60)
				
				if size < 40 then size=40 end
				if size > 100 then size=100 end

				balloon="0:items1:2:"..size..":"..(50)..":"..user.cookies.."c"
				
--			end
		end
	end
	
	vobj_set(vuser,"balloon", balloon ) -- place it in hand
	vobj_set(vuser,"speed", speed )
	
	
	return true
end

-----------------------------------------------------------------------------
--
-- return the menu prop string for this kolumbo hook
--
-----------------------------------------------------------------------------
function gtab.door_menu(vobj)
	
	return "base/use"
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
local room_bot=vobj_get_vobj_str(vroom.uniques.room_bot)

--local zroom=gtab.rooms[vroom.class]
--if not zroom then zroom=gtab.rooms[0] end


	if vals[1]=="base use" then
	
		if vobj.props.dest=="./play" then
		
			if not user or user.cookies<100 then -- need money to get in
			
				if vroom and room_bot then
					usercast(user, vobj_build_say_msg(room_bot,"You will need at least 100 cookies to enter the game room, click me for more help.") )
				end
				
				return vobj.props.dest,true
				
			end
		
		end
	
	end

end


-----------------------------------------------------------------------------
--
-- check the setting of props on some special objects
--
-----------------------------------------------------------------------------
gtab.poker_set = function(vobj,prop,val,user)

local vroom=vobj_get_vroom(vobj)
	if not vroom then return end
local room=vroom_get_room(vroom)
	if not room then return end
local zd=gtab.manifest_zd(room,vroom)
	if not zd then return end

	if prop=="use" then
	
		local aa=str_split(" ",val)
		
		if aa[1]=="fold" then
		
			gtab.zd_fold(zd,string.lower(user.name))
		
		elseif aa[1]=="raise" then

			gtab.zd_raise(zd,string.lower(user.name),force_floor(tonumber(aa[2]) or 0))
		end
	
	
		return
	end

	vobj_set_vobj(vobj,prop,val,user)

end


-----------------------------------------------------------------------------
--
-- this player folds
--
-----------------------------------------------------------------------------
gtab.zd_fold = function(zd,n)

local v=zd.players[n]

	if not v then return end
	
local vroom=get_vroom(zd.name)
local room=vroom_get_room(vroom)
local user=get_user(n)

--dbg(n," fold\n")

	if zd.players[n] then
		zd.players[n]=nil
		zd.player_count=zd.player_count-1 -- one less player
	end
	
	if not vroom or not room then return end

	if user then
		feats_rank(user,"pokr_rank",-v.spent) -- cost of folding
					
		roomqueue(room,{cmd="act",frm=user.name,txt="folds"},false)
	end
	
	if zd.player_count==1 then -- only one player left, they win the entire pot and do not show their cards
	
		for n,v in pairs(zd.players) do
			local user=get_user(n)
			
			roomqueue(room,{cmd="act",frm=user.name,txt="wins ("..(zd.pot).."c)"},false)
			
			feats_rank(user,"pokr_rank",zd.pot) -- rank up for remaining user
	
			user.cookies=user.cookies+zd.pot
			zd.pot=0
			
		end
		
		zd.winners=zd.players
		
		zd.players={}
		zd.player_count=0
		
	end
	
	gtab.zd_send_refresh(zd)
end

-----------------------------------------------------------------------------
--
-- this player raises (to n, they also put their cookies in the pot)
--
-----------------------------------------------------------------------------
gtab.zd_raise = function(zd,n,raise)

local vroom=get_vroom(zd.name)
local room=vroom_get_room(vroom)

	if not vroom or not room then return end

local v=zd.players[n]
	if not v then return end
	
local user=get_user(n)
	if not user then return end

--dbg(n," raise ",raise,"\n")
	
local cost=0

	if zd.bet<raise then
	
		cost=raise-v.bet
		
		if user.cookies>=cost then -- ok to raise
		
			local tocall=zd.bet-v.bet
			local toraise=raise-zd.bet
		
			if tocall>0 then
				roomqueue(room,{cmd="act",frm=user.name,txt="calls ("..(tocall).."c) and raises ("..(toraise).."c)"},false)
			else
				roomqueue(room,{cmd="act",frm=user.name,txt="raises ("..(toraise).."c)"},false)
			end
			
			v.bet=raise
			zd.bet=raise
			zd.pot=zd.pot+cost
			user.cookies=user.cookies-cost
			
			v.spent=v.spent+cost

			local now=lanes.now_secs()
			local tim=force_floor(zd.time-now)

			if tim<10 then -- bump up to 10secs if less remains
				zd.time=now+10
			end
			
			v.raise=v.raise*2
			if v.raise>user.cookies then v.raise=user.cookies end
			
			gtab.zd_send_refresh(zd)
		end
		
	end
end

-----------------------------------------------------------------------------
--
-- tell everyone about a new bet or other statechange
--
-----------------------------------------------------------------------------
gtab.zd_send_refresh = function(zd)

local vroom=get_vroom(zd.name)
local room=vroom_get_room(vroom)
local poker=vobj_get_vobj_str(vroom.uniques.poker)
local hand=vobj_get_vobj_str(vroom.uniques.hand)

local now=lanes.now_secs()
local tim=force_floor(zd.time-now)

	for n,u in pairs(zd.users) do -- let the non players know what is going on
	
		local vuser=get_vuser(n)
		
		if not zd.players[n] then -- skip the players as we have already dealt with them
			vobj_trigger_vuser(vuser,poker,"poker", tim..":"..(zd.pot)..":"..(zd.bet)..":"..(0)..":"..(0) ) -- signal next round
		else
			local v=zd.players[n]
			vobj_trigger_vuser(vuser,poker,"poker", tim..":"..(zd.pot)..":"..(zd.bet)..":"..(v.bet)..":"..(v.raise) ) -- signal next round
		end
		
		gtab.check_balloon(vroom,vuser)
	end
	local vuser=get_vuser("*")
	gtab.check_balloon(vroom,vuser)
	
end



gtab.bot_menu="base/Help/Poker/Call/Fold/Raise/Deal/Cookies/Give me some cookies!"

gtab.bot_responses=
{
	["base help"]					= "Click and hold on me then move the mouse to an option and release the button to select a help topic.",
	["base poker"]					= "Wet Pokr is a Texas Holdem variant, you should ask the great god google to teach you about Texas Holdem.",
	["base call"]					= "In Wet Pokr there are no turns and you call automatically. Watch the counter in the bottom left to see when.",
	["base fold"]					= "If the bet gets to high for you and you dont want to call then click fold before the call timer hits zero.",
	["base raise"]					= "You may raise at anytime but whenever you do the call timer may be raised to give others time to react.",
	["base deal"]					= "You must have at least 10 cookies to join in a deal, joining happens automatically like calling.",
	["base cookies"]				= "Sign up for 100 cookies and play our other games to earn more cookies, you will need cookies to play.",
}

-----------------------------------------------------------------------------
--
-- perform game logic and say when we should be called again
--
-----------------------------------------------------------------------------
gtab.zd_update = function(zd,room,vroom)

local room_bot=vobj_get_vobj_str(vroom.uniques.room_bot)

	if room_bot and not room_bot.set then
	
		if vroom.class=="title" then
			vobj_set(room_bot,"balloon","0:items1:12:100:50") -- welcome to wetgenes, I love you.
		end
		
		vobj_set(room_bot,"menu",gtab.bot_menu)
		
		room_bot.set=function(vobj, prop , val ,user)

--dbg(prop," ",val,"\n")
			if prop=="use" then
			
			local s=string.lower(val)
			
				if s=="base give me some cookies!" then -- special request
				
					local ip=user_ip(user)
					local done=day_flag_get(ip,"pokr_cookies")
					
					if not done then
					
						day_flag_set(ip,"pokr_cookies")
						usercast(user, vobj_build_say_msg(room_bot,"OK here are 100 cookies.") )
						user.cookies=user.cookies+100
						
					else
						usercast(user, vobj_build_say_msg(room_bot,"Sorry but you or someone else on the same IP has already taken the 100 free cookies for today.") )
					end
				
				else

					local r=gtab.bot_responses[s]
					
					if r then -- basic talking

						usercast(user, vobj_build_say_msg(room_bot,r) )
					
					end
				end

				return
			end
			
			if data.ville.set[vobj.type] then -- use custom prop setter
				
				return data.ville.set[vobj.type](vobj,prop,val,user)
			end
			
			vobj_set_vobj(vobj,prop,val,user)			vobj_set_vobj(vobj,prop,val,user)
		end
	end

	if vroom.class=="title" then -- just the title room not the main game
	
	
		if room_bot then
		
			for n,b in pairs(zd.salutations) do
			
				local user=get_user(n)
				
				if user then
				
					usercast(user, vobj_build_say_msg(room_bot,"Welcome to WetPokr, I love you. Please click me for more help.") )
			
				end
			
			end
			zd.salutations={}
		
			vuser_walkto(room_bot,{x=20,y=0,z=20})
		
		end
		
		zd.wakeup = gtab.now + 10 -- call again in 10 secs
	
		return
	elseif room_bot then
		-- keep bot in check
		vuser_walkto(room_bot,{x=20,y=0,z=300})
	end

local poker=vobj_get_vobj_str(vroom.uniques.poker)
local hand=vobj_get_vobj_str(vroom.uniques.hand)

	zd.wakeup = gtab.now + 1 -- call again in one sec
	
	if not poker or not hand then return end -- something is borked in the room
	
	poker.set=gtab.poker_set -- make sure callback is enabled

-- count players
	zd.player_count=0
	for i,v in pairs(zd.players) do
		zd.player_count=zd.player_count+1
	end
	if zd.player_count==0 then
	
		zd.state="wait" -- go back to waiting state, nobody is playing and the pot just carries over
	
	end
	
	
	local function next_state(d)
	
		for n,v in pairs(zd.players) do
--dbg(n,"\n")

			local user=get_user(n)
			local vuser=get_vuser(n)
			
--dbg(user.name," ",user.cookies,"\n")
			
			if not user then -- they have dropped/left the game (leaving the room wont trigger this) the user will still auto call
			
				gtab.zd_fold(zd,n) -- make them fold (need to make this more persistant later)
				
				if zd.player_count==0 then break end -- this caused a win
				
			else
		
				v.raise=zd.raise
				if v.raise>user.cookies then v.raise=user.cookies end
				
				local tocall=zd.bet-v.bet
				
				if user.cookies < tocall then tocall=user.cookies end -- all in, get to call for free untill the end of the hand
				
				v.spent=(v.spent or 0)+tocall
				
				user.cookies=user.cookies-tocall
				zd.pot=zd.pot+tocall
				
				v.bet=v.bet+tocall
				
				if tocall>0 then -- say this uses called
				
					if zd.state=="wait" then
						roomqueue(room,{cmd="act",frm=user.name,txt="joins ("..(tocall).."c)"},false)
					else
						roomqueue(room,{cmd="act",frm=user.name,txt="calls ("..(tocall).."c)"},false)
					end
				end
				
				if d.dealhand then
				
					v.cards=gtab.deal()..gtab.deal()
					
				end
				vobj_trigger_vuser(vuser,hand,"cards", v.cards ) -- display cards to each player, but only send their own hand
				
				vobj_trigger_vuser(vuser,poker,"cards", zd.cards ) -- put cards on table
				
				if zd.cards~="-" and v.cards~="-" then
				
					v.dat=gtab.get_hand_data( zd.cards .. v.cards )
					
					v.dat.fromhand=0 -- count number of cards in best hand that came from our hand (not totally precise but useful)
					for c=1,string.len(v.cards),2 do
						local s=string.sub(v.cards,c,2)
						if string.find(v.dat.best_5cards,s) then
							v.dat.fromhand=v.dat.fromhand+1
						end
					end
				
					vobj_trigger_vuser(vuser,hand,"title", v.dat.best_hand_caps ) -- tell each player the best hand they have
					
--					roomqueue(room,{cmd="say",frm=user.name,txt="DBG ("..(zd.cards .. v.cards)..")"})
				else
				
					vobj_trigger_vuser(vuser,hand,"title", "-" ) -- no hand
				end
			end
		end
		
		if d.showdown and zd.player_count>1 then -- this is the show down stage, the remaining hands are shown and compared and the pot is awarded to the winners
		
			local win={}
			local win_cookies=0
			for n,v in pairs(zd.players) do
				
				local user=get_user(n)
				
				if not win[1] then -- noone to compare to
				
					win[1]=v
				else -- compare
				
					local result= gtab.compare_cards(win[1].dat,v.dat)
					
					if result==-1 then -- we lose, winers remain
					
					elseif result==1 then -- we win, replace winners
					
						win={v}
					
					elseif result==0 then -- its a draw, add us to list of winners
					
						table.insert(win,v)
					end
				end
				
				feats_rank(user,"pokr_rank",-v.spent) -- cost to stay in
				
				if user.cookies==0 then -- hand out all in award
					feats_award(user,"pokr_all_in")
				end
			end
			
			if #win> 0 then -- how much each winner gets
			
				win_cookies=force_floor(zd.pot/#win)
			end
			for i,v in ipairs(win) do -- link by name for easy lookup
				win[v.id]=v
			end
			
			if #win>1 then -- hand out pot share feat
				for i,v in ipairs(win) do
					local user=get_user(v.id)
					feats_award(user,"pokr_split")
				end
			end
			
			zd.winners={}
			for n,v in pairs(zd.players) do
			local user=get_user(n)
			local vuser=get_vuser(n)
			local winstr
			
				if win[n] then -- this is a winner
				
					zd.winners[n]=v
					
					winstr=" and wins "..win_cookies.."c"
					
					user.cookies=user.cookies+win_cookies
					zd.pot=zd.pot-win_cookies
					
					feats_rank(user,"pokr_rank",win_cookies) -- rank up
				
				else -- this is a loser
				
					winstr=""
					
					if user.cookies==0 then -- hand out wipe out to the winners
						for i,v in ipairs(win) do
							local user=get_user(v.id)
							feats_award(user,"pokr_wipe_out")
						end
					end
				end
			
				roomqueue(room,{cmd="act",frm=user.name,txt="has "..v.dat.best_hand_english..winstr},false)
				
				if user.client then
					if gtab.pokr_award_hand(user,v.dat) then -- we got an award
					
					end
				end
			end
			
			zd.players={}
			zd.player_count=0
		
		end
		
		for n,u in pairs(zd.users) do -- let the non players know what is going on
		
			local vuser=get_vuser(n)
			
			if not zd.players[n] then -- skip the players as we have already dealt with them
			
				if d.dealhand then
					vobj_trigger_vuser(vuser,hand,"cards", "-" ) -- make sure non players have no cards
				end
				
				vobj_trigger_vuser(vuser,poker,"poker", "20:"..(zd.pot)..":"..(zd.bet)..":"..(0)..":"..(0) ) -- signal next round
				vobj_trigger_vuser(vuser,poker,"cards", zd.cards ) -- put cards on table
			
				vobj_trigger_vuser(vuser,hand,"title", "-" ) -- no hand
			else
				local v=zd.players[n]
				vobj_trigger_vuser(vuser,poker,"poker", "20:"..(zd.pot)..":"..(zd.bet+d.betplus)..":"..(v.bet)..":"..(v.raise) ) -- signal next round
			end
--dbg("check ",n,"\n")
			gtab.check_balloon(vroom,vuser)
		end
		
		local vuser=get_vuser("*")
		gtab.check_balloon(vroom,vuser)
		
		zd.bet=zd.bet+d.betplus
		
	end
	
	if zd.time-9 < gtab.now and zd.thunk~=zd.time then -- bot thunks before time out
	
		zd.thunk = zd.time -- mark thunk as done
		
		if zd.players["*"] then -- the whale is playing
		
			local v=zd.players["*"]
			local u=get_user("*")
			
			local fold_chance=0
			local raise_chance=0
			local r=math.random()
			
			local tocall=zd.bet-v.bet
			if u.cookies < tocall then tocall=u.cookies end
			
			local mycookies=(v.bet+u.cookies) -- total cookies risked + inhand
			
			local betpref=
			{
				["high cards"]=0,01,
				["pair"]=0.02,
				["two pair"]=0.05,
				["three of a kind"]=0.1,
				["straight"]=0.2,
				["flush"]=0.3,
				["full house"]=0.5,
				["four of a kind"]=0.8,
				["straight flush"]=1.0,
			}
			local bet=0
			if v and v.dat and v.dat.best_hand then
				bet=betpref[v.dat.best_hand] or 0
			end
			bet=force_floor((v.bet+u.cookies) * bet )
			
			if v.dat and v.dat.fromhand then -- become less sure the more cards we share with other players
				if v.dat.fromhand==2 then 
					bet=force_floor(bet)
				elseif v.dat.fromhand==1 then 
					bet=force_floor(bet*0.75)
				elseif v.dat.fromhand==0 then 
					bet=force_floor(bet*0.25)
				end
			end
			
			if bet<=10 then bet=10 end -- minimum bet
			
			if zd.state=="0" then -- initial bet
			
				fold_chance=(tocall/bet)-1
				raise_chance=0
			
			elseif zd.state=="3" then -- check our hand
			
				fold_chance=(zd.bet/bet)-1
				raise_chance=bet/zd.bet
				
			elseif zd.state=="4" then -- check our hand
			
				fold_chance=(zd.bet/bet)-1
				raise_chance=bet/zd.bet
			
			elseif zd.state=="5" then -- final show down
			
				fold_chance=(zd.bet/bet)-1
				raise_chance=bet/zd.bet
			
			end
			
			local commit
			if mycookies>0 then
				commit=v.bet/mycookies
			else
				commit=1
			end
			if commit>0.5 then fold_chance=0 end -- we are commited (50% in), bluff it the rest of the way :)

			local risk
			if mycookies>0 then -- dont bother folding when the cost of staying in is low
				risk= tocall*2 / mycookies
			else
				risk=0
			end
			fold_chance=fold_chance*risk
			
			if raise_chance <= 0.1  then raise_chance=0.1 end
			if fold_chance  <= 0.0  then fold_chance= 0.0 end
			
			if raise_chance >= 0.5  then raise_chance=0.5 end
			if fold_chance  >= 0.5  then fold_chance= 0.5 end
			
			if v.bet==zd.bet then fold_chance=-1 end -- dont raise then fold

--dbg("ygor",zd.state," ",bet," ",force_floor(fold_chance*100)," ",force_floor(raise_chance*100)," ",force_floor(r*100),"\n")

			if r<=fold_chance then -- the random god says to fold?
				gtab.zd_fold(zd,"*")
			elseif r>=(1-raise_chance) then -- the random god says to raise?
				gtab.zd_raise(zd,"*",zd.bet+v.raise)
			end
		end
		
	end
	
	if zd.state=="wait" then -- starting state
	
		if zd.time < gtab.now then -- try and deal cards every 20 secs
		
			zd.time=gtab.now+20 -- time to wait
			
			gtab.shuffle() -- keep the cards shuffled
			
			zd.winners={}
			zd.players={}
			zd.cards="-"
			zd.player_count=0
			
			local ips={}
			for n,v in pairs(zd.users) do
				if v.cookies>=10 then -- must have some cookies to play
				
					local ip=user_ip(v) -- only accept one player from each ip
					
					if ( not ips[ip] ) or is_god(v.name) then
					
						ips[ip]=true
						zd.players[n]={ id=n, name=n , bet=0 , cards="-" , raise=1, spent=0 }
						zd.player_count=zd.player_count+1
						
					end
				end
			end
			
			if ( zd.whale_cookies > 0 or room.brain.user.cookies>0 ) and room.name=="public.pokr" then -- deal in the whale, but only in the main room...
			
			
				zd.whale_user=room.brain.user
				zd.whale_vuser=room_bot
			
				room.brain.user.cookies=room.brain.user.cookies+zd.whale_cookies
				zd.whale_cookies=0
				
				zd.players["*"]={ id="*", name=zd.whale_user.name , bet=0 , cards="-" , raise=1}
				zd.player_count=zd.player_count+1
				
			end
			
			if zd.player_count<2 then -- not enough players or all players have same IP, silent fail
			
				zd.bet=0
				zd.raise=0
				
				zd.players={}
				zd.player_count=0
			
				next_state({betplus=0}) -- no players so this is just a reset
				
			else -- start the game
			
			
				zd.bet=1
				zd.raise=1
				zd.time=gtab.now+20 -- time to think before auto call
				
				next_state({betplus=1,dealhand=true})
				
				zd.state="0" -- switch to next state
			end
			
		end

	elseif zd.state=="0" then -- everyone has two cards, wait for betting to end then deal 3 cards to the table
	
		if zd.time < gtab.now then -- everyone left in calls
		
			zd.cards=gtab.deal()..gtab.deal()..gtab.deal()

			zd.time=gtab.now+20 -- time to think before auto call
			
			next_state({betplus=1})
			
			zd.raise=math.ceil(zd.bet/10)
			
			zd.state="3" -- switch to next state
		end
		
	elseif zd.state=="3" then -- 3 cards on the table, wait for betting to end then deal 3 cards to the table
	
		if zd.time < gtab.now then -- everyone left in calls
		
			zd.cards=zd.cards..gtab.deal()

			zd.time=gtab.now+20 -- time to think before auto call
						
			zd.raise=math.ceil(zd.bet/10)
			
			next_state({betplus=1})
			
			zd.state="4" -- switch to next state
		end
		
	elseif zd.state=="4" then -- 4 cards on the table, wait for betting to end then deal 3 cards to the table
	
		if zd.time < gtab.now then -- everyone left in calls
		
			zd.cards=zd.cards..gtab.deal()

			zd.time=gtab.now+20 -- time to think before auto call
						
			zd.raise=math.ceil(zd.bet/10)
			
			next_state({betplus=1})
			
			zd.state="5" -- switch to next state
		end
		
	elseif zd.state=="5" then -- 5 cards on the table, wait for betting to end then handle results and go back to wait state
	
		if zd.time < gtab.now then -- everyone left in calls
		
--			zd.cards=zd.cards..gtab.deal()

			zd.time=gtab.now+20 -- time to wait
						
			zd.raise=0
			
			next_state({betplus=0, showdown=true })
			
			zd.state="wait" -- switch to next state
		end
		
	end


--dbg(zd.name," ",zd.state," ",gtab.now,"\n")


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

	if zd.wakeup <= gtab.now then -- thunk npcs

		zd.wakeup = gtab.now + 60 -- dont sleep for more than 1 min
		
		gtab.zd_update(zd,room,vroom) -- update the game associated with this vroom
		
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

dbg("checking pokr data\n")


	for n,zd in pairs(gtab.zds) do -- runtime patchup
	
		zd.losers=zd.losers or {}
		zd.salutations=zd.salutations or {}
		zd.whale_cookies=80000--zd.whale_cookies or 0
	
	end

-- add  game to each room named *.gamename.*

	gtab.room_names={}
	for n,r in pairs(data.rooms) do
	
		local aa=str_split(".",n)
		
		if aa[2]=="pokr" then
			
			gtab.room_names[n]=true
			r.ville_game=gtab
--			r.novillebot=true
			r.locked="ville"
--			r.max_users=16

		end
	end
	
	queue_update( gtab ) -- queueing again will just replace the old one so its safe to do multiple times
	
	gtab.update() -- create anything we need
end



-----------------------------------------------------------------------------
--
-- add game to this newly created room
--
-----------------------------------------------------------------------------
gtab.add_to_room = function(room)

	room.ville_game=gtab
--	room.novillebot=true
			
	gtab.room_names[room.name]=true
	gtab.update()

end

-----------------------------------------------------------------------------
--
-- do stuff every hour
--
-----------------------------------------------------------------------------
gtab.hour_tick = function()

	local zd=gtab.zds["public.pokr/play"]
	if zd then
		if zd.whale_cookies<80000 then zd.whale_cookies=80000 end
	end

end
-----------------------------------------------------------------------------
--
-- do stuff every day
--
-----------------------------------------------------------------------------
gtab.day_tick = function()

	local zd=gtab.zds["public.pokr/play"]
	if zd then
		if zd.whale_cookies<160000 then zd.whale_cookies=160000 end
	end

end
