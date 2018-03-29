
--
-- data.* should be used for persistant vars as the plan is for this file to reload on command
--


-----------------------------------------------------------------------------
--
-- fill in basic data if it does not already exist
--
-----------------------------------------------------------------------------
function client_new_data(flags)

	spew_setup(flags)
	
--	dbg(serialdbg(data.brains,1))

end

-----------------------------------------------------------------------------
--
-- client connect
--
-----------------------------------------------------------------------------
function client_connected(client,flavour)
--dbg("connected"," ",dbg_client(client)," : ",flavour or "","\n")

	local ctab={}
	data.clients_tab[client]=ctab

	if flavour=="telnet" then
		ctab.telnet=true
		ctab.spew_ok_to_send=true
	elseif flavour=="irc" then
		ctab.irc=true
	end
	
	client:settimeout(0.00001) -- this is a hack fix?

local user=new_user{name="me",client=client}
	

end
				
-----------------------------------------------------------------------------
--
-- client disconnect
--
-----------------------------------------------------------------------------
function client_disconnected(client,error)
--dbg("disconnected ",error," ",dbg_client(client),"\n")

local user=data.clients[client]

--dbg("disconnect ",tostring(client)," ",tostring(user and user.name),"\n")


	data.clients_tab[client]=nil
		
	del_user(user)

end

-----------------------------------------------------------------------------
--
-- has this client performed a handshake?
--
-----------------------------------------------------------------------------
function client_handshake_done(client)

local ctab=data.clients_tab[client]

	if ctab.spew_ok_to_send then return true end
	
	return false

end

-----------------------------------------------------------------------------
--
-- has this client performed a handshake?
--
-----------------------------------------------------------------------------
function client_handshake_set(client,flavour)
--dbg("handshake"," ",dbg_client(client)," : ",flavour or "","\n")

local ctab=data.clients_tab[client]

	ctab.spew_ok_to_send=true
	
	
	if flavour=="websocket_old" then
		ctab.websocket_old=true
	elseif flavour=="websocket" then
		ctab.websocket=true
	end
	
	client_send(client,"") -- send anything we are waiting to send
end


-----------------------------------------------------------------------------
--
-- needed to grab bytes etc from the websocket stream
--
-----------------------------------------------------------------------------
local sb=string.byte
local function grab1(s,n)
	return sb(s,n)
end
local function grab2(s,n)
	return (sb(s,n)*256)+sb(s,n+1)
end
local function grab4(s,n)
	return (sb(s,n)*256*256*256)+(sb(s,n+2)*256*256)+(sb(s,n+2)*256)+sb(s,n+3)
end
local function grab8(s,n)
	return (grab4(s,n)*(256*256*256*256)) + grab4(s,n+4)
end


-----------------------------------------------------------------------------
--
-- fiddle with line terminators a bit
--
-----------------------------------------------------------------------------
local function wrap(ctab,line)

	if ctab.telnet then
	
		return line
		
	elseif ctab.irc then
	
		return line.."\r\n"
		
	elseif ctab.websocket then
	
		local len=#line
		local pre={}
		
		pre[#pre+1]=string.char(0x81)
		if len<126 then -- small packet

			pre[#pre+1]=string.char(len)
		
		elseif len<65535 then -- large packet
		
			pre[#pre+1]=string.char(126)
			
			pre[#pre+1]=string.char(math.floor(len/256))
			pre[#pre+1]=string.char(len%256)

		else -- really large packet which we do not send...
			return ""
		end
		
		pre[#pre+1]=line -- include payload
		
		local s= table.concat(pre)

-- dbg(string.format("%02x %02x %02x %02x ... %d\n",sb(s,1),sb(s,2),sb(s,3),sb(s,4),#s))
		
		return s
		
	elseif ctab.websocket_old then
	
		return "\0" .. line .. "\255"
		
	else
	
		return line.."\n\0"
				
	end
end

-----------------------------------------------------------------------------
--
-- send a line to a client
--
-----------------------------------------------------------------------------

function client_send(client,line)

local ctab=data.clients_tab[client]

--dbg("send:"..line.."\n")

local user=data.clients[client]

	if ( not client ) or ( not line ) or ( not user ) then return end
	
	client:settimeout(0.00001) -- this is a hack fix?
	
	if ctab.spew_ok_to_send then
	
		if ctab.spew_send_cache then
		
			for i,line in ipairs(ctab.spew_send_cache) do
				if line~="" then	
					client:send(wrap(ctab,line))			
				end
			end
			
			ctab.spew_send_cache=nil -- all sent
		
		end
		
		if line~="" then	
			client:send(wrap(ctab,line))			
		end
			
	else -- delay sending until after connection has been confirmed
	
		if line~="" then	
			ctab.spew_send_cache=ctab.spew_send_cache or {}
			ctab.spew_send_cache[#ctab.spew_send_cache+1]=line
		end
		
	end

end

-----------------------------------------------------------------------------
--
-- receive some data from a client
--
-----------------------------------------------------------------------------
function client_received_websocket(client,line)
-- dbg("WS received ",dbg_client(client),"\n")
	
	local ctab=data.clients_tab[client]
	
	if line then
		if ctab.lineparts then -- continue our cache
			ctab.lineparts=ctab.lineparts..line			
		else -- start new cache
			ctab.lineparts=line
		end
	end
	local s=ctab.lineparts
	if not s then return end
	local slen=#s
	if slen==0 then ctab.lineparts=nil return end --no more line


	if slen<2 then return end -- wait for more data


	local user=data.clients[client]


-- dbg(string.format("%02x %02x %02x %02x ... %d\n",sb(s,1),sb(s,2),sb(s,3),sb(s,4),slen))

	local b1=grab1(s,1)
	local b2=grab1(s,2)
	
	local header_size=2
	local mask_size=0
	local data_size=0
	local size=0
	local mask={0,0,0,0}
	
	if b2>=128 then -- a mask (we should always have this)
		mask_size=4
		b2=b2-128
	end
	
	if b2==126 then -- 16bit length
	
		if slen<(2+2) then return end -- wait for more data
	
		header_size=header_size+2
		data_size=grab2(s,3)
			
	elseif b2==127 then -- 64bit length
	
		if slen<(2+8) then return end -- wait for more data
		
		header_size=header_size+8
		data_size=grab8(s,3)
	else
		data_size=b2
	end

	size=header_size + mask_size + data_size
	
-- dbg(string.format("%02x %02x\n",b1,b2))

-- dbg(header_size," , ",mask_size," , ",data_size," , ",size," , ",#s,"\n")

	if slen >= size then -- we have all data so process it

	local frame=b1%16
	
-- dbg("WS frame : ",frame," : ",size,"\n")

		if mask_size==4 then -- read the 4 mask bytes
			mask[1]=grab1(s,header_size+1)
			mask[2]=grab1(s,header_size+2)
			mask[3]=grab1(s,header_size+3)
			mask[4]=grab1(s,header_size+4)
		end

		if frame==0x1 then -- a text frame, which is what we want
		
			local sdat={}
			
			for i=1,data_size do
				local m=mask[((i-1)%4)+1]
				sdat[#sdat+1]=string.char(bit.bxor(m,grab1(s,header_size+mask_size+i)))
			end
			
			local sd=table.concat(sdat)
			
--dbg(sd,"\n")

			table.insert(user.linein,sd)
			queue_update(user)
			
			if not ctab.spew_ok_to_send then
			
				ctab.spew_ok_to_send=true -- flag as real connection, ok to start sending
				client_send(client,"")
				
			end

		end
		
		
		if slen > size then
			ctab.lineparts=s:sub(size+1) -- remove what we read
			return client_received_websocket(client) -- call us again to try and handle the rest 
		else
			ctab.lineparts=nil -- all used, remove it
		end
	end
		
end

function client_received(client,line)
	local ctab=data.clients_tab[client]

--dbg("received ",line," ",dbg_client(client),"\n")

	if ctab.websocket then return client_received_websocket(client,line) end

local user=data.clients[client]

local line_term="\0"

	if ctab.telnet then -- break on \n not \0
		line_term="\n"
	elseif ctab.irc then -- break on \n not \0
		line_term="\n"
	elseif ctab.websocket_old then -- break on \255 not \0
		line_term="\255"
	end

--dbg("received ",line," ",user_ip(user),"\n")
	
	if not ctab.spew_ok_to_send then
	
		ctab.spew_ok_to_send=true -- flag as real connection, ok to start sending
		client_send(client,"")
		
	end

	client:settimeout(0.00001) -- this is a hack fix?
	
	if user.lineparts then -- continue our cache
	
		user.lineparts=user.lineparts..line			
		
	else -- start new cache
	
		user.lineparts=line
		
	end
	
	if string.len(user.lineparts)>4096 then
		dbg("spamdrop 4096 "..user.name," : ",user.lineparts,"\n")
		user.lineparts=nil
		return
	end
	-- ignore large packets (we never need to send this much data?)
	
	local zero,linepart
	
	zero=string.find(user.lineparts,line_term)
	
	while zero do -- we have a command or more to split up
	
		if zero>1 then
		
			linepart=string.sub(user.lineparts,1,zero-1) -- command
			user.lineparts=string.sub(user.lineparts,zero+1) -- remainder

--dbg("handling: ",linepart,"\n")		
			table.insert(user.linein,linepart) -- do this line in the users coroutine
		else
			user.lineparts=string.sub(user.lineparts,zero+1) -- remainder
		end
		
		zero=string.find(user.lineparts,line_term)
	end
	
	if linepart then queue_update(user) end -- flag the users coroutine to update later as we sent it data

end

-----------------------------------------------------------------------------
--
-- remove this client from active clients
--
-----------------------------------------------------------------------------
function client_remove(client)

	data.clients_tab[client]=nil
	
	client:close()
	connections:remove(client)

end

