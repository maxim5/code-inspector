
--
-- data.* should be used for persistant vars as the plan is for this file to reload on command
--



-----------------------------------------------------------------------------
--
-- connect this user to the mux, if not already
--
-----------------------------------------------------------------------------
function mux_connect(user)
--dbg("muxconnected"," ",dbg_client(client),"\n")

local muxclient
local mux={}

	if user.mux then return user.mux end -- already connected

	muxclient = socket.connect(cfg.mux_server, cfg.mux_port)
	
	if not muxclient then return nil end -- connection error
	
	muxclient:settimeout(0.001) -- make sure we don't block in accept
	connections:insert(muxclient)
	
	mux.client=muxclient
	mux.user=user
	user.mux=mux
	
	data.muxouts[muxclient]=mux
	
	if not user_confirmed(user) then -- auto guest login
	
		mux_send(mux.client,"connect guest\n")
		
	end
	
	return mux
	
end
				
-----------------------------------------------------------------------------
--
-- client disconnect, remove mux from user
--
-----------------------------------------------------------------------------
function mux_disconnected(client,error)
--dbg("muxdisconnected ",error or ""," ",dbg_client(client),"\n")

local mux=data.muxouts[client]

	if not mux then return end
	
	data.muxouts[client]=nil
	
	if mux.user then
		mux.user.mux=nil
		mux.user=nil
	end

	client:close()
	connections:remove(client)
	
end

-----------------------------------------------------------------------------
--
-- client time out
--
-----------------------------------------------------------------------------
function mux_timedout(client)

--ignore

end

-----------------------------------------------------------------------------
--
-- send a line from a client
--
-----------------------------------------------------------------------------
function mux_send(client,line)

	client:settimeout(0.00001) -- this is a hack fix?
	client:send(line)

end

	
-----------------------------------------------------------------------------
--
-- receive a line from a client
--
-----------------------------------------------------------------------------
function mux_received(client,line)
--dbg("muxreceived ",line," ",dbg_client(client),"\n")

	client:settimeout(0.00001) -- this is a hack fix?
	
local mux=data.muxouts[client]
local msg

	if not mux then return end

	if mux.user then
	
		msg={cmd="mux",txt=line}
			
		usercast(mux.user,msg)
	
	end

end


