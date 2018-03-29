--[[]]
local fail = {}
local manage = {}
local string = {}
string[5] = " has a bad day"
string[7] = " is on a failing spree"
local message = nil



local function reset()
    fail = {}
    message = nil
end
local function fail_message(num, cn)
    local manag = num
    manag = tostring(manag)
    local cn = tonumber(cn)
    local num = tonumber(num)
    if manag == "manage" then
        messages.notice(cn, players.all(), " green<name<"..cn..">> managed to frag someone blue<(failing)>")
        manage[server.player_sessionid(cn)] = false
        return 1
    end
    if not string[num] then return end
    local color = "red"
    if num == 3 or num == 2 then
        color = "orange"
    end
    local message = color.."<name<"..cn..">>"..(string[num])
    messages.fail(cn, players.all(), message) 

end
local function failf (cn)
    if fail[server.player_sessionid(cn)] == nil then
        fail[server.player_sessionid(cn)] = 1
        return
    end
    fail[server.player_sessionid(cn)] = fail[server.player_sessionid(cn)] + 1
    if fail[server.player_sessionid(cn)] == 5 then
        fail[server.player_sessionid(cn)] = 5
        fail_message(5, cn)
        return
    end
    if fail[server.player_sessionid(cn)] == 7 then
        fail_message(7, cn)
        fail[server.player_sessionid(cn)] = 7
        manage[server.player_sessionid(cn)] = true
        return
    end

end
local function own (cn)
    if manage[server.player_sessionid(cn)] == nil then
        manage[server.player_sessionid(cn)] = false
    end
    if fail[server.player_sessionid(cn)] == nil then
       fail[server.player_sessionid(cn)] = 1
    end
    if fail[server.player_sessionid(cn)] ~= 0 then
        fail[server.player_sessionid(cn)] = fail[server.player_sessionid(cn)] - 1
    end
    if manage[server.player_sessionid(cn)] then
        fail_message("manage", cn)
        manage[server.player_sessionid(cn)] = false
    end
end
server.event_handler("mapchange", function()
    reset()

end)
server.event_handler("frag", function(actor, target)
    own(target)
    failf(actor)
end)
server.event_handler("suicide", function(cn)
    failf(cn)
end)
--[[]]

