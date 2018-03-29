#!/usr/bin/env lua
local json = require("json")
local socket = require("socket")
local mime = require("mime")
local tcp = socket.tcp()
local sf = string.format

local page = "sample.json"
local action = print
if not arg[1] and not arg[2] then
    print("You need to specify a valid user name and password!")
    print("Example: "..arg[0].." thelinx secret [filter] [action]")
    return
end
if arg[3] then
    if tonumber((arg[3]:gsub(",", ""))) then
        page = "filter.json?follow="..arg[3]
    else
        page = "filter.json?track="..arg[3]
    end
end
if arg[4] then
    function action(string)
        print(string)
        os.execute(sf(arg[4], string:gsub("<", "&lt;"):gsub(">", "&gt;")))
    end
end

tcp:settimeout(99999999)
tcp:connect("stream.twitter.com", 80)
tcp:send(sf([[
GET /1/statuses/%s HTTP/1.0
Host: stream.twitter.com
Authorization: Basic %s

]], page, mime.b64(arg[1]..":"..arg[2])))

while true do
    local tw = tcp:receive("*l")
    if not tw then return end
    if tw:len() > 50 then
        local t = json.decode(tw)
        if t.user and t.text then
            action(sf("<%s> %s", t.user.screen_name, t.text))
        end
    end
end
