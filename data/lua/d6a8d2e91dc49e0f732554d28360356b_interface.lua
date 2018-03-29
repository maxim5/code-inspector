local L, this = ...
this.name = "LCORE Platform Agnosticism API"
this.version = "1.1"
this.status = "production"
this.desc = "Provides an interface to platform-specific APIs."

local core
local api

core = {
	platform = nil,
	platform_name = nil,
	autoload = true,
	platforms = {},

	deduce_platform = function(self)
		if (self.platform_name) then
			return self.platform_name
		else
			if (love) then
				return "lcore.platform.love"
			else
				return "lcore.platform.vanilla"
			end
		end
	end,

	set_platform = function(self, name)
		self:load_platform(name)
	end,

	load_platform = function(self, name)
		name = name or self:deduce_platform()

		if (self.platforms[name]) then
			self.platform = self.platforms[name]
			return self.platforms[name]
		end

		local loaded, err = L:get(name .. ".core", true)

		if (loaded) then
			self.platforms[name] = loaded
			self.platform = loaded
		else
			return nil, L:error("Could not load platform '" .. (name or "[nil]") .. "', got error: " .. tostring(err), 1)
		end
	end
}

api = {
	__interface = core
}

setmetatable(api, {
	__index = function(self, key)
		local interface = self.__interface

		if (interface.platform) then
			return interface.platform[key]
		elseif (interface.autoload) then
			interface:load_platform()
			return interface.platform[key]
		else
			L:error("Platform not initialized!", 1)
		end
	end
})

return api