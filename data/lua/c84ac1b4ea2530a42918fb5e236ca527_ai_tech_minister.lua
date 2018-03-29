-----------------------------------------------------------
-- manage research
-----------------------------------------------------------

require('custom_research')
require('helper_functions')

--Use our wrapper method in order to trap and log our errors
function TechMinister_Tick(minister)
	dtools.setLogContext(minister,"TECH")
	dtools.info('TechMinister_Tick')
	return dtools.wrap(TechMinister_Tick_Impl,minister)
end

function BalanceLeadershipSliders(ai, ministerCountry)
	local PRIO_SETTINGS = {
		[0] = CDistributionSetting._LEADERSHIP_DIPLOMACY_,
			CDistributionSetting._LEADERSHIP_NCO_,
			CDistributionSetting._LEADERSHIP_ESPIONAGE_,
			CDistributionSetting._LEADERSHIP_RESEARCH_
	}

	local sum = 0
	local changes = { [0] = 0, 0, 0, 0 };
	local AvailLS = 1

	local threat = ministerCountry:GetRelation(ministerCountry:GetHighestThreat()):GetThreat():Get()
	-- Officer ratio. aims at 100% - 125% in peace time
	local OfficerGoal = 1 + math.min(threat / 100, 0.25)

	local lawGroup = CLawDataBase.GetLawGroup(GetLawGroupIndexByName('conscription_law'))
	local lawIndex = ministerCountry:GetLaw(lawGroup):GetIndex()
	local targetedLawIndex = GetLawIndexByName('two_year_draft')
	-- As long as we don't have appropriate conscription laws don't aim too high.
	if lawIndex < targetedLawIndex then
		OfficerGoal = math.max(1.0, -0.15 * (targetedLawIndex - lawIndex) + OfficerGoal)
	end
	-- Officer ratio. aims at 200% for maximum org. in war
	if ministerCountry:IsAtWar() then
		OfficerGoal = 2
	-- Officer ratio. aims at 150% in preparation of war
	elseif ministerCountry:GetStrategy():IsPreparingWar() or ministerCountry:IsMobilized() then
		OfficerGoal = 1.5
	end

	local OfficerNeed = math.max((OfficerGoal-ministerCountry:GetOfficerRatio():Get())/OfficerGoal, 0)
	OfficerNeed = math.min(AvailLS, OfficerNeed)
	AvailLS = AvailLS - OfficerNeed
	changes[CDistributionSetting._LEADERSHIP_NCO_] = OfficerNeed

	-- aim for 1/5 IC or at least 20 and use 1/2 of avail LS
	local DiploNeed = math.max(20, ministerCountry:GetTotalIC()/5)
	DiploNeed = 0.5 * AvailLS * math.max(0, (DiploNeed-ministerCountry:GetDiplomaticInfluence():Get())/DiploNeed)
	DiploNeed = math.min(AvailLS, DiploNeed)
	AvailLS = AvailLS - DiploNeed
	changes[CDistributionSetting._LEADERSHIP_DIPLOMACY_] = DiploNeed

	-- always go for 10%, more is useless, unless if GiE
	local domesticSpyPresence = ministerCountry:GetSpyPresence(ministerCountry:GetCountryTag())
	local domesticSpyLevel = tonumber(tostring(domesticSpyPresence:GetLevel()))
	local EspionageNeed = math.min(0.1, AvailLS)
	if domesticSpyLevel < 5 then
		EspionageNeed = math.min(EspionageNeed + (5 - domesticSpyLevel) / 20, AvailLS)
	end
	if ministerCountry:IsGovernmentInExile() then
		EspionageNeed = AvailLS
	end
	EspionageNeed = math.min(AvailLS, EspionageNeed)
	AvailLS = AvailLS - EspionageNeed
	changes[CDistributionSetting._LEADERSHIP_ESPIONAGE_] = EspionageNeed

	-- remainder into research
	local totalLS = ministerCountry:GetTotalLeadership():Get()
	local researchLS = CFixedPoint(totalLS * AvailLS)
	-- LS actually spent to research
	researchLS = researchLS:GetRounded()
	local ResearchNeed = researchLS / totalLS
	ResearchNeed = math.min(AvailLS, ResearchNeed)
	AvailLS = AvailLS - ResearchNeed
	changes[CDistributionSetting._LEADERSHIP_RESEARCH_] = ResearchNeed

	-- Remainder into rest
	local totalNeed = OfficerNeed + DiploNeed + EspionageNeed
	if totalNeed > 0 then
		OfficerNeed = OfficerNeed + AvailLS * (OfficerNeed / totalNeed)
		changes[CDistributionSetting._LEADERSHIP_NCO_] = OfficerNeed

		DiploNeed = DiploNeed + AvailLS * (DiploNeed / totalNeed)
		changes[CDistributionSetting._LEADERSHIP_DIPLOMACY_] = DiploNeed

		EspionageNeed = EspionageNeed + AvailLS * (EspionageNeed / totalNeed)
		changes[CDistributionSetting._LEADERSHIP_ESPIONAGE_] = EspionageNeed
	end

	local command = CChangeLeadershipCommand(ministerCountry:GetCountryTag(), changes[ 0 ], changes[1], changes[2], changes[3])
	ai:Post(command)
end


function TechMinister_Tick_Impl(minister)
	-- Utils.LUA_DEBUGOUT("->TechMinister_Tick " .. tostring(minister:GetCountryTag()))
	local ministerCountry = minister:GetCountry()
	BalanceLeadershipSliders(minister:GetOwnerAI(), ministerCountry)

	local i = ministerCountry:GetAllowedResearchSlots() - ministerCountry:GetNumberOfCurrentResearch()

	if i > 0 then
		local techList = ProposeResearch(minister)
		local ai = minister:GetOwnerAI()

		i = math.min(i, table.getn(techList))
		for j = 1, i do
			local command = CStartResearchCommand(minister:GetCountryTag(), techList[j][2])
			ai:Post(command)
		end
	end
	-- Utils.LUA_DEBUGOUT("<-TechMinister_Tick")
end

function EvaluateCurrentResearch(minister)
	-- sort list of research if priorities changed

end

function ProposeResearch(minister)
	local sortedTechs = {}
	local ministerCountry = minister:GetCountry()
	local ministerTag = minister:GetCountryTag()

	-- Utils.LUA_DEBUGOUT( tostring(ministerTag) )
	-- Construct lists of favourite tech for the country
	local techScoreTable = ConstructTechScoreTable(minister)

	-------------------------------DEBUG------------------------------------
	--Utils.LUA_DEBUGOUT( "--------------LISTE TECH: 1--------------------" )
--	local j = 1
--	while listmaj[j] do
--		Utils.LUA_DEBUGOUT( listmaj[j] )
--		j = j + 1
--	end
--	Utils.LUA_DEBUGOUT( "--------------LISTE TECH: 2--------------------" )
--	j = 1
--	while listimp[j] do
--		Utils.LUA_DEBUGOUT( listimp[j] )
--		j = j + 1
--	end
--	Utils.LUA_DEBUGOUT( "--------------LISTE TECH: 3--------------------" )
--	j = 1
--	while listnorm[j] do
--		Utils.LUA_DEBUGOUT( listnorm[j] )
--		j = j + 1
--	end
--	Utils.LUA_DEBUGOUT( "---------------------------------------" )
	-------------------------------------------------------------------
	local score = 0

	-- Count number of theory techs we're researching
	local researchCountTheory = 0
	for tech in CTechnologyDataBase.GetTechnologies() do
		if 	tech:IsValid() and
			tostring(tech:GetFolder():GetKey()) == 'theory_folder' and
			not minister:CanResearch(tech)
		then
			-- Make sure it's not one of the supply techs
			local isSupplyTech = false
			for bonus in tech:GetResearchBonus() do
				isSupplyTech = true
				break
			end

			if not isSupplyTech then
				researchCountTheory = researchCountTheory + 1
			end
		end
	end

	for tech in CTechnologyDataBase.GetTechnologies() do
		if tech:IsValid() and minister:CanResearch(tech) then
			score = CalculateTechScore(minister, ministerCountry, tech, techScoreTable, researchCountTheory)
			table.insert(sortedTechs, {score, tech})
		end
	end
	table.sort(sortedTechs, function(x, y) return x[1] > y[1] end) -- highest score first

	-------------------------------DEBUG------------------------------------
	-- if ministerCountry:GetMaxIC() > 60 then
	-- if tostring(ministerTag) == 'SCH' then
		-- local techFolder = {}

		-- local techModifiers = minister:GetTechModifers()
		-- local folderModifiers = minister:GetFolderModifers()

		-- Utils.LUA_DEBUGOUT(tostring(ministerTag) .. " tech folder modifiers:")
		-- for tech in CTechnologyDataBase.GetTechnologies() do
			-- if minister:CanResearch(tech) and tech:IsValid() then
				-- local folder = tech:GetFolder()
				-- local folderIndex = folder:GetIndex()
				-- if not techFolder[folderIndex] then
					-- techFolder[folderIndex] = {
						-- name = tostring(folder:GetKey()),
						-- modifier = folderModifiers:GetAt(folderIndex),
						-- folder = tech:GetFolder(),
						-- techs = {}
					-- }
				-- end

				-- table.insert(techFolder[folderIndex].techs, tech)
			-- end
		-- end

		-- local folderSum = 0
		-- for _,folderEntry in pairs(techFolder) do
			-- local s = ""
			-- for i = 0, math.ceil(folderEntry.modifier * 100) do
				-- s = s .. "#"
			-- end

			-- Utils.LUA_DEBUGOUT(s .. " (" .. folderEntry.name .. ")")

			-- local sum = 0
			-- for _,tech in pairs(folderEntry.techs) do
				-- sum = sum + techModifiers:GetAt(tech:GetIndex())
			-- end

			-- folderSum = folderSum + folderEntry.modifier
		-- end
		-- Utils.LUA_DEBUGOUT("Sum folder modifiers: " .. folderSum)
		-- Utils.LUA_DEBUGOUT("\n")

		-- Utils.LUA_DEBUGOUT(tostring(ministerTag) .. ".sortedTechs = {")
		-- local i = 0
		-- for _, debugtech in ipairs(sortedTechs) do
			-- Utils.LUA_DEBUGOUT( "(" .. tostring(debugtech[1]) .. ", (" .. tostring(debugtech[2]:GetKey()) .. ")" )
			-- i = i + 1
			-- if i > 50 then
				-- break
			-- end
		-- end
		-- Utils.LUA_DEBUGOUT("}\n")
	-- end
	-----------------------------------------------------------------

	return sortedTechs
end

function CalculateResearchBonus(country, tech)
	local researchBonus = 0
	for bonus in tech:GetResearchBonus() do
		local ability = 1.0 * country:GetAbility(bonus._pCategory)
		if ability < 5 then
			researchBonus = researchBonus - (5 - ability) * 0.1 * bonus._vWeight:Get()
		elseif ability < 20 then
			researchBonus = researchBonus + math.sqrt(ability - 5) * 0.1 * bonus._vWeight:Get()
		else
			researchBonus = researchBonus + 0.4 * bonus._vWeight:Get()
		end
	end
	return researchBonus
end

function CalculateTechScore(minister, ministerCountry, tech, techScoreTable, researchCountTheory)
	local ministerTag = ministerCountry:GetCountryTag()
	local techName = tostring(tech:GetKey())
	local techStatus = ministerCountry:GetTechnologyStatus()
	local techFolder = tech:GetFolder()
	local techFolderName = tostring(techFolder:GetKey())

	--------------------------------------------------------------
	local score = 1
	if techScoreTable[techName] then
		score = techScoreTable[techName]
	end
	--------------------------------------------------------------
	if techFolderName == "air_doctrine_folder" and not ministerCountry:IsAtWar() then
		--Utils.LUA_DEBUGOUT( "air doctrine in peace time" )
		score = score * 0.75
	elseif techFolderName == 'theory_folder' then
		local techs = GetTechsForTheoryTech(techName)

		if #techs > 0 then
			score = 0

			-- Do not spend more than 5% of our reseach capacity to theory techs.
			-- Do only use theory techs if more than 5 research slots are available.
			local slots = ministerCountry:GetAllowedResearchSlots()
			local ratio = researchCountTheory / slots
			if ratio < 0.05 and slots > 5 then
				local count = 0
				for _,tName in ipairs(techs) do
					if techScoreTable[tName] then
						local t = GetTechByName(tName)
						local techLvl = techStatus:GetLevel(t)
						local nYear = math.max(techStatus:GetYear(t, techLvl + 1) - CCurrentGameState.GetCurrentDate():GetYear(), 0)
						score = score + techScoreTable[tName] - nYear * 1.5

						count = count + 1
					end
				end
				if count > 0 then
					score = score / count

					local categoryName = GetCategoryNameForTheoryTech(techName)
					for cat in CTechnologyDataBase.GetCategories() do
						local catName = tostring(cat:GetKey())

						if categoryName == catName then
							local ability = 1.0 * ministerCountry:GetAbility(cat)
							local bonus = (8 - math.min(ability, 8)) / 8 -- +100%
							local malus = math.min(math.max(ability - 8, 0), 10) / 10 -- -100%
							score = score * (1 + bonus - malus)
							break
						end
					end
				end
			end
		end
	end

	-- Utils.LUA_DEBUGOUT(techName)
	-- Utils.LUA_DEBUGOUT( 'SCORE de base: ' .. score )
	--------------------------------------------------------------
	-- Give Penalty or Bonus for years concern
	local techLvl = techStatus:GetLevel(tech)
	local nYear = techStatus:GetYear(tech, techLvl + 1) - CCurrentGameState.GetCurrentDate():GetYear()
	score = score - math.max(nYear * 1.5, -1) -- a major tech 2 years in the future will get worser than a minor tech.
	-- Utils.LUA_DEBUGOUT( 'SCORE apr?s années: ' .. score )
	--------------------------------------------------------------
	-- Give bonus based on our ability (-50% - +40%)
	local researchBonus = CalculateResearchBonus(ministerCountry, tech)
	--------------------------------------------------------------
	-- If tech enable a new unit or if it's a one lvl tech, give a small bonus to score
	local oneLvlBonus = 0
	if tech:IsOneLevelOnly() or tech:GetEnableUnit() then
		oneLvlBonus = 0.1
	end
	--------------------------------------------------------------
	-- Small random factor (+/-5)
	local randomBonus = (math.mod(CCurrentGameState.GetAIRand(), 11) - 5) /  100
	--------------------------------------------------------------
	score = score * (1 + researchBonus + oneLvlBonus + randomBonus)
	--------------------------------------------------------------
	-- Utils.LUA_DEBUGOUT( 'SCORE apr?s hasard: ' .. score )
	return math.max(0, Utils.CallScoredCountryAI(ministerTag, "CalculateTechScore", score, ministerCountry, tech, listmaj, listimp, listnorm))
end
