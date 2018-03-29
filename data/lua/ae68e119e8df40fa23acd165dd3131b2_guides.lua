local Guides = {}

local function Go(id, date, body )
    local g = {heroid = id, date = date, body = body }
    Guides[id] = g
end

local function tip_dota1(tip)
    if DOTA_VERSION == 1 then
        return '(Dota 1 only): '..tip
    else
        return nil
    end
end

local function tip_dota2(tip)
    if DOTA_VERSION == 2 then
        return '(Dota2 only): '..tip
    else
        return nil
    end
end

local function tip_ministun(skillname)
    return [[$]]..skillname..[[ does a ministun. Use it to cancel chanelling spells and TPs.]]
end

local function tip_ab_disassemble()
    return [[Remember that you can disassemble $arcane_boots to build a $soul_booster for the $bloodstone if you want to.]]
end

local function tip_todo()
    return [[This hero has been recently remade or introduced and we haven't updated his guide yet. Check out for an update soon!]]
end

local function tip_orb_lifesteal(item, skill)

    return string.format(
[[Once you get a $%s the lifesteal will take precedence over $%s, unless you %s. You should prioritize lifesteal most of the time though, since it helps DPSing.]],
    item,
    skill,
    If(DOTA_VERSION==1, 
        [[toggle autocast]],
        [[toggle aotucast or manually cast the orb]]
    )
)

end

local function tip_phase_boots_crit()
    return tip_dota1(
[[Due to engine limitations, it is not possible to bash and get critical hits while under the effects of Phase Boots. If you don't want to worry about this just get Power Treads but if you can keep this limitation in mind or are playing in Dota 2 you might prefer Phase Boots's mobility and damage instead.]])

end

---------------


local I_STARTING  = "Starting"
local I_EARLY     = "Early Game"
local I_CORE      = "Core" 
local I_LUXURY    = "Extension"
local I_OPTIONAL  = "Optional"
local I_FALLBACK  = "Fallback"

---------------

-----------------------
-- Sentinel Strength --
-----------------------

Go('Kunkka', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{
    Q, {MAX, W}, {MAX, Q}, {MAX, E}
}

Guide.ItemBuild{
    {I_STARTING,
        {2, 'gauntlets'}, 'flask', 'tango', {2, 'clarity'}},
    {'Survivability Core',
        'magic_wand', 'vanguard', 'ancient_janggo'},
    {'Damage Core',
        'phase_boots', 'invis_sword', 'greater_crit'},
    {I_LUXURY, 
        'pipe', 'black_king_bar', 'assault', 'heart'}
}

Guide.Tips{

[[$H is an initiator and splash damage dealer. If you can get off the torrent + boat combo, the damage and stun time makes for a great lead for his team. Tidebringer also lets Kunkka be a solid laner, despite being a melee hero.]],

[[If you get a $invis_sword, use it offensively to initiate and to splash the backstab damage using $W.]],

[[Starting from level 2 of $E it is possible to set up a guaranteed $Q. A common alternate Kunkka build is therefore Torrent at level 1 and X at levels 2 and 3. X can also be used to break channeling and TPs.]],

[[The boat works like this: it starts moving from behind you and crashes 1000 units in the direction you cast it, dealing damage and stunning in a 400 AoE. The cursor is used only to select the direction and the distance is fixed.]],

}

end)

---------------

Go('Beast', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, 'flask', 'tango'},
    {I_CORE,
        'bottle', 'magic_wand', 'boots', 'vanguard', 'blink'},
    {I_LUXURY,
        'necronomicon', {ALT, 'arcane_boots', 'travel_boots'}, 'vladmir', 'pipe'},
}

Guide.Tips{

[[$H is a ganker and utility hero. Early on he is a decent laner (with help from his pig and axes) and latter on he is a strong ganker/initiator (with his ultimate) and provides a powerful attack speed aura to his team.]],

[[$H can be a great ward killer. If he gets a Gem he can use the hawk to spot wards and the pig to kill them.]],

}

end)

---------------

Go('Cent', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, E, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING, 
        'stout_shield', 'branches', 'flask', {2, 'tango'} },
    {I_CORE,
        'magic_wand', {ALT, 'hood_of_defiance', 'vanguard'}, 'boots', 'blink'},
    {I_LUXURY, 
        'radiance', 'pipe', 'heart', 'shivas_guard', 'assault'},
}

Guide.Tips{

[[$H is very versatile in that he can be played as an initiator/ganker or in a carry-ish role. In any case, his highlights are his blink+Sstomp combo and his solid early game damage with $W.]],

[[The blink dagger is really essential, as everything you do only works at melee range and you risk being ignored if you can't get into position. Blink also makes it easier for $Q to hit multiple heroes.]],

[[Use $W just after an auto-attack (while your attack is on cooldown) to optimise the damage of the combo.]],

tip_ministun('W'),

}

end)

---------------

Go('ES', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {2, E}, W, {MAX, E}, {MAX, Q} }

--[=[
--For testing items
Guide.ItemBuild{

    {"Protectorate",
        'assault','heart','black_king_bar','aegis',
        'shivas_guard','bloodstone','sphere','vanguard',
        'blade_mail','soul_booster','hood_of_defiance','manta',},
        
    {"Arcane Sanctum",
        'sheepstick','orchid','cyclone','force_staff','dagon',
        'necronomicon','ultimate_scepter','refresher','veil_of_discord',
        'rod_of_atos',},
        
    { "Supportive Vestiments",
        'mekansm','vladmir','arcane_boots','ring_of_aquila',
        'buckler','ring_of_basilius','pipe','urn_of_shadows',
        'headdress','medallion_of_courage','ancient_janggo',
        'tranquil_boots', },
    
    { "Ancient Weaponry",
        'rapier','monkey_king_bar','radiance','butterfly',
        'greater_crit','basher','bfury','abyssal_blade',
        'lesser_crit','armlet','invis_sword','ethereal_blade',},
    
    { "Enchanted Artifacts", 
        'sange_and_yasha','satanic','mjollnir','skadi','sange',
        'helm_of_the_dominator','maelstrom','desolator','yasha',
        'mask_of_madness','diffusal_blade','heavens_halberd',},
    
    { "Gateway Relics",
        'travel_boots','phase_boots','power_treads','soul_ring',
        'hand_of_midas','oblivion_staff','pers','poor_mans_shield',
        'bracer','wraith_band','null_talisman','magic_wand', },
    
    { "Cache of Quel-Thelan", 
        'gloves','lifesteal','ring_of_regen','blink','sobi_mask',
        'boots','gem','cloak','magic_stick','talisman_of_evasion',
        'ghost', },

    { "Ancient of Wonders",
        'clarity','flask','tango','bottle','ward_observer','ward_sentry',
        'dust','courier','tpscroll','smoke_of_deceit','flying_courier', },
    
    { "Sena the Accessorizer",
        'gauntlets','slippers','mantle','branches','belt_of_strength',
        'boots_of_elves','robe','circlet','ogre_axe','blade_of_alacrity',
        'staff_of_wizardry','ultimate_orb', },
    
    { "Weapon Dealer",
        'blades_of_attack','broadsword','quarterstaff','claymore','ring_of_protection',
        'stout_shield','javelin','mithril_hammer','chainmail','helm_of_iron_will',
        'platemail','quelling_blade', },

    { "Secret Shop",
        'demon_edge','eagle','reaver','relic','hyperstone','ring_of_health',
        'void_stone','mystic_staff','energy_booster','point_booster','vitality_booster',
        'orb_of_venom',},
}
--]=]

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, {2, 'tango'}, {2, 'clarity'} },
    {I_CORE,
        'magic_wand', 'arcane_boots', 'blink' },
    {I_LUXURY,
        'ultimate_scepter', 'shivas_guard', 'sheepstick', 'heart' },
    {I_FALLBACK,
        'ward_observer', 'bottle', 'bracer', 'vitality_booster' },
}

Guide.Tips{

[[$H is a support hero with a very long range stun and great initiation ability once he farms a Blink Dagger.]],

[[Early game, $H can either support or roam, using $Q's terrain blocking to secure kills or save allies. Either way, stay hidden in the trees to get the best fissure angles (and to also leech XP).]],

[[Later in the game, try to farm a dagger. A timely $R deals devastating damage against clumped enemies and you can keep them stunned for a long time by chaining your skills correctly.]],

[[Unit-targeting $Q guarantees a sure hit. Ground targeting it is harder but allows for better placement and blocking.]],

[[$W is only really good for triggering $E - don't be too exited about the damage part of it.]],

}

end)

---------------

Go('Omni', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, E, W, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango', {2, 'clarity'} },
    {I_CORE, 
        'arcane_boots', 'soul_ring', 'mekansm' },
    {I_LUXURY,
        'orb_of_venom', 'vanguard', 'pipe', 'shivas_guard', 'sheepstick' },
}

Guide.Tips{

[[$H is a tanky support hero and chaser. His signature skills are his $Q, a strong heal/nuke and $W, a free BKB for your carry. $E also gives him an useful role in ganks and his ultimate makes his entire team safe from physical damage for a while.]],

[[$R should be used in the heat of a battle (instead of as an initiation or escape skill). It prevents physical damage but not magic damage.]],

}

end)

---------------

Go('Brew', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {2, S}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, {2, 'tango'}, 'flask' },
    {I_CORE,
        'magic_wand', 'phase_boots', 'ultimate_scepter', 'blink' },
    {I_LUXURY,
        'necronomicon', 'shivas_guard', 'vladmir',
        'ancient_janggo', 'assault', 'pipe' },
}

Guide.Tips{

[[$H is an initiator and anti-carry. His playstyle revolves around using his strong $Q to gank and initiate (with Dagger), $W for reducing the effectiveness of an enemy right-click carry (miss chance) and his ultimate for wreaking havoc without fear of dying.]],

[[You need to micro the primal split pandas to take the most out of them. Send the Fire Panda to attack someone squishy (it has good attack damage). Use the Earth Panda to stun someone and use the Storm Panda to cyclone (euls) someone else.]],

[[After the initial initiation items, Panda should focus on getting aura items, since those still work even during $R. Regeneration items, like $heart, also work when hidden.]],

[[The early levels in stats are to help with the manapool and to let you cast $Q more then once. It might be a good idea to get a single early level of $E though, for the evasion.]],

[[At the end of $R, $H will reaper at Earth's place. If Earth is dead he'll take Storm's place, and if Storm is also dead he'll take Fire's place.]],

}

end)

---------------

Go('Sven', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, S, Q, S, Q, R, Q, E, S, S, R, W, W, W, W, E, E, E }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango', {2, 'clarity'} },
    {I_CORE,
        'bottle', 'magic_wand', 'power_treads', 'blink' },
    {I_LUXURY,
        'urn_of_shadows', 'ancient_janggo', 'mask_of_madness', 'black_king_bar'},
}

Guide.Tips{

[[$H is a very versatile hero, being viable as both a support/roamer hero or as a semi-carry. Either way, his strong AoE stun and $R both give him great early game potential and relative independence compared to his peers.]],

[[The reason for all those levels in stats is that Sven has terrible mana issuess early on, so he really needs the extra mana pool. Feel free to get a single early level of $E  if you know what you are doing though.]],

[[Sven's reliable AoE stun is a great combo with delayed effect spells. Lina and Leshrac used to be a very popular lane combinations.]],

} 

end)

---------------

Go('Tiny', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, R}, {1, Q}, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING, {4, 'branches'}, {2, 'flask'} },
    {I_CORE, 'bottle', 'magic_wand', 'arcane_boots', 'blink' },
    {'Caster Extension',
        'force_staff', 'cyclone', 'ethereal_blade', 'shivas_guard', 'sheepstick' },
    {'Siege/DPS Extension',
        'ultimate_scepter', 'assault' },
}

Guide.Tips({

[[Tiny's Avalanche + Toss combination is one of the most damaging nuke combos in the game. Later on, he can get a Blink Dagger to transition into an initiator or an $ultimate_scepter.name to become a very fast building destroyer and pusher.]],

[[The AvaToss combo works by casting Avalanche and Toss in quick succession (using your instantaneous casting animation). If you get it right, the target will be hit by avalanche, get tossed up and finally land before the avalanche finishes, being dealt damage by the avalanche a second time!]],
    
[[Toss casting mechanics:]]..Markup.ulist({
  [[The cursor chooses where to throw the tossed unit to.]],
  [[You must target another unit (you cannot target ground).]],
  [[The the circle indicates the damage AoE.]],
  [[The tossed unit is chosen at random among those standing next to Tiny. For the avatoss combo, stand right next to your target so you can throw it at itself.]],
}),

[[Don't forget that Toss can also be used to throw a friendly hero at the enemy team (Earthshaker/Tidehunter/Techies) or to throw an escaping enemy back into your team.]],

[[Consider skipping Craggy Exterior for stats if there aren't many physical DPS or melee heroes on the other team. As for items, some people like getting Phase Boots for better positioning and mobility.]]

})

end)

---------------

Go('TC', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'quelling_blade', {3, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'boots', 'magic_wand', 'bottle', 'urn_of_shadows',
        'power_treads', {OPT, 'vanguard'} },
    {I_LUXURY,
        'assault', 'satanic', 'black_king_bar',
        'shivas_guard', 'refresher' },
}

Guide.Tips{

[[$H is part initiator, part damage buffer, part dpser. $W + $Q can allow a lot of set-up time (useful for his ult or allied spells). His aura, while melee ranged, is devastating and his damage buff from ancestral spirit can make him a temporary dpser.]],

[[Remember that any hero damage will wake-up a stomped enemy. Coordinate well and be patient.]],

}

end)

---------------

Go('Treant', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {2, E}, {MAX, W}, Q, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango', {2, 'clarity'} },
    {I_CORE,
        'soul_ring', 'arcane_boots' },
    {I_LUXURY,
        'necronomicon', 'shivas_guard', 'refresher', 'vladmir' }
}

Guide.Tips{

[[$H is a support hero with good defensive laning skills. His Leech Seed is great for diving and counter-diving and $Q can save allies from ganks. Finally, his aura helps allies all over the map and $R is a decent teamfight spell.]],

[[The idea behind this skill build is to get at least two levels in $E (during the day, when its more effective) and then either proceed to max $W, if you need to take part or defend against ganks, or $E if everyone is playing passive. You can get also get an early level of $Q somewhere along the way to protect against ganks, although you probably won't have the mana by then to liberaly use it.]],

[[For the item build, $H is very item independent, as all he needs is some some minor mana regen. After that, feel free to get whatever supportive items you feel will be good for your team.]],

[[$Q is not only useful to scape. You can also use it as a Smoke to gank people and you can also use it to ambush pushers. Also remember that $H can cast spells without breaking his invis.]],

[[$R will entangle opponents that used BKB before you cast Overgrowth, but they can also use the BKB to get out of the entangle if they use it after you cast your ultimate.]],

}

end)

---------------

Go('Wisp', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {4, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'ring_of_basilius', 'boots', 'magic_wand', 'mekansm' },
    {I_LUXURY,
        {2, 'bracer'}, 'vanguard', 'ancient_janggo', 'urn_of_shadows'}
}

Guide.Tips{

[[$H is a very different kind of support hero. While he doesn't have any direct disable and is very fragile, he can turn any allied hero into a powerful global ganking machine.]],

[[Using $Q in the fountain area gives double regen to your ally. This is nice if you use $R for a quick fountain trip or if you happen to have a leaver Wisp on your team.]]

}

end)

---------------

Go('Alch', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        {ALT, 'vanguard', 'hood_of_defiance'},
        'radiance', 'power_treads' }, 
    {I_LUXURY,
        'assault', 'heart', 'manta', 'shivas_guard', 'mjollnir'}
}

Guide.Tips{

[[Alchemist is a versatile hero, but the most popular way to play him is as a tanky Radiance carry. An Alchemist with high levels of $R is nigh unkillable - add some DPS items and start having fun.]],

[[Feel free to max $E first if you are able to safely freefarm without needing the $Q to counter-push. ]],

[[$R increases your base health and mana regeneration considerably. Since it has a low cooldown, feel to cast it liberally while farming.]],

[[If you have a percentage-based mana regen item (such as a $sobi_mask), dropping it and picking it up again while in $R form will make it provide extra mana even after the rage duration ends. Free mana!]],

[[An alternative play-style for Alchemist is to use him as a support. In this case, max your long-duration stun early and get cheap items such as Soul Ring to allow you to gank around.]],

}

end)

---------------

Go('Clock', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, E}, {2, Q}, W, {2, Q}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'boots', 'bottle', 'vanguard' },
    {'Defensive Items',
        'blade_mail', 'hood_of_defiance'},
    {'Agressive Items',
        'ultimate_scepter', 'shivas_guard', 'necronomicon' }
}

Guide.Tips{

[[$H is an initiator. He can use his $R to jump in and his $Q and $W to lock down a priority enemy.]],

[[An alternate skill build is maxing $Q an getting an early level of Cogs. This increases Clock's ganking power at the expense of his global range nuke and farming skill.]],

[[$H's main needs from items are survivability and some mana, since he has below average stats.]],

[[$E is extremely versatile. You can use it to:]]..
Markup.ulist({
  [[Scout: Enemy lanes (just after spawning), runes, Roshan, junglers...]],
  [[Last hit against a tough lane.]],
  [[Push or counterpush a lane across the map.]],
  [[Aid in far away ganks.]]
}),

[[$Q's ministuns can effectively silence heroes with long cast animations, such as ]]..Heroes.SF.nick..[[ or ]]..Heroes.ES.nick..[[. You can be extra aggressive in these cases.]],

}

end)

---------------

Go('DK', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {1, R}, {1, W}, {MAX, Q}, {MAX, E}, {MAX, W}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'gauntlets'}, 'flask', {2, 'tango'} },
    {I_CORE,
        {OPT, 'quelling_blade', 'urn_of_shadows', 'soul_ring'},
        'power_treads', 'black_king_bar', 'helm_of_the_dominator' },
    {I_LUXURY,
        'invis_sword', 'hyperstone',
        'greater_crit', 'assault', 'satanic', 'mjollnir' },
}

Guide.Tips{

[[$H is a strength carry that is greatly defined by his ultimate. The level 1 form (green dragon) is a strong tower razer and the level 3 form (blue dragon) deals devastating, slowing, damage in an AoE.]],

[[You can use dominated creeps to stack ancient creep camps (pull at the 52 second mark). DK can quickly little trouble using his AoE to clear a large creep stack and will gain lots of gold and exp by doing so.]],

[[Some people like not leveling up a skill at level 15 so they can completly skip the second dragon for (goind straight to from level 1 to level 3 $R when they reach 16). Additionally, some people like skipping the later levels of $W for stats, since the stun duration increases very littl with levels.]],

}

end)

---------------

Go('Huskar', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, E}, {MAX, Q}, {MAX, S} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'gauntlets'}, {2, 'branches'}, 'flask', 'tango'},
    {I_CORE,
        'power_treads', 'helm_of_the_dominator'},
    {'DPS Items',
        'black_king_bar', 'greater_crit', 'armlet' },
    {'Survivability Items',
        'magic_wand', 'cloak', 'urn_of_shadows', 'heavens_halberd'},
}

Guide.Tips{

[[$H is a rambo initiator and anti-tank DPS hero. He is also a great agressive laner, since he can use $W to orbwalk and harass. In a teamfight $H will deal massive damage if left unchecked at low life, but he is very vulnerable to burst nukes and disables.]],

[[The idea behind the $E + $Q build is to get the maximum fighting potential when you get your $helm_of_the_dominator: the lifeleech and heal lets you stay alive at low hitpoints while dealing lots of damage with $E.]],

[[In the build the single level in $W is basicaly useful for orbwalking and for lowering your HP when jungling. We don't max it since it doesn't stack with the $helm_of_the_dominator.]],

[[Some alternative skill builds that some people are fond of are $W + $E and $E + Stats. Both of them have more damage potential, but come at a cost in survivability.]],

tip_orb_lifesteal('helm_of_the_dominator', 'W'),

[[You can reduce the self-damage from your ultimate by getting magic resistance items or a BKB.]],

}

end)

---------------

Go('BB', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'sobi_mask', 'boots',
        'power_treads', 'vanguard', 'hood_of_defiance' },
    {I_LUXURY,
        'radiance', 'blade_mail', 'assault', 'pipe' }
}

Guide.Tips{

[[$H is a unique kind of carry hero, depending on long drawn out fights to let quill and radiance damage rack up. As such, healers and Dazzle (for his ult) go very well with him. He can also be a very useful chaser with his snot.]],

[[Don't worry too much about your mana - $H has a very good INT gain so you only really get mana problems early game.]],

[[$W is great for ganking and for getting hero kills, but consider getting levels in $H if you and your team are more concerned with pushing towers instead.]],

}

end)



---------------

Go('Phoenix', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {3, 'branches'}, 'flask', 'tango'},
    {I_CORE,
        'ring_of_basilius', 'magic_wand', 'tranquil_boots', 'chainmail',
        'urn_of_shadows', 'soul_ring' },
    {I_LUXURY,
        'medallion_of_courage', 'mekansm', 'hood_of_defiance',
        'shivas_guard', 'ancient_janggo'},
}

Guide.Tips{

[[$H is a slippery initiator and support hero. He can jump in from a long range, his spirits are a powerful and spammable heal/nuke and his ultimate can be very strong if used correctly.]],

[[This item build emphasizes survivability and utility. $H has very poor natural armour so getting armour items is specially important.]],

[[When using $W, always try to fully charge them and then release them at an enemy (you will still get heal in an AoE around you even if you realease them). Some people like getting Soul Ring for maximum spammability.]],

[[$R is a great teamfight skill, specially if comboed with $Q's disarm.]],

}

end)

---------------

Go('Legion', {day=30, month=7, year=2012}, function()

Guide.Tips{

tip_todo(),

}

end)

---------------

Go('Shredder', {day=30, month=7, year=2012}, function()

Guide.Tips{

tip_todo(),

}

end)

----------------------
-- Scourge Strength --
----------------------

Go('Axe', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, W}, Q, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'vanguard', 'arcane_boots', 'blink', 'hood_of_defiance'},
    {I_LUXURY,
        'pipe', 'shivas_guard', 'assault', 'travel_boots' }
}

Guide.Tips{

[[Axe is an initiator and DotA's only true tank (since he can force enemies to attack him). He is also a bestly caster early game, with $W being able to deal very high damage and $E making it very dangerous for any enemies that get too close him.]],

[[Issuing an attack order on an enemy hero draws creep aggro (this useful for $E). If an enemy tries to spam Stop or run away during Beserker's Call it will only make helix proc more often.]],

[[The way that $E works means that Axe jungles faster and safer if he fights multiple creeps at once. Try to stack the creep camps (by pulling them at around the 51 second mark).]],

[[You might find some older guides that advocate skipping $W entirely (getting it at levels 22-25). This is because back it used to be one of the worst skills in the game but it has since been greatly buffed since then.]]

}

end)

---------------

Go('CK', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, R}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'gauntlets', {3, 'branches'}, 'flask', {2, 'clarity'}},
    {I_CORE,
        'magic_wand', 'bottle', 'power_treads'},
    {I_LUXURY,
        'armlet', 'ancient_janggo', 'black_king_bar', 'heart'},
}

Guide.Tips{

[[$H is a ganker, chaser and a strong teamfight brawler. He is bulky and fast and his low-cooldown skills let him shut down single targets very effectively.]],

[[Be aware that $H has big mana problems - conserve your skills and remember to get some items to help in this sense.]],

[[When comboing, use $W before bolt. It has a bigger range and it also lets you hit the enemy more times.]],

[[Your ultimate has a relatively long cooldown, but the images are very bulky. Its best to think of it as more of a damage boost, a-la Sven, and less of an usual image skill a-la Naga or Phantom Lancer.]]

}

end)

---------------

Go('Doom', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, W, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'phase_boots', 'vanguard',
        {OPT, 'bottle', 'radiance'}
    },
    {I_LUXURY,
        'pipe', 'assault', 'shivas_guard', 'heart', 'sheepstick'}
}

Guide.Tips{

[[$Q is a mix of caster and carry. He can use his ultimate to basically nuliffy and enemy for a whole teamfight and his $E is the strongest single-target nuke in the game if you hit the level damage bonus. Finally, the bonus gold from $Q allows $H to farm items in order to shore up his weaknesses or to help him carry a bit.]],

[[The best neutral creeps to get with $Q tend to be the ones with strong passives. In particular, the Kobold speed aura, the Alpha Wolf crit and damage aura and the Centaur AS aura are some of the strongest choices. The Ogre Magi is also nice early game to increse you very weak armour stat.]],

[[$R's silence is specially devastating against enemy casters and heroes that relly on passives or items (lifesteal, evasion, etc).]],

[[Sometimes it is best to save skill points for a bit in order to give Lvl? Death the best multipliers.]],

[[Doom has a very low armour stat. Keep that in mind when laning and when making item choices.]]

}

end)

---------------

Go('Naix', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        {OPT, 'quelling_blade', 'hand_of_midas'},
        'magic_wand', 'power_treads', 'armlet' },
    {I_LUXURY,
        'sange', 'basher', 'hyperstone',
        'assault', 'mjollnir', 'abyssal_blade' },
}

Guide.Tips{

[[$H is a carry. $Q's magic immunity gives him a window of time to deal damage unimpeded and Feast adds significant damage to his attacks (specially against low armour and high HP heroes).]],

[[The item build prioritizes cheap damage/str and negative armour items (to sinergize with $W's physical damage). Blink Dagger is a nice way to negate your melee range and can allow you to blink in and take down a squishier hero before the Rage duration ends.]],

[[Get Rage at level 1 to protect against disables if you are against a hard lane.]],

[[While Lifestealer is a great jungler (check out the ]]..
Markup.link(
    "http://www.playdota.com/guides/choke-point-jungling",
    [[guide on choke-point jungling]] )..
[[), but at early levels he might be able to farm faster in lane (and aid in ganks with his slow).]],

[[$R can be used on enemy creeps, to heal yourself, or on allied heroes, for positioning. Heroes with good mobility, like Storm Spirit or ]]..Heroes.Prophet.nick..[[ are great for this. (Only infest allied creeps if you really need to - you don't get healed when you come out and the enemies can kill it if they see you getting in).]]

}

end)

---------------

Go('LoA', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {2, W}, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {2, 'branches'}, 'flask', {2, 'tango'} },
    {I_CORE,
        'ring_of_basilius', 'magic_wand', 'arcane_boots', 'mekansm' },
    {I_LUXURY,
        'vladmir', 'ancient_janggo', 'necronomicon', 'sheepstick' },
}

Guide.Tips{

[[$H straddles a fine line between straight support and a dps-er (but when in doubt err on the side of support). While he lacks straight disables or range, he compensates by having multiple spells that protect his teamates in battle.]],

[[$W removes debuffs and stuns!]],

[[$R can be manually casted. Use this when you are about to be focused with lots of damage instead of letting it trigger on its own.]],

[[$Q will not deal self damage if you are under $W (and will actually heal you when $R is active.]]

}

end)

---------------

Go('Lycan', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {3, 'branches'}, 'flask', 'tango'},
    {I_CORE,
        'ring_of_basilius', 'medallion_of_courage', 
        'vladmir', 'power_treads' },
    {I_LUXURY,
        'black_king_bar', 'basher', 'necronomicon',
        'heart', 'assault' }
}

Guide.Tips{

[[$H is a strong jungler who can come out of the forest with solid midgame carrying potential. He excells at ganking lone heroes and taking down undefended towers with his summons.]],

[[Jungling tips: Spawn wolves at the fountain (to get your mana back). Kill easy and pullable camps first until you reach level 3. Micro the damaged wolves back before they die, for maximal DPS.]],

[[It is possible to solo Roshan with wolves, maxed $E and $vladmir.
Howl is global and very cheap so don't forget to use it to help ganks on the other side of the map.]],

}

end)

---------------

Go( 'NS', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, R}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'gauntlets'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'power_treads', 'urn_of_shadows', 'point_booster'},
    {I_LUXURY,
        'ultimate_scepter', 'basher', 'black_king_bar', 'vladmir', 'assault'},
    {I_OPTIONAL,
        'bottle', 'orb_of_venom'},
}

Guide.Tips{

[[$H is a natural ganker, and can rack up lots of kills and map control after night falls. Play aggressively and try to get as many kills as possible in your first night to get the ball rolling.]],

[[Since all your skills other then $Q are only really useful at night, consider leaving their skill points unspent as you level up in the first day. This will let you choose the best skill build when the night finally comes.]],

[[Get an early level of $W if you need the silence, either for defense or to gank someone with a big disable or escape mechanism.]],

[[$R stops the day-night clock during its duration. Cast it as often as you can during the night time, since it effectively will make the whole night last longer.]],

tip_ministun( 'Q' ),
}

end)

---------------

Go('Pit', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, E}, {MAX, W}, R, {MAX, Q}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', 'branches', 'flask', 'tango' },
    {I_CORE,
        'soul_ring', 'magic_wand', 'arcane_boots', 'vanguard' },
    {I_LUXURY,
        'necronomicon', 'ancient_janggo', {OPT, 'mekansm', 'pipe'},
        'travel_boots', 'shivas_guard' }
}

Guide.Tips{

[[$H is a pusher and counter-pusher. All his spells are all AoE (perfect against enemies trying to push against a chokepoint or ramp), and his $E becomes very deadly after some creep corpses have piled up. $R is best used for safely retreating after a team push.]],

[[For items, the main needs for $H are some mana regen to spam spells (but not too much, due to his good int gain) and tankyness to stay in the middle of a fight.]],

[[Avoid overfarming with your skills: $H works fine with few items and your teamates might put the gold to better use.]],

[[Note that $W spawns some corpses that you can blow up with Expulsion.]],

}

end)

---------------

Go('Pudge', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, {2, 'tango'}, {2, 'clarity'} },
    {I_CORE,
        'bottle', 'magic_wand', 'urn_of_shadows',
        {OPT, 'vanguard'}, 'hood_of_defiance', 'ultimate_scepter' },
    {I_LUXURY,
        'pipe', 'force_staff', 'travel_boots', 'ancient_janggo',
        'blade_mail', 'shivas_guard', 'heart' }
}

Guide.Tips{

[[$H is a ganker. He tries to fish off enemy heroes with his $Q (after which he can finish them off with his combo) and later in the game the extra strength from $E allows him to mantain a presence through sheer tankyness.]],

[[$R's range is longer than $W's - make sure $W is hitting the enemy before you use $R.]],

[[$W's self-damage is magic, so it can be reduced with hood or pipe. You can also use Rot to kill yourself, denying gold and experience to the enemy (but you will still lose the usual gold on death).]],

[[You can toggle $W while channelling $R without interrupting it. You can also cast $W while the meat hook is returning to you (but not when it being sent forward).]],

}

end)

---------------

Go('Leoric', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {2, S}, {1, W}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'gauntlets', {3, 'branches'}, {2, 'tango'} },
    {I_CORE,
        {OPT, 'quelling_blade'}, 'magic_wand', 'armlet', 'power_treads' },
    {I_LUXURY,
        'force_staff', 'desolator', 'black_king_bar', 'assault', 'heavens_halberd'}
}

Guide.Tips{

[[$H is a DPS carry. Move around the battlefield, casting stuns and stabbing enemies in their face without fear.]],

[[$H takes some skill to play well. Managing and conserving your mana is of utmost importance, you need early farm and items and the melee range demands perfect positioning at all times.]],

[[$magic_wand and $soul_ring are nice ways to guarantee mana for Reincarnation (especially if against mana burners).]],

string.format(
    [[Feeling too noob to use an Armlet? %s.]],
    Markup.link(
        "http://www.playdota.com/guides/epic-return",
        "WHAT THE FUCK, YOU HAVE ONE ACTIVE SKILL YOU DUMB SHIT"
    )
)

}

end)

---------------

Go('Slardar', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE, 
        'power_treads', 'magic_wand', 'vanguard',
        {OPT, 'urn_of_shadows'}, 'blink' },
    {I_LUXURY,
        'black_king_bar', 'assault' }
}

Guide.Tips{

[[$H is a strength semi carry, with good chasing and early damage potential. He is also a good initiator, specially after he gets items, blink and BKB.]],

[[The item build prioritizes survivability first. $H is squishier then he appears, due to $Q and needing to get in melee range of his enemies.]],

[[$R will give vision over its target and will also reveal its target if it goes invisible.]]

}

end)

---------------

Go('Dirge', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, Q, {MAX, E}, W, {MAX, Q}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'ring_of_basilius', 'vanguard', 'arcane_boots' },
    {I_LUXURY,
        'pipe', 'bloodstone', 'ultimate_scepter',
        'shivas_guard', 'blade_mail' },
}

Guide.Tips{

[[$H causes mayhem in team fights. Most of his skills do (or amplify) AoE damage, he has 3 skills that help him stay alive and $W can be one of the strongest nukes or heals in the games if you are in a crowded location.]],

[[The item build tries to prioritize tankyness and mana, to allow you to spam your spells and stay close in order to maximize the $R damage amp.]],

[[The rationale for the skill build is that low levels of $Q and $W have lower mana costs, and that there is no point in increasing the soul rip cap during the laning phase, when there are few creeps around. Finally, tombstone zombies are very strong early game. Midgame, max $Q if you have the mana to support it or max $W otherwise.]],

[[$W can heal your Tombstone. This can be useful, but be careful do not do it on accident.]],

tip_ab_disassemble(),

}

end)

---------------

Go('Tide', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, R}, {MAX,Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango', {2, 'clarity'}},
    {I_CORE,
        'magic_wand', 'arcane_boots', {OPT, 'urn_of_shadows'} },
    {I_LUXURY,
        'pipe', 'ward_observer', 'shivas_guard', 'refresher', 'blink' },
}

Guide.Tips{

[[$H is defined by his extremely powerful ultimate: He can go for a more supporting build, getting few items but still being able to help in team fights with $R or he can go for a more tanky build, farming some survivability items and leading the charge during a push, daring the enemy team to come into range of his $R.]],

[[$E shell removing debuffs and stuns makes it very hard for the enemy team to prevent you from casting Ravage.]],

tip_dota1([[Damage block items ($stout_shield, $vanguard, $poor_mans_shield) don't stack with $E (so don't buy them in that case).]]),

[[$R has a huge AoE so tide does not need a Blink Dagger as much as other initiators. That said, it is never really a bad item to get.]]

}

end)

---------------

Go('Magnus', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, 'flask', {2, 'tango'} },
    {I_CORE,
        'bottle', 'magic_wand', 'power_treads', 'blink', 'bfury' },
    {I_LUXURY,
        'mask_of_madness', 'black_king_bar', 'vladmir', 'greater_crit' },
}

Guide.Tips{

[[$H is an initiator and DPSer. Early on, he can get good lane control with judicious use of $Q and bottle (be sure to get those runes!). Later on, he is all about blink-ulting and then taking powerful, empowered, swings that will cleave onto many helplessly stunned heroes.]],

[[If you go treads on $Q, remember to abuse stat-switching to maximize your mana (switch to int before casting, switch to agi when using bottle charges). Arcane and Phase are also fine choices though.]],

[[Unit-targeting $Q makes it easier to hit, while ground-targeting allows you to better choose the AoE and hit enemies that are further away.]],

}

end)

---------------

Go('SB', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, E, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'magic_stick', {3, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'magic_wand', 'power_treads', 'mask_of_madness' },
    {I_LUXURY,
        'black_king_bar', 'ancient_janggo', 'heavens_halberd',
        'greater_crit', 'assault', 'mjollnir' }
}

Guide.Tips{

[[$H is, at heart, a 1v1 hero (so he only really works well on pubs, to be honest). He can gank fantastically (if your opponents don't check their status bar for $Q) and has the only bash in DotA that has the potential to be a literal perma-bash.]],

[[The $mask_of_madness build has good killing potential early, but is less effective latter in the game and demands a BKB and/or survivability items. Go for $mjollnir or $assault if you want a more survivable build.]],

[[$Q gives vision, even on invisible heroes. You can use it to help ganks even if you don't really plan to go there.]],

[[Bash has the same percentage chance at every level, which is why one level is fine early on.]],

}

end)

---------------

Go('SK', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', 'branches', 'flask', {2, 'tango'}, {2, 'clarity'} },
    {I_CORE,
        'magic_wand', 'bottle', 'arcane_boots', 'blink' },
    {I_LUXURY,
        'ultimate_scepter', 'veil_of_discord', 'shivas_guard',
        'sheepstick', 'black_king_bar' }
}

Guide.Tips{

[[$H has decent ganking abilities, an amazing ult (especially coupled with a blink dagger), great disabling and mobility ($Q + dagger) and even good pushing power with $E.]],

[[If you are laning against a melee hero (like ]]..Heroes.Brood.nick..[[), an early level of $E can be a great harassing tool.]],

[[After you get a Blink Dagger, it is possible to channel the $R from a safe distance (remember to shift-queue the blink so you don't miss any pulses). Don't be too picky though - its fine if you only hit 2 or 3 heroes.]],

[[Similarly to Puck, Sand King gains much of his effectiveness though mobility and being hard to kill. $Q, $E and Dagger can all be used to dodge projectiles (such as Magic Missile) and you can use Sand Storm as a 3 second Wind Walk, due to the fade time.]],

}

end)

----------------------
-- Sentinel Agility --
----------------------

Go('AM', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, Q, E, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'ring_of_health', 'power_treads',
        {ALT, 'vanguard', 'bfury'},
        'manta' },
    {I_LUXURY,
        'heart', 'butterfly', 'basher', 'black_king_bar', 'vladmir' },
}

Guide.Tips{

[[$H is a carry hero that also has some early damage potential in the form of $Q. However, his greatest asset is definitely the incredible versatility of his $W: jump in for a gank, jump out from a gank, quickly farm jungle camps...]],

[[The skill build is fairly flexible. A good rule of thumb is to get at least one level of each skill by level 4. From there, max $Q and then $W if you want to be aggressive or max the shield then the blink if you want to be passive and farm. You can also get some levels of stats if you are just passively farming for the late game.]],

[[For the item build, the early $vanguard is for when you want need the HP boost early game, either to be more aggressive or just to survive against an aggressive enemy team. $bfury is a more late game oriented build: it leaves you very squishy but lets you farm lanes and neutrals really fast (blink around the jungle!).]],

tip_ministun('R'),

[[Don't be tricked by the "Anti Mage" title: $Q is actually better against low-mana STR and AGI heroes and the spell shield is only really useful if you get some HP items. The ultimate is genuinely good against spellcasters though.]],

}

end)

---------------

Go('Sniper', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, E, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'slippers'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'ring_of_aquila', 'wraith_band', 'power_treads' },
    {I_LUXURY,
        'manta', 'butterfly', 'monkey_king_bar',
        'greater_crit', 'skadi' },
}

Guide.Tips{

[[$H is a stereotypical ranged AGI carry. He deals outstanding DPS late game if he farms some items, but he is weak early game and does not have any escape or survivability skills. That said, he is a good turtler: his spammable AoE helps defend chokepoints and his range lets him stay far back in a safer position.]],

[[$Q deals residual damage to towers, so take advantage of that when pushing or harassing.]],

tip_dota1([[$W and MKB's True Strike don't stack. You can toggle MKB on and off to choose the effect you prefer.]]),

[[As is usual with many other carry heroes, a $invis_sword can be acquired as an escape mechanism in pubs (but don't expect that to work if your opponents know what they are doing).]],

[[With higher levels of $E, $H is able to out-range towers. During the night he is limited by his sight range though, so you might need a spotter.]],

}

end)

---------------

Go('Jugger', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {2, S}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'boots', 'flask'},
    {I_CORE,
        'magic_wand', 'poor_mans_shield', 'phase_boots', 'bfury'},
    {I_LUXURY,
        {ALT, 'manta', 'sange_and_yasha'}, 'desolator', 'assault', 'butterfly' },
}

Guide.Tips{

[[The boots first build assumes you and your allies are aggressively going for a very quick First Blood with $Q. Get normal items otherwise.]],

[[$W is left for last in this build since Juggernaut has a very constrained mana pool. However, feel free to get it earlier if you really need the heal or if your team is pushing many towers. Healing ward is a very good pushing skill!]],

[[If possible, wait until creeps are cleared before omnislashing. If you get lifesteal, remember that you're invulnerable and gaining life during your ultimate. This is pretty abusable.]],

[[You can damage buildings with your attack during spin. You can also TP during spin to escape in many otherwise hopeless situations.]],

[[The reason for the early levels of stats is to give you enough mana pool to cast both $Q and $R.]],

tip_ministun('R'),

}

end)

---------------

Go('LD', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, 'flask', 'tango', 'stout_shield' },
    {I_CORE,
        'tranquil_boots', 'boots', 'orb_of_venom', 'radiance'},
    {I_LUXURY,
        'assault', 'pipe', 'mjollnir' },
}

Guide.Tips{

[[$H is a carry. Unusualy, however, he has solid pushing abilities in mid-game due to his bear's siege damage and great tanking ability.]],

[[The usual item build is going for a Radiance rush on the Spirit Bear, to compliment its tankiness and pushing power. From that point you continue to get even more survivability and aura items. The bear also benefits a lot from atack speed, in order to take down towers and proc Entangle more often.]],

[[During the early game, use the bear to harass and get lane control while the ranged Sylla gets last hits. Later in the game you want to spend the majority of your time in Bear form, since it adds so much HP and damage. The same idea applies for items: early on you keep most stuff in the bear and latter on you start putting things on the druid.]],

[[If you don't feel comfortable microing a lane Sylla you can try jungling instead. It farms slower but is easier to pull off.]],

[[If you buy a Quelling Blade for the bear, remember to sell it after level 3 of Spirit Bear, since it otherwise conflicts with the entangle.]],

[[Stats from items don't benefit the bear, so get damage, attack speed and armour items for it instead.]]

}

end)

---------------

Go('Luna', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'ring_of_aquila', 'power_treads',
        'helm_of_the_dominator', 'yasha' },
    {I_LUXURY,
        'black_king_bar', 'manta', 'butterfly',
        'satanic', 'assault' },
}

Guide.Tips{

[[$H is a carry hero with solid nuking potential early game and good AoE damage potential late game. However, she has to deal with her low survivability and attack range. Maybe she has a place in teams that can take advantage of her early-game damage aura?]],

[[The skill build suggested here is pretty straightforward: max $Q, since it is a spammable nuke and is required by $R. $E goes next, since it is a non-scaling aura and $W are gotten last, since they require items to be fully effective and pushing the lane early on is unsafe.]],

[[Don't forget about $helm_of_the_dominator's creep domination ability. Get a creep with a good aura to follow you around or stack ancient camps, by pulling them at the 52 second mark. Luna's AoE makes her one of the best heroes at clearing ancient stacks.]]

}

end)

---------------

Go('Morph', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'flask'}, {2 ,'tango'} },
    {I_CORE,
        'ring_of_aquila', 'wraith_band', 'power_treads', {OPT, 'sphere'} },
    {I_LUXURY,
        'ethereal_blade', 'skadi', 'manta', 'butterfly',
        'black_king_bar', 'helm_of_the_dominator', 'heart' },
}

Guide.Tips{

[[$H - his $E skill can give him an effective 5 AGI growth per level (if you convert all your STR) and $ethereal_blade + $W can deal massive damage. Meanwhile, $Q and $R allow him to safely farm all game long while still being able to show up at important team-fights.]],

[[Travels give you insane mobility for farming, but only get them if you are going really well. $manta and $sphere are nice options if you want some more survivability early game.]],

[[When in a though spot, Morph STR points for a very strong pseudo-heal.]],

}

end)

---------------

Go('Naga', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, E}, W, {MAX, Q}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'quelling_blade', 'vanguard', 'power_treads', 'diffusal_blade'},
    {I_LUXURY,
        'heart', 'manta', 'butterfly' },
}

Guide.Tips{

[[$H is an illusion carry hero. Early on she has good nuking and pushing power, and later in the game she can transition to a Radiance based farmer or do great DPS with diffusal. Finally, her $R is great for positioning in team fights and setting ridiculous combos with Enigma or Tidehinter.]],

[[The skill build is straightfoward: max nuke, get one point in the disable and then max images. For the item build, avoid raw damage items, orb items, and Vlads since those don't benefit the illusions. $diffusal_blade and $radiance are the exceptions: feedback works on melee illusions and radiance, if rushed, can greatly speed your farming and teamfight potential (similarly to Phantom Lancer).]],

[[$H is great against BKBs: $W goes through BKB and $R does not put magic immune units to sleep, meaning you can use your ultimate to turn a fight into a 5 vs 1 if someone on the other team turns on his BKB.]],

}

end)

---------------

Go('PL', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, R}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'power_treads', 'vanguard', 'diffusal_blade'},
    {I_LUXURY,
        'heart', 'butterfly', 'manta', 'travel_boots' },
}

Guide.Tips{

[[$H is an illusion-based hard carry. Late game, he can can create large armies of expendable illusions that can be used for DPS or for mass pushing or split pushing.]],

[[Misc illusion mechanics: Illusions do not benefit from damage items, auras or orb effects (so don't get a Vlads!). Notable exceptions are Radiance (the illusions get the burn effect) and Diffusal Blade (they get the mana burn).]],

[[If you are able to farm it fast enough, a quick Radiance can be a great pick. Use your radiance-empowered illusions to farm multiple lanes and the jungle and send them forward to harass before teamfights.]],

[[While you should focus primarily on farming during the early game, $Q is a strong, low-cooldown, spell that can be devastating if combined with an allied mana source (like Keeper of the Light).]],

[[An alternate skill build is leaving Doppelwalk at 1 and getting lots of stat points instead. Its better for farming and carrying latter on but puts a larger toll on your mana pool ($W's mana cost is larger at level 1).]],

}

end)

---------------

Go('POTM', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {1, W}, {1, E}, {MAX, Q}, {MAX, W}, {MAX, E}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'slippers'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'bottle', {2, 'wraith_band'}, 'power_treads'},
    {I_LUXURY,
        'sphere', 'manta', 'butterfly' },
    {"Orb Effects",
        'diffusal_blade', 'desolator', 'helm_of_the_dominator' },
}

Guide.Tips{

[[$H is a classical semi-carry with strong ganking abilities. Her high burst damage, coupled with her stun and her ability to catch up with her prey makes her among the best semi-carry gankers in the game. Later in the game she can transition to a solid dps role but will be outperformed by true dps carries unless she can take advantage of an early level or item lead.]]

}

end)

---------------

Go('Riki', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, Q, E, {MAX, W}, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'slippers'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'poor_mans_shield', 'power_treads', 'diffusal_blade' },
    {I_LUXURY,
        'manta', 'black_king_bar', 'butterfly', 'basher', 'greater_crit' },
}

Guide.Tips{

[[$H is a carry with great burst damage from $E and a suberb AoE silence in the form of $Q. Oh, and he also gets permanent invisibility starting at level 6...]],

[[Early game try your best to farm as much as you can without dying. Aftetr you get your core items you can start taking part in teamfights and ganks, using your $Q + $E combo to take down fragile casters and supports before they can come out of the smoke screen. The smokescreen is also good against other melee carries, since they get affected by the miss chance.]],

[[$diffusal_blade is certainly the item to go for: not only does it give great damage with $E, but the purge keeps targets inside the smokescreen for longer. Additionaly, the mana burn transfers to manta illusions and in Dota 2 you can purge yourself to remove the $dust buff.]],

}

end)

---------------

Go('Troll', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, {MAX, W}, {MAX, E}, {MAX, Q} }

local boots = If(DOTA_VERSION == 1, 'power_treads', 'phase_boots')

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'poor_mans_shield', boots, 'helm_of_the_dominator', 'invis_sword' },
    {I_LUXURY,
        'black_king_bar', 'manta', 'lesser_crit', 'butterfly',
        'satanic', 'monkey_king_bar' },
}

Guide.Tips{

[[$H is a melee carry (yes, he is also ranged but he is more effective in melee form). While he has good right click damage and can single out enemies with his bash, he also has a low-cooldown global team damage boosting and pushing skill to help his team. If you stop to think about it, its actually very similar to Lycanthrope and many of the same principles apply.]],

[[You should spend most of your time in melee form, since it gives so many bonuses. Use the ranged form only for laning and for when you need to cast the slowing version of Whirling Axes.
Troll Warlord is very good at farming neutrals after he gets levels in Fervor and some lifesteal. He is also quite adept at killing Roshan solo. Don't forget to use your $helm_of_the_dominator's dominate ability - just having a Centaur or Wolf give you their aura speeds your jungling by a lot.]],

[[A common build mistake is getting IAS items to "proc bash more often". $H already has tons of IAS from his skills so getting damage items is more effective.]],

tip_phase_boots_crit()

}

end)

---------------

Go('Gyro', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'ring_of_aquila', 'phase_boots', 'bottle' },
    {I_LUXURY,
        'invis_sword', 'helm_of_the_dominator', 'black_king_bar',
        'monkey_king_bar', 'butterfly', 'satanic' },
}

Guide.Tips{

[[$H is a semicarry with some nuking and ganking potential early game and some carry and AoE potential lategame. Early on, get some cheap health and mana regen and try to pull of some ganks. If you are successful you can carry your advantage into the lategame and transition into more traditional agi DPS items.]],

[[Remenber that $E's hits farther then your attack range so its its not strictly necessary to order attacks on your primary target.]],

[[Depending on how the game is going on, it might be a good idea to delay maxing getting $E or $Q for some levels in stats.]],

}

end)

---------------

Go('Drow', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, {MAX, E}, {MAX, W}, {MAX, S} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'slippers'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        {OPT, 'wraith_band', 'wraith_band'}, 'power_treads', 'helm_of_the_dominator',
        'manta', 'black_king_bar' },
    {I_LUXURY,
        'butterfly', 'greater_crit', 'heart' },
}

Guide.Tips{

[[$H is a ranged agi carry. Her niche lies in her great midgame damage, due to her $R and $E allowing her to shine before other carries would come online.]],

[[Orbwalking is a very important skill to master - manually cast your $Q and immediately issue a move command to cancel the animation and move closer to your target. This gives the best DPS when chasing and will not draw creep or tower aggro.]],

[[When laning, take advantage of your very good attack range and orb-walking potential to harass. In clashes, be aware of your squishiness - don't charge right in the middle and remember to use your silence at oportune times.]],

tip_orb_lifesteal('helm_of_the_dominator', 'Q').. 
[[In this build Frost Arrows are left at level one. This still allows for orbwalking and since we go for Helm of the Dominator the extra levels would be somewhat wasted. That said, feel free to put more levels on Frost Arrows if you are ganking and killing a lot.]],

}

end)

---------------

Go('TA', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {2, E}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'flask'} },
    {I_CORE,
        'bottle', 'phase_boots', 'desolator', 'blink' },
    {I_LUXURY,
        'black_king_bar', 'medallion_of_courage', 'assault', 'butterfly' },
}

Guide.Tips{

[[$H is a bursty physical damage "nuker" and is usually played as a solo mid ganker and semi-carry.]],

[[$R are a great scouting tool but also can be used to cast slows on-demand. Blink Dagger is very useful for closing on on unsuspecting targets and also compensates for your almost-melee range.]],

[[Skill build rationale: $Q is maxed first since it is your versatile, bread-and-butter skill. 2 levels of $E should be enough for laning and then start getting $W levels when you enter the ganking phase.]],

[[$H needs to be played aggressively for maximum effect:]]..
Markup.ulist({
    [[$Q can let you walk into situations other heroes would rather avoid, due to the damage prevention.]],
    [[$W is an offensive skill first and invisibility spell second!]]
}),

[[To avoid whiffing on Meld, ]]..
Markup.link(
    "http://www.playdota.com/guides/the-solo-mid-guide",
    [[position you cursor on top of your target and immediately right-click after melding so you don't miss]])..
[[.]],

}

end)

---------------

Go('Ursa', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {3, W}, {MAX, E}, {MAX, W}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'ring_of_basilius', 'phase_boots', 'vladmir', 'blink' },
    {I_LUXURY,
        'black_king_bar', 'heart', 'sheepstick' },
}

Guide.Tips{

[[If $H can connect his combo he can almost instantly kill any single hero, provided you don't get disabled and they don't have a $ghost. $E' quadratically growing damage also guarantees that not even the tankiest enemy can be safe. $H is also arguably the best Roshan killer in the game, and you can easily solo him as soon as you have your Vlads and maxed Fury Swipes.]],

[[Don't be fooled by $H being an agility hero. He already have infinite attack speed with overpower, so its better to focus on getting mobility, damage and HP.]],

[[As with any other hero with only melee range skills, you really need a Blink Dagger (or a $invis_sword, if its a pub).]],

[[Overpower lasts more than its cooldown. You cast it and wait before engaging then you hit the enemy for 5 quick hits and then immediately overpower again for another 5 fast hits. This can deal devastating damage, and is great for ganking or killing Roshan.]],

}

end)

---------------

Go('VS', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, W, E, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, 'flask', {2, 'tango'}, {3, 'clarity'} },
    {I_CORE,
        'magic_wand', 'boots', 'ward_observer', 'ward_sentry' },
    {I_LUXURY,
        'ancient_janggo', 'urn_of_shadows', 'vanguard', 'hood_of_defiance',
        'ultimate_scepter' },
}

Guide.Tips{

[[Even though she is an agility hero with good stats, $H is traditionally played as a support or roamer (and has always been one of the top heros in that role, if I may say). The reason the why the support build works is that not only does $H have a great stun but both her "steroid" skills ($E and $W) actually work for her allies too. Also, her attack range is not that good and using $R can sometimes be too risky if your team is depending on you to carry them.]],

[[$R's most common use is to pick off enemy heroes that get out of position. It can also be used to cancel channelling spells and TPs, even through BKB. (Enigma's Black Hole is a shining example).]],

}

end)

---------------

Go('BH', {day=30, month=7, year=2012}, function()

Guide.SkillBuilds{
    {name = "Hard lane skills",
        {2, E}, W, {MAX, Q}, {MAX, W}, {MAX, E} },
    {name = "Safe lane skills",
        {MAX, W}, E, {2, Q}, E, {MAX, Q}, {MAX, E} }
}

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'poor_mans_shield', 'power_treads', 'ring_of_aquila', 'bottle' },
    {I_LUXURY,
        'ancient_janggo', 'desolator', 'monkey_king_bar',
        'butterfly', 'manta', 'heart' },
}

Guide.Tips{

[[$H is a ganker and semi-carry. His signature skill is $R, a skill that can give a great gold advantage to his whole team, but he is also known for his flexible laning possibilities.]],

[[The reasoning behind the weird skill builds:]]..
Markup.ulist({
    [[$Q has a weird damage scaling. Either rush all 4 levels or leave it at level 2.]],
    [[$W is great for lane control and farming, but only if you can safely get on melee range.]],
    [[$E has a longer duration then its cooldown starting on level 2. (levels 3 and 4 give less of a benefit)]],
}),

[[If you are on a safe lane, harass heavily with $E + $W, and try to dominate your opponent. If on a hard lane, try to use $E to safely leech XP and lasthits. If on a solo mid position, get two levels of Toss at 4 and 5 for maximum ganking potential.]],

[[Don't be selfish about $R - it gives a MS buff to all your team and it gives everyone gold regardless of who gets the final last hit. $R also reveals invisible heroes so it is a good counter to heroes that depend on that to escape.]],

tip_ministun('Q'),

[[The idea behind this item build is to get some mana and survivability early game and lots of damage lategame to work with $W's crit.]],

[[This miniguide is heavily borrowing from ]]..
Markup.link(
    "http://www.dotahut.com/guides.php?g=49",
    [[Bulba's guide to Bounty Hunter]])..
[[.]],

}

end)

---------------

Go('Xin', {day=30, month=7, year=2012}, function()

Guide.Tips{

tip_todo(),
    
}

end)

---------------------
-- Scourge Agility --
---------------------

Go('BS', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {2, W}, Q, {MAX, E}, {MAX, W}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'quelling_blade', 'slippers', 'branches', 'flask' },
    {I_CORE,
        'poor_mans_shield', 'phase_boots', 'radiance', {OPT, 'hand_of_midas'} },
    {I_LUXURY,
        'dagon', 'monkey_king_bar', 'desolator',
        'diffusal_blade', 'black_king_bar', 'butterfly' },
}

Guide.Tips{

[[$H is ganker and dpser. Early on he can lane relatively safely due to the heal from $W and afterwards he can gank spellcasters using his long duration silence and $R.]],

[[$W allows you to last hit more aggressively than other melee heroes. Be sure to get the denies as well!]],

[[If you are having a hard time in lane, get more levels of Bloodbath. If you are having a great time, consider leaving it at just level 1.]],

[[The idea behind the $radiance build is that you can passively gain life from creeps that die from standing around you. It also complements your chasing potential and is an attempt at helping you carry the game a bit.]],

}

end)

---------------

Go('Clinkz', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, W}, E, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'circlet', {3, 'branches'}, 'flask', 'tango'},
    {I_CORE,
        'wraith_band', 'magic_wand', 'power_treads', 'orchid' },
    {I_LUXURY,
        'black_king_bar', 'monkey_king_bar', 'butterfly', 'skadi' },
}

Guide.Tips{

[[$H is a ganker and a building razer. He can use his Windwalk to sneak on his target and his other 3 skills do do great burst damage.]],

[[$orchid or $sheepstick are actually great items on $H. He needs the mana regeneration and the disables are crucial for bursting down lone targets.]],

[[For luxury items, prioritize damage, since you already have great attack speed with $Q. $skadi is listed because it stacks with $W, giving good chasing power.]],

[[$R gives more bonuses if you eat a larger jungle creep.]],

}

end)

---------------

Go('Brood', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_regen', {2, 'branches'}, 'flask' },
    {I_CORE,
        'soul_ring', 'power_treads', 'orchid', 'black_king_bar'},
    {I_LUXURY,
        'butterfly', 'manta', 'greater_crit'},
}

Guide.Tips{

[[$H is quite possibly the best solo pusher in the game. Push a lane while the rest of your team pushes another and the enemy will be in a tough position.]],

[[The HP regeneration from your webs coupled with a Soul Ring allows you to spam $Q nonstop (start doing this after level 5). If your lane is clear, take down the tower. If it isn't, send the spiders to the jungle to farm some neutrals. You can also just use the nuke to push an opponent off the lane if you want to.]],

[[Do not feed the spiderlings. They give out significant gold and XP when they die!
You can start getting Incapacitating Bite levels at level 8 if you need the killing power earlier.]],

[[[Webs destroy trees where they are planted. You can use this to get clear vision of the jungle when laning and to open up new paths when jungling.]],

}

end)

---------------

Go('NA', {day=30, month=7, year=2012}, function()

Guide.Tips{
    tip_todo(),
}

end)

---------------

Go('Weaver', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, E, {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {4 , 'branches'}, 'ring_of_protection', {2, 'tango'} },
    {"Radrush Core",
        'ring_of_basilius' , 'ring_of_health', 'vitality_booster', 'radiance'},
    {"Midgame Core",
        'ring_of_aquila', 'medallion_of_courage', 'vanguard', 'power_treads' },
    {I_LUXURY,
        'heart', 'black_king_bar', 'butterfly', 'monkey_king_bar', 'rapier'},
}

Guide.Tips{

[[$H can be very elusive and hard to kill hero, due to her great mobility with $W and ability to ignore burst damage with $R. Because of this, she is often played as a $radiance carry, although she can also be built for midgame damage, due to her minus armour aspect and minor tower diving potential.]],

[[The item build should always a fine balance between defensive and damage items. You absolutely require extra survivability in order to be able to use your skills to maximum effect but you will eventually need damage items in order to make your presence felt.]],

[[Be careful about prolonging your game too much and farming excessively. Weaver can get out-carried if the game goes extremely late.]],

}

end)

---------------

Go('PA', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, W, E, {MAX, Q}, {MAX, W}, {MAX, E} }

local boots = If(DOTA_VERSION == 1, 'power_treads', 'phase_boots')

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'poor_mans_shield', boots, 'bfury',
        'helm_of_the_dominator', 'black_king_bar' },
    {I_LUXURY,
        'satanic', 'monkey_king_bar', 'bfury', 'basher' },
}

Guide.Tips{

[[$H is a carry with great chasing ability and the strongest critical strike ability in the game. Once she gets a BKB and some damage items, she can almost instantly kill a hero she jumps on with $W.]],

[[During the laning phase, $Q should be primarily used as a last-hitting tool. Don't harass heroes with it since that does not do vety much damage.]],

[[Do not buy evasion ($butterfly) or critical strike items ($greater_crit), since they do not stack with your skills.]],

tip_phase_boots_crit(),

}

end)

---------------

Go('SF', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, {MAX, W}, {MAX, R}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'flask'} },
    {I_CORE,
        'bottle', 'power_treads', 'black_king_bar' },
    {I_LUXURY,
        'manta', 'butterfly', 'desolator', 'greater_crit'},
    {"Mobility",
        'blink', 'invis_sword' }
}

Guide.Tips{

[[$H is a glass-cannon semi-carry. He can do amazing damage with his ultimate, his 3 cheap AoE nukes and his high attack damage. He is also a strong mid lane hero, but he is vulnerable to ganks and $smoke_of_deceit.]],

[[When getting items, mobility and survivability let you make better use of your ultimate while traditional carry items like $butterfly and $manta help your physical DPS.]],

[[Learn how to cancel $Q's backswing animation - it speeds your combo by a lot.]],

}

end)

---------------

Go('TB', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, W}, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango'},
    {I_CORE,
        {2, 'wraith_band'}, 'power_treads', {ALT, 'manta', 'sange_and_yasha'} },
    {I_LUXURY,
        'skadi', 'helm_of_the_dominator', 'butterfly', 'heart' },
}

Guide.Tips{

[[$H is an illusion-based carry. While he doesn't have any nukes or straight disables, he has very good innate DPS from his skills and his illusions can deal significant damage early on, making him a relatively good pusher.]],

[[For the skill build, more early levels of $W are good for farming, but you can prioritize $E if you are doing fine and are aggressively pushing. $Q is usually maxed last, but it can be a good idea to get a single level early, for defensive purposes.]],

[[For the item build, go preferrably for stat items, as those also benefit the illusions.]],

}

end)

---------------

Go('Spectre', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'quelling_blade', 'flask', {2, 'tango'} },
    {I_CORE,
        'poor_mans_shield', 'magic_wand', 'power_treads', 'radiance'},
    {I_LUXURY,
        'manta', 'heart', 'diffusal_blade', 'blade_mail', },
}

Guide.Tips{

[[Mercurial is a hard carry. However, instead of carrying with many DPS items, Spectre does its job by getting tanky items and staying in the middle of a fight dealing damage over time with Radiance and Dispersion.]],

[[While the $radiance build is optimal if you can quickly pull it off, if you are having a hard time fall back to the $diffusal_blade build. It also lets you go for a more ganky playstile too, with $Q, $W and your global ultimate.]],

[[On teamfights, cast $R first. It reveals the enemy team and let the illusions deal lots of $diffusal_blade and $W damage.]],

}

end)

---------------

Go('Veno', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, W, {MAX, E}, {MAX, Q}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango', {2, 'clarity'} },
    {I_CORE,
        'magic_wand', 'arcane_boots', 'ward_observer'},
    {I_LUXURY,
        'vanguard', 'pipe', 'force_staff', 'ultimate_scepter'},
    {I_OPTIONAL,
        'ring_of_basilius', 'mekansm' }
}

Guide.Tips{

[[Although $H is an agility hero with decent stats, his skills allow him to be played as a very item independent support hero. $Q is one of the most powerful slows in the game at level 1 (and is also cheap, mana-wise), $E are very strong pushing and counter-pushing skills and $R is a "fire and forget" ultimate that does significant damage even if Veno gets focused down.]],

[[Higher levels of $E are great for stopping enemy pushes. They deal lots of damage to creeps, last a long time and are immune to AoE wave clearing effects.]],

[[$E are very cheap and spammable and can be used to provide vision. Casting one whenever you plan to enter a jungle or go up a ramp can let you avoid many nasty surprises.]],

[[A support Venomancer is very item independent, so he should be fine just getting tanking items while also providing wards/dust/etc to his team.]],

[[$R's poison will not kill enemies and will leave them at 1 HP instead. To ensure a kill tag them with either $Q or $W.]],

}

end)

---------------

Go('Viper', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'circlet', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'power_treads', {ALT, 'vanguard', 'hood_of_defiance'} },
    {I_LUXURY,
        'ultimate_scepter', 'black_king_bar', 'butterfly' },
}

Guide.Tips{

[[Viper is tanky mid game DPS hero and ganker. He is also a very strong laner, due to his powerful orb-attacks.]],

[[This item build prioritises some cheap survivability items. Viper already deals good early game damage with his skills but he needs to get close in for maximal effectiveness (and building him for the late game is usually not worth it). Its kind of a similar idea to most Razor builds.]],

[[Once Viper has his ultimate, you can use it to gank heavily. (However, get an ally with a stun to help you, since you have no way to interrupt a TP scroll). On teamfights, try to cast your ultimate on the enemy carry, since it slows attack speed by quite a bit and the disable goes through BKB.]],

[[Orbwalking is a very important skill to master - manually cast your $Q and immediately issue a move command to cancel the animation and close in to your target. A manualy casted $Q also doesn't draw creep or tower aggro.]],

}

end)

---------------

Go('Meepo', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, {MAX, W}, E, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {2, 'branches'}, {2, 'flask'}, 'tango' },
    {I_CORE,
        'ring_of_basilius', 'mekansm', 'travel_boots'},
    {I_LUXURY,
        'pipe', 'vladmir', 'assault' },
    {I_OPTIONAL,
        'blink' }
}

Guide.Tips{

[[Meepo is a mid-game carry (this is not the same as a semi-carry!). He depends on getting a significant level advantage early on and the using his multiple clones to roll face between levels 11 and 16. Once his clones reach a hero they can keep him permantly slowed or netted and casting all poofs in quick succession deals massive magical damage.]],

[[Your clones also act as an extra hero when it comes to XP gain. If they are on separate places they will get XP from both and if they are together with an ally they will get a greater share of XP from then the nearby heroes. Use this to get the level advantage you need and try to win the game while your numbers advantage is still significant.]],

[[In an ideal world, the team can farm some of Mek, Vlad's or Pipe to let Meepo get stronger faster.]],

[[While Meepo is knows as a hard hero to micro, its not actually that hard to pull off the simpler moves. For example, poofing a target you have netted is just a matter of repeating W-click-tab-W-click-tab...]]

}

end)

---------------

Go('Razor', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, W, E, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'slippers'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'poor_mans_shield', 'magic_wand', 'phase_boots',
        'hood_of_defiance', 'vitality_booster' },
    {I_LUXURY,
        'pipe', 'manta', 'mjollnir', 'heart', 'monkey_king_bar' },
}

Guide.Tips{

[[$H is a DPSer, chaser and an anti-carry. While he has less inherent right-clicking potential lategame, he has a good early-game presence due to his spells and $W can give a great boost in damage (while crippling an enemy hero at the same time).]],

[[Since most of $H's damage comes from his spells, and he needs to stay in close to use them, the core item build prioritises tankability and the mobility from $phase_boots. You can transition into more traditional agi carry items after that. This is kind of similar to most Viper builds.]],

}

end)

---------------

Go('Slark', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, E, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'power_treads', 'vanguard' },
    {I_LUXURY,
        'sange_and_yasha', 'black_king_bar', 'basher', 'armlet',
        'butterfly', 'skadi' },
}

Guide.Tips{

[[$H is a weird kind of ganker and carry - his $E can over a fight turn $H into a fearsome beast and his $R allows him to quickly appear anywhere in the map, with fully regenerated HP. However, he is very fragile and has to deal with a pitiful STR gain.]],

[[This item build prioritises getting HP items, so you can survive until you steal enough stats and so you can cast $Q and put the regeneration of Shadow Dance to good use.]],

[[Remember that $Q dispells buffs! If you time if correctly you can use it to remove stuns and pesky things such as $dust.]],

[[Put the extra stats you get after a fight to good use - go take down some towers or some neutrals.]],

[[$R can act as a "ward detecting maphack" if you pay attention to it.]],

}

end)

---------------

Go('Void', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, W, {MAX, E}, {MAX, W}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'poor_mans_shield', 'power_treads', 'bfury' },
    {I_LUXURY,
        'black_king_bar', 'butterfly', 'greater_crit', 'satanic' },
    {I_OPTIONAL,
        'vanguard', 'mask_of_madness', 'hand_of_midas'},
}

Guide.Tips{

[[$H is a hard-carry. When farmed he can reliably get rid of one or two heroes during the $R duration and supports become food to $Q coupled with some bashes.]],

[[If you are planning on coming online in the lategame, $bfury and Midas are items that help you farm while still being useful by themselves. If you want to have a bigger early presence in your $R, consider getting a quick $mask_of_madness or a $necronomicon. If you need survivability, a vanguard makes you very tanky when coupled with $W.]],

[[The skill build is similarly flexible: you can delay $W if you feel safe and you can delay $E if you want to be more defensive.]],

}

end)

---------------

Go('Medusa', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {2, S}, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {4, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'ring_of_aquila', 'power_treads', 'sphere',
        'manta', 'butterfly' },
    {I_LUXURY,
        'satanic', 'skadi', 'travel_boots',
        'rapier', 'mjollnir' },
}

Guide.Tips{

[[$H is one of the hardest hard carries in the game. If she gets farm and the game goes late she deals significant damage in an AoE, while being very tanky and hard to kill due to $E and $sphere. Early game she lacks ganking potential, but $W gives some decent lane control and $R can act as a "get out of a gank free" card.]],

[[Try to cast $W so that the last bounce hits an enemy hero (preferably killing some creeps in the way). This deals the most damage and refunds almost all the mana you spent.]],

[[Activating $manta will turn off the $E so remember to turn it back on afterwards.]],

[[I can't stress enough how much farm Medusa needs to be effective - make sure your team knows that you wont be able to help in early fights very much.]],

}

end)

---------------------------
-- Sentinel Intelligence --
---------------------------

---------------

Go('CM', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, W, {MAX, E}, {MAX, Q}, {MAX, W}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'branches'}, 'flask', 'tango', 'courier', 'ward_observer' },
    {I_CORE,
        'boots', 'magic_wand', 'bracer', 'bracer' },
    {I_LUXURY,
        'urn_of_shadows', 'point_booster', 'ghost',
        'ward_observer', 'smoke_of_deceit' },
}

Guide.Tips{

[[$H's two nukes/disables make her a very powerful supporter and roamer in the early stages of the game. Her aura can also greatly help mana-starved heroes, allowing them to spam spells when they normally couldn't.]],

[[$H has very flexible item and skill builds. Here are some rules of thumb:]]..
Markup.ulist({
    [[Most of the time it is better to max $Q over $W, since it is AoE.]],
    [[You often want one or two levels of $E by level 4.]],
    [[Freezing Field is expensive and hard to cast (since CM is slow, squishy). Only get it at level 6 if you know what you are doing.]]
}),
    
    [[Your casting animation back-swing is enormous. Always cancel it by immediately giving another order after casting a spell.]],
    
    [[Be mindful of CM's low movespeed. It can be hard to run away from other heroes if you get caught out of position.]],
    

}

end)

---------------

Go('Ench', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'tango', 'clarity', 'smoke_of_deceit' },
    {I_CORE,
        'ring_of_basilius', 'magic_wand', 'arcane_boots',
        'ultimate_scepter', 'ancient_janggo' }, 
    {I_LUXURY,
        'mekansm', 'sphere', 'shivas_guard', 'sheepstick', 'heart' },
}

Guide.Tips{

[[$H is a superb offensive jungler, being able to use converted creeps to gank and push very early.]],

[[$H's main weakness is her low strength stat - while her skills allow her to tank lots of damage over time, be sure to get HP items to protect her from burst.]],

}

end)

---------------

Go('Puck', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, E, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'circlet', {3, 'branches'}, 'flask', 'tango', 'clarity'},
    {I_CORE,
        'magic_wand', 'boots', 'bottle', 'blink'},
    {I_LUXURY,
        'arcane_boots', 'ancient_janggo', 'mekansm', 'sheepstick', 'shivas_guard', 'skadi' },
}

Guide.Tips{

[[Puck is an initiator by design - blink into the enemy lines, cause chaos with your silence and trap them with your ultimate to set up for an allied attack.]],

[[The early level of $E is a great way to avoid harass damage during the laning phase.]],

[[Level 4 $E lasts long enough to re-enable a disabled Blink Dagger. Good use of this combo can make Puck very hard to kill.]],

[[Blink-initiation is much faster than your orb and gives no time for the enemy to spread out. It also allows you to use the $Q to escape latter.]],

}

end)

---------------

Go('Chen', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {2, W}, {MAX, E}, {MAX, W}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', 'ring_of_protection', 'flask' },
    {I_CORE,
        'ring_of_basilius', 'arcane_boots', 'mekansm', 'ward_observer' },
    {I_LUXURY,
        'ultimate_scepter', 'ancient_janggo', 'vladmir' },
}

Guide.Tips{

[[Chen is a natural jungler, with tremendous early game power from his converted creeps. Use them (and their skills)  to gank and push towers. Later in the game, the creeps become less powerful so use them more for the disables and buy items to support your team.]],

[[$Q deals less damage then $W, but does have a slow. Swap it with $W in the skill build if you don't want to depend on you creep micro or allied heroes for providing disables.]],

[[Best creeps: Any creep from the hard camp, centaurs and, situationally, an Alpha Wolf (damage aura) or Kobold Taskmaster (speed aura)]],

[[Remember that Holy Persuasion can be used to save allies from sticky situations, by sending them back to base!]],

}

end)

---------------

Go('KOTL', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, E}, {MAX, Q}, {MAX, R}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', 'ward_observer', 'branches', {2, 'tango'} },
    {I_CORE,
        'mekansm', 'ring_of_basilius', 'tranquil_boots' },
    {I_LUXURY,
        'arcane_boots', 'vladmir', 'pipe', 'travel_boots',
        'sheepstick', 'dagon', },
}

Guide.Tips{

[[$H's $E can turn any allied hero with a low CD spell into a powerful spellcaster.]],

[[Don't be selfish! Prioritize chakra-ing your teammates over yourself and don't fall into the trap of using $Q to farm.]],

[[$H is extremely item-independent. Don't farm to much and pick utility items as needed by your team.]],

}

end)

---------------

Go('Zeus', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, E, {MAX, W}, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'tango'}, {4, 'clarity'} },
    {I_CORE,
        'boots', 'bottle', 'point_booster', 'magic_wand',
        {OPT, 'bracer', 'null_talisman'} },
    {I_LUXURY,
        'arcane_boots', 'ultimate_scepter', 'mekansm',
        'force_staff', 'ancient_janggo', 'ultimate_orb' },
}

Guide.Tips{

[[Zeus is a glass cannon nuker. He can deal great damage from a distance with his spammable spells but is squishy and demands careful positioning in battles.]],

[[Rules of thumb from ]]..
Markup.link(
    "http://www.playdota.com/guides/shocking",
    [[Merlini's great Zeus guide]])..
Markup.ulist({
    [[Use $W first (it gives vision)]],    
    [[Cast spells safely from afar (from fog, trees, hills, etc).]],
    [[Prioritize raw HP and Mana items (nulls, bracers, point booster) over regeneration (Euls, etc)]]
}),

tip_ministun('W'),

[[Both your ultimate and lightning bolt give vision of your targets (and bolt gives truesight!).]],

}

end)

---------------

Go('Prophet', {day=30, month=7, year=2012}, function()

Guide.SkillBuilds{
    {name="Laning Skills",
        Q, W, {3, E}, {MAX, W}, E, {MAX, Q} },
    {name="Jungling Skills",
        E, W, Q, {MAX, E}, {MAX, W}, {MAX, Q} }
}

Guide.ItemBuild{
    {"Lane Starting",
        'circlet', {4, 'branches'}, {2, 'tango'} },
    {"Jungle Starting",
        'ring_of_basilius', {2, 'clarity'} },
    {I_CORE,
        'power_treads', 'magic_wand', 'urn_of_shadows',
        {OPT, 'medallion_of_courage', 'hand_of_midas'} },
    {I_LUXURY,
        'mekansm', 'sheepstick', 'force_staff',
        'necronomicon', 'desolator', 'ultimate_scepter' },
}

Guide.Tips{

[[$H is characterized by his global presence with $W. Early game he can farm and gain levels while still being able to aid in any gank and late game he can keep lanes pushed for map control, while still being able to go back for any teamfight.]],

[[Don't mindlessly spam $R, as doing so steals gold from you allies and pushes lanes making it harder for them to farm. For maximal effectiveness, use it to push and counterpush at opportune times and wait until your targets are visible before casting (%R will not hit units under the fog of war).]],

[[It is possible to cancel the Teleport during casting animation to confuse your enemies.]],

}

end)

---------------

Go('Silencer', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, R}, {MAX, W}, S, {MAX, E}, {MAX, S}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'mantle', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'power_treads', 'mekansm', 'force_staff' },
    {"Support Extension",
        'vanguard', 'hood_of_defiance', 'rod_of_atos', 'cyclone' },
    {"Carry Extension",
        'sheepstick', 'shivas_guard' },
}

Guide.Tips{

[[$H's stats and his $W point him at a semi-carry role, while his passive and ultimate have a more utility role, greatly hindering enemy combo-based heroes (such as Tiny or Storm Spirit). All in all, a good $H will adapt towards the end of the spectrum that most helps his team (getting more utility and dps items if carrying and getting more survivability items if supporting with his passive).]],

[[$Q is a situational skill. Pick 3 or 4 levels of it early on if you are against a trilane or a particularly susceptible hero (someone with only costly skills, such as Skeleton King or Sven) or skip it completely otherwise.]],

}

end)

---------------

Go('Lina', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, E, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango', {2, 'clarity'} },
    {I_CORE,
        'arcane_boots', 'bottle', 'urn_of_shadows' },
    {I_LUXURY,
        'ward_observer', 'ancient_janggo', 'force_staff',
        'force_staff', 'blink', 'sheepstick' },
}

Guide.Tips{

[[$H is a nuker that can also have reasonable physical damage latter in the game (with $E).]],

[[Try to lane with a stunner that can set up an easy $W for you.]],

[[Don't waste your ult just to get kills on dying enemies. However, don't be afraid to use it to speed up a gank if enemies are missing.]],

[[Later in the game, you can keep up $E for long periods of time if space your spell casts a bit instead of casting them in a quick sequence.]],

}

end)

---------------

Go('Storm', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ Q, E, {3, W}, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'mantle'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'bottle', 'power_treads', {OPT, 'null_talisman', 'null_talisman'} },
    {I_LUXURY,
        'black_king_bar', 'sheepstick', 'bloodstone', 'orchid', 'heart' },
}

Guide.Tips{

[[$H's $R is one of the best mobility skills in the game. Smart use of it allows $H to initiate, escape and chase very well.]],

[[$H is fragile, so don't initiate teamfights unless you have BKB or know what you are doing. Remember to abuse your mobility to get the weaker and out of position targets first.]],

[[As with any spellcaster hero, your skills are the most effective around level 11-14. Use this window to gank as much as possible.]],

[[Remember to always attack an enemy after casting a spell in order to get the most $E. Small jumps with $R can also give you extra procs when your other spells are on cooldown.]],

[[An alternative build is maxing $E over $Q after level 6. This has less burst damage but is also less mana intensive. You need to be good at not wasting $E procs though.]],

}

end)

---------------

Go('WR', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, W}, {MAX, Q}, {MAX, R}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {4, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'ring_of_basilius', 'phase_boots', 'mekansm', 'force_staff' },
    {I_LUXURY,
        'sheepstick', 'monkey_king_bar', 'orchid',
        'maelstrom', 'ultimate_scepter' },
}

Guide.Tips{

[[$H is arguably the most versatile hero in DotA. Because of this, she will often pick up utility items and try to fill whatever role her team is missing the most.]],

[[Practice getting good shackleshots. 3.75 seconds is a lot and is game-breaking if you catch two heroes in it!]],

[[When harassing, keep in mind that Powershot loses some power after going through creeps.]],

[[$R is not a channeling spell! You can move and cast spells as long as you don't order an attack on a different target.]],

}

end)

---------------

Go('Disruptor', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ R, {MAX, Q}, E, {MAX, W}, {MAX, E}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, 'flask', {2, 'tango'}, {3, 'clarity'} },
    {I_CORE,
        'bottle', 'magic_wand', 'arcane_boots'},
    {I_LUXURY,
        'mekansm', 'force_staff', 'pipe', 'urn_of_shadows',
        'veil_of_discord', 'sheepstick' },
}

Guide.Tips{

[[$H is a nuker that can be played as a decent mid hero. While he lacks any direct disables, he has lots of positioning skills and can cause lots of AoE confusion if he gets his combo off.
$W is a very versatile skill and is key to playing a good $H. Some things to keep in mind are using it to win rune races and as a way to send enemies enemies that just TP-ed in back to their base.]],

[[The idea behind the skill build is maxing the nuke first and then $W, since it get much better with levels. $E is gotten last since it is already pretty good at level 1, but you can get it before $W if you want. Finally, the 2nd and third levels of $R are gotten only later since they give only small damage increases]],

[[As for items, go for generic utility and support items.]],

}

end)

---------------

Go('Ogre', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'mantle'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        {2, 'null_talisman'}, 'bottle', 'arcane_boots' },
    {I_LUXURY,
        'force_staff', 'blade_mail', 'bloodstone',
        'sheepstick', 'shivas_guard' },
}

Guide.Tips{

[[Ogre is a support that transitions well into the lategame - his skills are all very strong when you can spam them (and when you get levels of Multicast) . His large STR gain also makes him much less vulnerable than other supports. However, early game you will have to deal with his melee range and low mana pool.]],

}

end)

---------------

Go('Techies', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, E, {MAX, W}, {MAX, S} }

Guide.ItemBuild{
    {I_STARTING,
        'tpscroll', {2, 'tango'}, {7, 'clarity'} },
    {I_CORE,
        'arcane_boots', 'void_stone', 'ultimate_scepter', 'soul_ring' },
    {I_LUXURY,
        'bloodstone', 'sheepstick', 'force_staff', 'travel_boots' },
}

Guide.Tips{

[[The Techies have many tricks in their arsenal. They can mine just off the creep-path to deny areas for the enemy and get kills if they wander there, they can pull off early ganks with suicide, they can stop creep spawns before the game really starts by eating a tree and placing a mine there, they can provide safe havens for their team with stasis traps, they can ward with remote mines, etc. An underrated hero that takes practice to master.]],

[[Stasis wards are amazing, so abuse them. Techies can lose a lot of effectiveness if the other team gets a gem but smart placement of mines (in foggy areas like the top of ramps, between trees, etc) can counteract some of that. Remote mines in a neutral or ancient camp can kill a carry trying to safely finish a big item. Don't underestimate the psychological effect that Techies can have.]],

[[Land Mines demolish creepwaves and damage towers so they are great for pushing.]],

}

end)

---------------

Go('Jakiro', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, R}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', 'ring_of_protection', 'branches', 'flask', 'tango' },
    {I_CORE,
        'ring_of_basilius', 'arcane_boots', 'mekansm',
        'urn_of_shadows', 'ward_observer' },
    {I_LUXURY,
        'void_stone', 'ultimate_scepter', 'shivas_guard',
        'pipe', 'sheepstick' },
}

Guide.Tips{

[[$H is an item independent support hero with good stats gain and great pushing power with liquid fire (it damages and slows towers!). However he has to deal with his low attack range and $W being very hard to hit.]],

[[If you want to push really hard, skill $E early instead of $W.]],

[[For items, focus on getting a bit of armor and mana regeneration.]]

}

end)

---------------

Go('Tinker', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, R, {MAX, E} }

Guide.Tip([[Get the first level of $R when you get your $travel_boots. The second and third level have a higher manacost so wait until you get INT and mana items before you skill them.]])


Guide.ItemBuild{
    {I_STARTING,
        {2, 'mantle'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'bottle', 'travel_boots', 'soul_ring',
        {OPT, 'null_talisman', 'null_talisman'} },
    {I_LUXURY,
        'sheepstick', 'shivas_guard', 'force_staff', 'manta',
        'ethereal_blade', 'dagon' },
}

Guide.Tips{

[[Remember that rearm works for your items! Once you have BoT and Rearm, use it to teleport to the fountain, rearm and teleport wherever you want with full health and mana. Awesome! ]]..
Markup.small([[But some items can't be rearmed, for balance reasons: $black_king_bar, $arcane_boots, $helm_of_the_dominator, $hand_of_midas, $refresher, $necronomicon, $mekansm and $pipe]]),

[[Early game, $H uses his nukes to be a powerful laner and ganker. After he farms his the $travel_boots, however, the $R combo allows him to transition into a global pusher,  farmer and harasser like no other.]],

[[Whenever someone buy a boots upgrade on $H that is not $travel_boots, Icefrog kills a kitten.]],

}

end)

---------------

Go('Rhasta', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, Q}, {MAX, W}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'tango'}, {3, 'clarity'} },
    {I_CORE,
        'magic_wand', 'bottle', 'arcane_boots',
        'blink', 'ultimate_scepter'},
    {I_LUXURY,
        'black_king_bar', 'bloodstone', 'refresher',
        'necronomicon', 'travel_boots' },
}

Guide.Tips{

[[$H is a very strong pusher and disabler. He usually lanes solo mid (since he is very level dependent) but he also works fine with allies to abuse shackle's long disable.]],

[[Use your ultimate to take towers down quickly - take care so that there are enough creeps to tank and remember to manually control the wards so they hit the right target.]],

[[$W has a big mana cost. Level $E instead if you are not comfortable managing your mana.]],

[[It is possible to trap opponents inside your Serpent Wards (for an almost guaranteed kill). It is very easy to trap melee heroes standing still while attacking you and with practice you can even trap hexed enemies. (Some people like getting Eul's for ward trapping but I think it is overkill).]],

}

end)

---------------

Go('Rubick', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'flask'} },
    {I_CORE,
        'bottle', 'magic_wand', 'arcane_boots', 'force_staff' },
    {I_LUXURY,
        'mekansm', 'urn_of_shadows', 'necronomicon', 'bloodstone', 'sheepstick' },
}

Guide.Tips{

[[$H has a good disable ($Q) and a decent lane nuke ($W), but his $R is definitely his defining characteristic, allowing him to heavily counter heroes with strong ultimates, as well as giving him tremendous versatility.]],

[[For the skill build, favor $W if you are going for solo mid and $Q if you are going for a more supportish role.]],

[[As for the item build, go for mana and utility items, similarly to other heroes that play in the same position (Shadow Demon, Disruptor, etc).]],

}

end)

---------------

Go('SM', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'mantle'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'null_talisman', 'arcane_boots', 'bloodstone' },
    {I_LUXURY,
        'sheepstick', 'heart', 'shivas_guard' },
}

Guide.Tips{

[[$H is a squishy nuker with very high range spells. Try to use your high movespeed to stay just out of range during fights while dealing lots of damage with your spammable spells.]],

[[For the item build, the greatest needs are mana regen, mana pool and survivability.]],

}

end)

--------------------------
-- Scourge Intelligence --
--------------------------

---------------

Go('Bane', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, Q}, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, 'flask', {2, 'clarity'} },
    {I_CORE,
        'soul_ring', 'arcane_boots', 'magic_wand', 'necronomicon' },
    {I_LUXURY,
        'force_staff', 'sheepstick', 'mekansm', 'pipe' },
}

Guide.Tips{

[[$H is a hero with many single-target disables. He can use them to be a strong ganker and to dominate some lanes.]],

[[$soul_ring + $W is a very powerful laning combo.]],

[[$necronomicon minions can deal significant damage on a Fiend's Gripped hero and are easy to micro while you are channeling.]],

}

end)

---------------

Go('DS', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, E, {MAX, Q}, {MAX, E}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        'stout_shield', 'branches', 'flask', {2, 'tango'} },
    {I_CORE,
        'magic_wand', 'vanguard', 'arcane_boots', 'cloak' },
    {I_LUXURY,
        'void_stone', 'pipe', 'shivas_guard', 'ultimate_scepter', 'sheepstick' },
}

Guide.Tips{

[[$H is an unusual int hero, as he is melee and has no nukes or stuns. To compensate, he has great mobility with $E, good jungling and solo laning potential with $W and great teamfight combo potential with $Q and $R.]],

[[If against a hard lane, $H can cast $W on creeps to farm from a safe distance. If against weaker enemies he can cast shell on himself to gank or to intercept creeps behind the tower (for pushing).]],

[[$R shines on pushing and teamfight situations. You can try to drop it on top of your enemies (with Vacuum to help) to do damage and chaos or you can set it up preemptively behind their tower, to prevent them from trying to come defend it.]],

}

end)

---------------

Go('Krob', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {4, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'ring_of_basilius', 'magic_wand', 'arcane_boots',
        'mekansm', 'platemail', 'sphere' },
    {I_LUXURY,
        'shivas_guard', 'ancient_janggo', 'ghost',
        'travel_boots', 'skadi' },
    {I_OPTIONAL,
        'vanguard', 'bloodstone' }
}

Guide.Tips{

[[$H is a midgame carry. Just by standing in the middle of a fight, she can deal a large amounts of damage with $R and $Q spam. $R is also a great skill for taking down towers. In a way she plays similarly to ]]..Heroes.Necro.nick,

[[The basic idea behind the item build is to get survivability (to stay close in teamfights) and mana (for casting spells).]],

[[$R ghosts cycle between you and their target and will tend to focus whoever you attack. For best results, stand close to your intended target and be attacking it before you cast $R.]],

[[Some people like getting $phase_boots for lots of mobility (since positioning is so critical on this hero).]]

}

end)

---------------

Go('Lion', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, S}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'mantle'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        {OPT, 'null_talisman', 'null_talisman', 'magic_wand' },
        'bottle', 'power_treads', 'blink' },
    {I_LUXURY,
        'ward_observer', 'force_staff', 'sheepstick', 'ghost', 'black_king_bar' },
} 

Guide.Tips{

[[Early game, $H can use his bust damage to be a strong solo mid hero. Get some quick levels and gank all over the place with maxed $Q + $R while they are still strong.]],

[[Getting a dagger allows you to initiate very well with your multiple disables - try to position yourself so that $Q gets multiple heroes.]],

[[$E is not needed if you manage your mana well (and skipping it for $Q and $W increases your ganking power). However, a single level might be useful for killing illusions or for dispelling Curse of the Silent.]],

[[Don't waste $R just to steal kills on dying enemies. However, don't be afraid to use it to speed up a gank if enemies are missing.]],

}

end)

---------------

Go('Enigma', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', 'sobi_mask', {2, 'clarity'} },
    {I_CORE,
        'soul_ring', 'boots', 'blink', 'mekansm' },
    {I_LUXURY,
        'black_king_bar', 'travel_boots' },
}

Guide.Tips{

[[$W Eidolons allow $H to jungle, lane and push very well. $E + $R is one of the strongest combos in the game and will win teamfights if you manage to get multiple heroes with it.]],

[[When using $R, try not to leave long-range stunners (that can break your channeling) out of it. Also pay attention to heroes, like ]]..Heroes.VS.nick..[[ or  ]]..Heroes.Beast.nick..[[, that can interrupt your channeling even through BKB.]],

[[Eidolons split and heal after some attacks. When jungling, be sure to micro the weak ones back before they die. Try to get a full army of 6 eidolons before ganking.]],

[[The reason for the quick $soul_ring is that it is the only thing that will give enough mana to freely spam $W. You don't need to worry about the health degen because the Eidolons can tank damage for you anyway.]],

}

end)

---------------

Go('Lich', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'boots', 'magic_wand', 'mekansm', 'ward_observer' },
    {I_LUXURY,
        'bracer', 'bracer', 'force_staff', 'ultimate_scepter',
        'sheepstick', 'vladmir' },
    {I_OPTIONAL,
        'urn_of_shadows' },
}

Guide.Tips{

[[$H's main strength is his ability to use $E to deny large amounts of gold and experience from the enemy team in the laning phase. His strong harassing potential only adds up to that. Finally, his $R is a very strong ultimate, often capable of turning ganks and teamfights around just by itself.]],

[[$W is very cheap so remember to always keep it up on all your allies.]],

[[$R works best when there are 2 or 3 enemy units clustered together but don't be too picky waiting for the "perfect moment", as it is still devastating even when there are more units around.]],

[[$H is a great hero for beginners new to the game. His spells are easy to cast, his ultimate is "fire-and-forget" and he can still help his team even when he is underleveled or underfarmed.]],

}

end)

---------------

Go('Necro', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {4, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'ring_of_basilius', 'magic_wand', 'power_treads', 'mekansm' },
    {"Survivability",
        'cloak', 'platemail', 'ancient_janggo', 'ghost', 'vitality_booster' },
    {"Mana and damage",
        'sheepstick', 'shivas_guard', 'assault', 'travel_boots' },
}

Guide.Tips{

[[$H loves a long fight. $Q and $W deal constant DPS to the enemy team, while $Q turns you into a walking fountain for your team. To maximize this, most item builds revolve around tanking up and then getting a big mana item.]],

[[$H's mana is tight in the early game. Be a good last hitter (for Sadist) and save $magic_wand charges for when you need to use $mekansm (double heal!)]],

string.format([[$R kill thresholds: %s / %s / %s of total HP. However, don't be afraid to cast $R a little early, since it provide a stun (that even goes through BKB!) and the damage is calculated only after the stun ends.]],
    Markup.bold("23%"), Markup.bold("31%"), Markup.bold("40%") ),

[[Never get $ultimate_scepter, since its bonus is very small. $soul_ring can be viable if you are pushing very aggressively early game. Getting a single early level of $W to negate enemy regeneration in the laning phase can also work sometimes.]],

}

end)

---------------

Go('Pugna', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, W, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'flask'} },
    {I_CORE,
        'bottle', 'power_treads', 'void_stone', 'mekansm' },
    {I_LUXURY,
        'sheepstick', 'ultimate_scepter', 'travel_boots', 'black_king_bar' },
}

Guide.Tips{

[[$H is a great pusher and can play a semi-carry role but but his greatest asset is surely his $E, as it turns fights into living hell for enemy spellcasters. Pugna is the real Anti Mage.]],

[[$Q also damages towers. Abuse this when pushing or when harassing in lane.]],

[[$H's greatest weakness is his low strength stat. Compensate by getting survivability items. On the other hand, he has really good int gain so some percentage mana regen items are all he will need for casting his spells.]],

[[Early levels of $R are mostly good for healing from creeps. Higher levels and the Aghs version can be used on enemy heroes to "force" them away from you during a fight.]],

[[It is possible to cast $W your own Netherward, to keep it alive some for extra seconds.]],

}

end)

---------------

Go('OD', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {3, W}, {MAX, E}, {MAX, R}, {MAX, Q}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'mantle', {3, 'branches'}, {2, 'tango'}, 'flask' },
    {I_CORE,
        {OPT, 'magic_wand', 'null_talisman', 'null_talisman'},
        'power_treads', 'force_staff', 'mekansm' },
    {I_LUXURY,
        'rod_of_atos', 'sheepstick', 'black_king_bar',
        'shivas_guard', 'hyperstone', 'heart' },
}

Guide.Tips{

[[$H is a glass cannon DPS hero. If he gets good items, his orb adds a pure damage nuke to each attack and his ultimate deals tons of AoE damage.]],

[[The skill build prioritizes survivability (Mek) and building up lots of INT. Don't look down on the mek too much - think of it as a better $vanguard.]],

[[Early game, spamming $W good damage for last hitting and harassing while crippling the enemy. This is particularly dominating in a solo lane and against another INT hero.]],

[[Remember to orbwalk when using Arcane Orb!]],

[[Level 4 $E recovers, on average, 10% of your mana pool per cast.]],

[[Don't be afraid to cast your ult on smart heroes - it heavily drains their mana if it doesn't deal too much damage to them.]],

[[The $ultimate_scepter is not worth the cost. Just get more INT items instead!]],

}

end)

---------------

Go('QoP', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ W, {2, Q}, {MAX, E}, {MAX, W}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        'mantle', {2, 'tango'}, {3, 'branches'} },
    {I_CORE,
        'bottle', 'power_treads',
        {OPT, 'magic_wand', 'null_talisman', 'null_talisman'} },
    {I_LUXURY,
        'sphere', 'ultimate_scepter', 
        'sheepstick', 'travel_boots', 
        'skadi', 'veil_of_discord' },
}

Guide.Tips{

[[$H is fragile and her nukes aren't the strongest in the game. However, the mobility she has with $W more than compensates, allowing her to quickly close in to any escaping or out of position enemy hero.]],

[[The 2 levels of $Q are great for harassing an enemy solo hero in lane. The alternative would would be leaving it at level 1 to max the other skills as sooner.]],

[[Wait until you reach your lane to decide if you want to get $W or $Q at level 1.]],

[[Don't rambo - wait for an ally to initiate before blinking into a group of enemies. Midgame, use your maxed nukes to gank a lot. Late game, farm powerful items to compensate for your declining nuking power.]],

[[The $ultimate_scepter upgraded ultimate has a very short cooldown so feel free to use it even on large creepwaves or lesser ganks]],

}

end)

---------------

Go('Warlock', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, {MAX, Q}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', 'ward_observer', 'mantle', 'branches' },
    {I_CORE,
        'ring_of_basilius', 'power_treads', 'mekansm' },
    {I_LUXURY,
        'necronomicon', 'ultimate_scepter', 'refresher', 'sheepstick' },
}

Guide.Tips{

[[When laning, $H can use his base damage and heal to either babysit or to solo farm. Later on he uses his powerful spells to turn teamfights around,]],

}

end)

---------------

Go('SD', {day=30, month=7, year=2012}, function()

Guide.SkillBuilds{
    {name = "Support skills",
        Q, {MAX, W}, {MAX, Q}, {MAX, E} },
    {name = "Solo mid skills",
        Q, {MAX, E}, {MAX, W}, {MAX, Q} },
}

Guide.ItemBuild{
    {I_STARTING,
        {2, 'mantle'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'magic_wand', 'bottle', 'arcane_boots' },
    {I_LUXURY,
        'force_staff', 'ancient_janggo', 'mekansm', 
        'urn_of_shadows', 'rod_of_atos' },
}

Guide.Tips{

[[$H is a very good harasser, often being picked as a solo mid hero or as a support. His $Q + $W combo is also great for setting up skill shots such as Leshrac's stun, Invoker's Sunstrike and Mirana's arrow.]],

[[Movespeed and mobility items help you stay at a safe distance while still dishing out damage.]],

[[$W and $E can also hit disrupted units. Use this to set up some easy to land combos.]],

[[On teamfights, prioritize $R on BKB carries, enemies with haste or double damage runes, big dispellable buffs (like Brood ultimate) and channelers.]],

[[Remember that you can control disruption illusions and tell them to change targets. Use this to pick off weak supports if you disrupt an enemy carry.]],

}

end)

---------------

Go('Bat', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'ring_of_protection', {4, 'branches'}, {2, 'tango'} },
    {I_CORE,
        'ring_of_basilius', 'bottle', 'boots',
        'urn_of_shadows', 'blink', 'travel_boots' },
    {I_LUXURY,
        'pipe', 'black_king_bar', 'heavens_halberd', 'shivas_guard' },
}

Guide.Tips{

[[$H is a great initiator with his $R: if he has a $blink, he can almost aways pick off one enemy hero before teamfights. He is also a very strong laner with his $Q: if an enemy is ever foolish enough to get 4 or 5 stacks of it, walking over them with $E is an almost guaranteed kill.]],

[[If there is nothing going on you can be use $E to farm multiple (stacked) neutral camps very effectively.]],

[[Use $W to scout before clashes, stop channeling and disable blink daggers.]],

}

end)

---------------

Go('Dazzle', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ E, W, {MAX, Q}, {MAX, E}, {MAX, R}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        'courier', {3, 'branches'}, {2, 'tango'}, {2, 'clarity'} },
    {I_CORE,
        'boots', 'medallion_of_courage', 'mekansm' },
    {I_LUXURY,
        'arcane_boots', 'ultimate_scepter',
        'sheepstick', 'orchid', 'necronomicon' },
}

Guide.Tips{

[[$H is usually played as a support, but his stats also allow him to make good use of gold and levels if those come by.]],

[[Early game, use your good base damage to control the lane. Latter on, you have 3 skills that keep your team alive while your ult turns the enemy into fragile targets over time.]],

[[The skill build is very flexible. Max either the heal or the poison first, depending on how offensive you are feeling and grab one level of grave early on. The item build is very flexible too.]],

[[$E damage is done in an AoE around each bounce. For maximum damage, heal a group of units clustered together around an enemy.]],

[[All of $H's skills deal physical damage. This means they get stronger if you first reduce their armor, with $R of some other method. By the way, don't be picky about using your ultimate, as it has a very low cooldown.]],

[[Poison touch only does its full disable at levels 3 and 4, so geting it to level 3 is a strong priority.]]

}

end)

---------------

Go('Invoker', {day=30, month=7, year=2012}, function()

Guide.SkillBuilds{
    {name = "QW Disabler (recomemended)",
        {3, Q}, {MAX, W}, E, {MAX, Q}, {MAX, E} },
    {name = "EQ Summoner",
        E, Q, W, {MAX, E}, {MAX, Q}, {MAX, W} },
    {name = "EW DPS",
        E, Q, W, {MAX, E}, {MAX, W}, {MAX, Q} }
}

Guide.Tip([[Key QW Abilities: EMP, Tornado, Cold Snap, Deafening Blast, Ice Wall]])

Guide.Tip([[Key EQ Abilities: Forge Spirits, Ice Wall, Cold Snap]])

Guide.Tip([[Key EW Abilities: Alacrity, Meteor, Sun Strike, Deafening Blast]])

Guide.ItemBuild{
    {I_STARTING,
        'blades_of_attack', 'branches', 'tango'},
    {I_CORE,
        'phase_boots', 'ancient_janggo', 'force_staff' },
    {I_LUXURY,
        'ultimate_scepter', 'sheepstick', 'orchid', 'skadi' },
}

Guide.Tips{

[[Well.. this hero is insanely flexible and can be played as some combination of nuker, disabler,pusher or carry. Personally, I think his disables (especially ice wall, emp and deafening blast) are his most powerful asset, but it's crucial to be flexible and experienced enough to use all of your spells.]],

[[I highly suggest you read more about Invoker than this alt+tab guide.]],

}

end)

---------------

Go('Visage', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, W}, Q, {MAX, E}, {MAX, Q} }

Guide.ItemBuild{
    {I_STARTING,
        {2 , 'gauntlets'}, {2, 'branches'}, 'flask', 'tango' },
    {I_CORE,
        'ring_of_basilius', 'arcane_boots', 'mekansm', 'urn_of_shadows' },
    {I_LUXURY,
        'force_staff', 'vladmir', 'hood_of_defiance',
        'ghost', 'shivas_guard', 'sheepstick' },
}

Guide.Tips{

[[Visage loves hanging around in the middle of a teamfight, spamming 300+ damage Soul Assumptions. For this reason, he is also one of the scariest trilane heroes, althogh he can also make do with fast familiars from solo-mid exp.]],

[[Try to get the most use of your familiars; scout the runes, use their stun and don't feed them (they have a 100 gold bounty!).]],

[[$Q also steals attack speed, so target the enemy carry during clashes.]],

}

end)

---------------

Go('Lesh', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, E}, {MAX, R} }

Guide.ItemBuild{
    {I_STARTING,
        {2, 'circlet'}, 'branches', 'flask', 'tango' },
    {I_CORE,
        'magic_wand', {OPT, 'bracer', 'bracer', 'null_talisman'},
        'arcane_boots', 'bloodstone' },
    {I_LUXURY,
        'ancient_janggo', 'ultimate_scepter', 'black_king_bar',
        'travel_boots', 'shivas_guard', 'heart' },
}

Guide.Tips{

[[$H is an AoE damage monster and an insane tower destroyer with $W (lvl 4 can take up to ~75% of a tower's HP!). However, he desperately needs health, mana pool and mana regeneration from items in order to be fully effective.]],

[[$H has a flexible skill build. An alternate one to kee[ in mind is getting $E first instead of $W early, so you don't have to rely on allies to clear the waves when you push.]],

[[$H is very mana hungry. Because of this most builds delay his ultimate and initially focus on just two skills. A reason to max $Q soon is that the AoE increases and gets easier to land.]],

[[Some people like getting $phase_boots for lots of mobility (since positioning is so critical on this hero).]],

tip_ab_disassemble(),

}

end)

---------------

Go('WD', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, E}, {MAX, W} }

Guide.ItemBuild{
    {I_STARTING,
        {4, 'branches'}, {2, 'tango'}, {2, 'clarity'} },
    {I_CORE,
        'boots', 'magic_wand', 'urn_of_shadows', 'point_booster' },
    {I_LUXURY,
        'medallion_of_courage', 'ultimate_scepter', 'black_king_bar',
        'force_staff', 'blink', 'sheepstick' },
}

Guide.Tips{

[[There are two main styles of playing $H. In the defensive style, you use his cheap heal to protect his fellow laners and his long-range stun to aid in skirmishes from a safe distance. In the offensive style, you get levels of $W to greatly amplify the damage dealt to the targets of his ganks.]],

[[You can manually select and control your $R. Use it to pick the best targets for it!]],

[[Higher levels of $E heal a lot and can greatly aid in pushing but have a very large mana cost.]],

[[$invis_sword can render you invisible without canceling the channelling on $R. However, this only matters if you get tons of farm in a pub and BKB is often a more solid choice to protect your channelling.]]

}

end)

---------------

Go('AA', {day=30, month=7, year=2012}, function()

Guide.SkillBuild{ {MAX, Q}, {MAX, W}, {MAX, S}, {MAX, E} }

Guide.ItemBuild{
    {I_STARTING,
        {3, 'branches'}, {2, 'tango'}, {2, 'clarity'}, 'flask' },
    {I_CORE,
        'boots', 'magic_wand', 'mekansm', 'force_staff', 'urn_of_shadows' },
    {I_LUXURY,
        'ancient_janggo', 'travel_boots', 'sheepstick', 'cyclone' },
}

Guide.Tips{

[[$H's strongest point is his ultimate. Not only is it a great counter to healing based strategies, its global range makes it a great for ganking and counterpushing. He is also a reasonable solo mid hero with $Q.]],

[[It is possible to use $W to scout fogged areas before entering them.]],

[[$R works like this: You send an invisible marker moving in the target direction. When the marker is where you want it to be, cast the skill again and the real ultimate will travel, spreading the chilling buff where it passes and exploding for damage when it finally reaches the marker.]],

}

end)

return Guides
