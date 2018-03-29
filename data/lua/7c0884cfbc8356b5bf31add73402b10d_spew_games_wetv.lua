
data.gametypes=data.gametypes or {}

local gtab=data.gametypes["wetv"] or {}
data.gametypes["wetv"]=gtab -- update, keep unique gtab table
data.gametypes["tv"]=gtab -- shorter name

gtab.name="wetv"

local playlists={
	blips=  {cat="blips", id="PL30Cmb00CTy1UheoL5mtYsvLoAspO2PaQ"},
	music=  {cat="music", id="PL30Cmb00CTy0ZNLzv6Xtou4Zr-nUJally"},
	horror1={cat="horror",id="PL9D3B08D539EDDCCA"},
	horror2={cat="horror",id="PL129A76A710B5C5C9"},
	comedy1={cat="comedy",id="PL940038B8C37BC2BF"},
	comedy2={cat="comedy",id="PL40847C47E99D785B"},
	movies1={cat="movies",id="PLD3363FF38E2801F2"},
	movies2={cat="movies",id="PLEE54950D026DAC24"},
	movies4={cat="movies",id="PL4CC4A2717485647E"},
	movies5={cat="movies",id="PLF435D6FFBD0302B3"},
}

local noir_cats={
--[[

		horror="http://www.youtube.com/channel/SBkqDlG0PHTIA/videos?flow=grid&view=11",
		scifi="http://www.youtube.com/channel/SBpDoh20iB9Ck/videos?flow=grid&view=11",
		comedy="http://www.youtube.com/channel/SBNsxfepiAWGo/videos?flow=grid&view=11",
		classic="http://www.youtube.com/channel/SB3w5c6v0vLX0/videos?flow=grid&view=11",
		toons="http://www.youtube.com/channel/SBTv6ixmnVooo/videos?flow=grid&view=11",
]]

--		uk="http://www.youtube.com/user/YouTubeMoviesGB/videos?flow=grid&view=26",
--[[
		horror="horror",
		scifi="science-fiction",
		action="action-adventure",
		comedy="comedy",
		classic="classics",
		mystery="mystery-suspense",
		toons="animation-cartoons",
		crime="crime",
		docu="documentary",
		drama="drama",
		family="family",
		romance="romance",
		foreign="foreign-film",
		new="",
]]
}


local tv_triggers={}
local tv_trigger=nil

-----------------------------------------------------------------------------
--
-- say something
--
-----------------------------------------------------------------------------
local function tv_say(game,s,user)
	if not game or not game.room or not game.room.brain or game.tv_hush then return end
	local msg={cmd="say",frm=game.room.brain.user.name,txt=s}
	if user then msg.blame=user.name end
	roomqueue(game.room,msg,game.room.brain.user)
end

local function tv_act(game,s,user)
	if not game or not game.room or not game.room.brain or game.tv_hush then return end
	local msg={cmd="act",frm=game.room.brain.user.name,txt=s}
	if user then msg.blame=user.name end
	roomqueue(game.room,msg,game.room.brain.user)
end

local function tv_say_link(game,s,link,user)
	if not game or not game.room or not game.room.brain or game.tv_hush then return end
	local msg={cmd="lnk",frm=game.room.brain.user.name,txt=s,lnk=link}
	if user then msg.blame=user.name end
	roomqueue(game.room,msg,game.room.brain.user)
end

local function tv_ret(user,s) -- tell something to one user only in response to a command

	userqueue(user,{cmd="note",note="notice",arg1=s})
	return true
end


local function is_tv_admin(game,name)

	return ( is_admin(name) or is_room_owner(game.room,name) )

end


local vid_blip_ids=
{
"SQpqq0SCYDk", -- Poly+Morfs:000
"HU82vMmAz4o", -- Poly+Morfs:002 : assfro
"-YSeUbg4SiI", -- Poly+Morfs:003 : tasty mcluvin
"FAlHE1lJ5fs", -- Polly+Morfs : 004 : Hungry Pussy
"fdO4ea3oP8Q", -- Polly+Morfs : 005 : Happy new meh.

"M2YKEq3zj2g", -- blip : big time tv 1
"3tK7xXaqXo4", -- blip : big time tv 2
"-xR6EGsdXkA", -- blip : big time tv 3
"5hBG7JT7Hy4", -- blip : big time tv 4

"U9CvUCCLJi0", -- blip : geronimo

"V8CZmAYu_G0", -- blip : good free 1
"bIj1VX55ak8", -- blip : good free 2

"DNot2PxnBZI", -- blip : nice to have you visit

"yH3xbCvheJE", -- blip : what?

"hIkC3Doa4fY", -- blip : Im gonna put the wheels on the bus back on

"zY0GAH8KJNs", -- blip : ello mate

"iRQIS4Xapx0", -- blip : max1
"GGGnKEddB_4", -- blip : max2
"BmyBIq60DoY", -- blip : max7

--"J7f9zWBnaBM", -- blip : vert

"GoefEAtidPs", -- blip : bollox
"HNM2H0YDwOQ", -- blip : buttox
"0XT2_aau71Y", -- blip : you want some?

"SmI9-fQKOIA", -- blip : mad

--"QoPWALM2riQ", -- blip YEAAAAAAAAH!

"1kOo9ZPDfaA", -- blip : entertained.
"Z-UFSOzOatk", -- blip : mentlegen.
"zTpYDg4FiJY", -- blip : sit quietly.
"kiFU_vq5dRM", -- blip : we are controlling transmission.
"tc5BmKcvI4Y", -- blip : there is nothing wrong with your television set.
"Wk4WmyObvvQ", -- blip : unlock this door.

"DXVLO9Dwa3o", -- blip : born looooooooooooooser. -requested by fish and Jshaw995 for giggles
"iSG7eUetEto", -- blip : my name is travis and i am putting on the ritz.
"ozW4mmKzcYQ", -- blip : twilight zone troll
"VroBOVAl2uE", -- blip : wetv in the 80s
"mfB17mZBMg8", -- blip : YOU'RE TEARING ME APART, LISA.
"nHj0dtSwxqQ", -- blip : lol Impossible
"-CbN3eHYn6I", -- blip : shieeeeeeeeeeeeeeeeeeet lol - oh bitch, u
"CU4WtOeQyn4", -- blip : tighten up the graphics


"WiBYFkABr7Y", -- blip : red dwarf alphabetipesgheti - requested by Athens
"qBW2z0NZ5zA", -- blip : arrested dev return from whence you came - requested by dizzy.
--"F0Nlatc-XIg", -- blip : dramatic perfume 444 - yukarin
--"0aCNJWeOiF4", -- blip : sugoi - paramedic

--"rUhOpCaQ9AU", -- blip : koffi! - butch
"x9gN2hdybFY", -- blip : i'm a bad troll - butch
--"4qeC6VKZblI", -- blip : ish goose, omgstn - butch
"N3iBN_Q7TuU", -- blip : kiss me, fat ghost - butch
"hMTJr4WI7uY", -- blip : nice tits, butch - butch
"cmYkUEnIzYY", -- blip : pastaaa~ - butch
"lHjpaqv7CQw", -- blip : vodkaa~ - butch
--"7hDqPmrhGa4", -- blip : chen is gay - butch
--"oOogTp-A5zM", -- blip : stop judging me! - butch
--"atcFIFrqfZc", -- blip : miku has something to say - butch
"E2Dvmj9K_og", -- blip : you're pathetic - kam
"IxmqKOSBOMo", -- blip : you fools - kam
"SHE-kXUqRKk", -- blip : video in 3 words - kam
--"fuoUm7il4tM", -- blip : you have no taste but we like you - kohaku
"UEk-m4hZCV8", -- blip : batman tas - kohaku
"UgOUiRavhIw", -- blip : today's quote - kohaku
"Fm2MskcNMXk", -- blip : potato chip - big_al
"r38zbDZ92eI", -- blip : nope - big_al
"82ZmA-CyBE0", -- blip : beeer - big_al
"-i96R7HhVdg", -- blip : obsessed with myspace - desu_boku


"AkwvguO0pSU", -- blipvert : ZOMBIE SEE ZOMBIE DO - Bernie Drummond
"SqB7zqZkQLI", -- blipvert : BRAVE NEW WORLD ORDER - Bernie Drummond
"JnL3G4zMxJo", -- blipvert : ONCE I WAS DEAD - Bernie Drummond
"LQWe6p1kXQo", -- blipvert : EGO - Bernie Drummond
"pW5mTPxhg_w", -- blipvert : HOW TO SERVE HUMANS - Bernie Drummond
"pGPKYqQRJmk", -- blipvert : ALL MESSED UP - Bernie Drummond
"V8vmjy-4p1Y", -- blipvert : they're coming - Bernie Drummond
"zJcH3QxnwFc", -- blipvert : XENOPHOBE - Bernie Drummond
"S-7AaKOQmoU", -- blipvert : Time - Bernie Drummond
"uop-BCUYu-8", -- blipvert : Like a clown - Bernie Drummond
"NUHVSISooAM", -- blipvert : TWELVE - Bernie Drummond
"DgQCN1_MSG0", -- blipvert : RUN RAT RUN - Bernie Drummond
"X_BWSqEWPIc", -- blipvert : there's someything about Alice - Bernie Drummond
"H52_208ATxQ", -- blipvert : CLANS PEOPLE - Bernie Drummond
"s5CMl1ceeho", -- blipvert : MOTHER - Bernie Drummond

"LmIBBUSrjw4", -- blipuser : we tv
}

-- online blips playlist
-- https://gdata.youtube.com/feeds/api/playlists/PL30Cmb00CTy1UheoL5mtYsvLoAspO2PaQ?alt=jsonc&v=2&max-results=50&start-index=1
-- https://gdata.youtube.com/feeds/api/playlists/PL30Cmb00CTy1UheoL5mtYsvLoAspO2PaQ?alt=jsonc&v=2&max-results=50&start-index=51
-- https://gdata.youtube.com/feeds/api/playlists/PL30Cmb00CTy1UheoL5mtYsvLoAspO2PaQ?alt=jsonc&v=2&max-results=50&start-index=101
-- ...


--"tv add max 6uGD5Lf3YXU ftkmoA-HVg0 ZRuV4_4ahjA w6yt0db89O4 rysRLBQtDE0 6MDz2r_Quko"

for i,v in ipairs(vid_blip_ids) do vid_blip_ids[v]=i end


local vid_ids=
{
"g82A7F39JqE", -- Little miss irony - Hope
"1AUhtoJXTTs", -- Little miss irony - No way to apologise
"ttmLeE0R-jU", -- Little miss irony- du reich so gut (Rammstein cover)


--"SWmwDy6wr8Y", -- HIM -  In Joy and Sorrow
--"wzVuVtq2bJs", -- HIM - Buried Alive by Love
--"LU2hw4HlZWI", -- HIM - The Sacrament
--"ohrvYiJmvko", -- HIM - And Love said No
--"109B0DYLa7E", -- HIM - The Funeral of Hearts
--"5_9bNx3Kyc8", -- HIM - Gone With The Sin
--"WD_2ebTYZNw", -- HIM - Wings of a Butterfly
--"StrJdI5cfUk", -- HIM - Right here in my Arms
--"bUQeU9Ezrhw", -- HIM - Join me in Death
--"23eZMdixAuk", -- APC - Judith
"tWFv5ZoVET8", -- APC - 3 Libras
--"Qn0G0NPmKpo", -- APC - The Outsider
--"OiNczmhgBZE", -- APC - Weak and Powerless
"0aKBuEt-xj4", -- Morning Musume
--"i4Zaqzb5if0", -- Morning Musume - Ai Araba
"Yz7TD9FpRTw", -- Mini Moni - Terephon Ring Ring
"QFFo7-huDko", -- Mini Moni - Okashi Tsukutte Okasui
--"tzOWMGfpMXM", -- Namie Amuro - Suite Chic
--"D_Klhcr4O1M", -- Utada Hikaru - Colours
--"2U0myXslSng", -- Utada Hikaru - Travelling
--"6SQD4yDHaJ0", -- Shina Ringo - Keikoku
--"VEnM9Y8h7PM", -- Shina Ringo - Shounan
--"KG3hVYGjjGg", -- Shina Ringo - Yami ni Furu Ame
--"2SdQGYHOFu8", -- Marilyn Manson - The Nobodies
--"KAg0kzKJJEs", -- MM - This is the new shit
--"3iuve2OjY_8", -- MM -Sweet Dreams
--"lykzEEAohJY", -- MM -Tainted Love
--"Q3G2BQVrcOs", -- MM - Personal Jebus
--"JFq2YJKYa-k", -- Amon Amarth - Runes to my memory
--"Wu8LpalUelY", -- Tricky -Black Steel
--"jiwmdhNuyqo", -- Tricky - Makes me Wanna die
--"6V26zxH_JMk", -- Tricky - Overcome
"aVjgRlto8PI", -- Krush -Kemuri
"vwgbe9gK6Zk", -- Krush - Journey of Time
"tOeL-BEgGFw", -- Krush - Only the strong Survive
--"eTs0lDbeB_k", -- Krush - Sun is shining
--"Jw8tL8PeD1Y", -- Shadow - Midnight in a Perfect World
--"bDhsn3jwHFc", -- Type O - Love you to death
--"8bvgy08jPf4", -- Ministry - Land of rape and honey
--"K0dfd_L4tDk", -- Joy Division - Love will tear us apart
--"d6PGzC6_WMw", -- Aphex Twin - Window Licker
"5Az_7U0-cK0", -- Aphex Twin - Come to Daddy
--"3kbW4ibIF8U", -- Prodigy -  Breathe
--"28ow4TLMTqM", -- Prodigy - Firestarter
--"1ZlLxYnDlYk", -- Prodigy - Baby's got a temper
--"FDO--nCQE_g", -- Prodigy - Spitfire
"-Fz85FE0KtQ", -- Prodigy - Voodoo People
--"jQxgvDn0Iko", -- Prodigy - Smack my bitch up
--"QWncI51ggH8", -- NIN - Closer
--"kk7SKP4PJ2w", -- NIN -Hand that feeds
--"l0s5UOVsMDg", -- NIN - The pErfect Drug
--"pOBV2Zl2cNs", -- NIN - Dead Souls
"DMZnaUMepFw", -- Cure - Burn
--"lyCAhpNJYA0", -- MM - Long hard roal outta Hell
"LOK2dWcGuDI", -- Brother Machine - Driving
"o4VMDxSyLAU", -- Laibach - Final Countdown
"X6aimibFW3U", -- Revolting Cocks - Do Ya Think I'm Sexy.
--"e01C3AqzjlE",
"mhXOrCoSnLs", -- Negativland - Time Zones, Miguel Soares, 2003
"UK-IOzJjm-Y", --Corporate Avenger - Fault The Police Music Video
--"c-iYsLRa7tg", -- Make Money Make More Money - El Queso Allstars
"Qt9MP70ODNw", -- SubGenius Commercial
"74XranJcH6s", -- Alec Empire. " Addicted to you "
"tqWP1rsAMrw", -- Rage Against the Machine - Renegades of Funk
"4fMqJWoOjE4", -- Rancid - Time Bomb
"SmVAWKfJ4Go", -- johnny cash - hurt
--"ZyHJ_EnmDmk", -- new kingdom - cheap thrills
"-1CE4P8qqPE", -- clawfinger - do what I say
"_VJFSz5p5Ao", -- 64revolt - hurricane
"FBxJqcX8ErU", -- rabbit junk - In Your Head No One Can Hear You Scream
"PJQVlVHsFF8", -- David Hasselhoff - Hooked on a Feeling
--"EsZYqaSc4cU", -- Tonight, Tonight - The Smashing Pumpkins
--"dxNX_PRqhCQ", -- The Smashing Pumpkins - Bullet With Butterfly Wings
--"RHUd896Sur0", -- the smashing pumpkins - Today
--"wrivjzw0RlI", -- Smashing Pumpkins 1979 video
"QQtLoJlQD6E", -- Smashing Pumpkins Disarm video
--"Jdgj0cSgPLo", -- Smashing Pumpkins - Rocket
--"xJOGq5XTojo", -- The Smashing Pumkins "Zero"
--"qiSkyEyBczU", -- Soundgarden - Black Hole Sun
--"nxpblnsJEWM", -- Radiohead - Creep
--"sUUHNf0S5cA", -- Nirvana- Lithium
--"wY3oEvaq71A", -- Nirvana-In Bloom
--"SK7Ai9dWrRQ", -- Nirvana - Heart Shaped Box
--"ncl7New1czM", -- ladytron-seventeen
"sGcdcVblZ-8", -- Pitchshifter - Genius
--"bOL5cpwTkes", -- Nirvana - Come As You Are
--"kPQR-OsH0RQ", -- nirvana - smells like teen spirit
"U8BWBn26bX0", -- The Avalanches - Frontier Psychiatrist (Good Quality)
--"wouKI_myXxk", -- Air - Sexy Boy video
--"LwC1swvlBPs", -- Air - Kelly watch the stars video
--"NAgX1jO3No0", -- Air - Playground Love video
--"dr4fP9A3Oi0", -- Air - Surfing on a Rocket video
--"pcJwz7wu8_s", -- Tenacious D - Tribute
--"lHY5L47tcHk", -- Tenacious D - wonder boy
"ygN8H3kI1qE", -- GLC - Half Man Half Machine
"5SAYlRf_6bQ", -- Goldie Lookin' Chain - Your Missus Is A Nutter
--"xv-2XYOtgCg", -- Goldie Lookin Chain - Guns don't kill people
"botnsA3KOWI", -- Goldie Lookin Chain - You knows I love you
"wAZTLVJSlNw", -- your mothers got a penis
"hkb3r9filcM", -- Atari Teenage Riot - Revolution Action (Banned Version)
"8SIwUOII7hg", -- Atari Teenage Riot - Kids Are United
"QenJDBmxi1E", -- Body Count's In the House
"pfFD709OtFM", -- Onyx ft. Biohazard - Judgement Night
"YfTg4Fcza58", -- Faith No More/Boo-Ya Tribe - Another Body Murdered
--"2kIZeVoRBuU", -- The Offspring - Self Esteem
--"f7-E1qTVJgE", -- The Offspring - Pretty Fly (For A White Guy)
--"yJQFf0qj9Nk", -- The Offspring - Come Out and Play (Keep 'Em Separated)
"AMnLIJSY1vY", -- Mad Capsule Markets - Pulse
"IGVJnir5sXE", -- All The Time In Sunny Beach - The Mad Capsule Markets
"E2oEoOQ5yWQ", -- The Mad Capsule Markets - SCARY
"mg-hh7XGw78", -- The Mad Capsule Markets - Midi Surf
"kfy2MbVqrWI", -- The Mad Capsule Markets - Systematic
"88slxSc5N8Q", -- The Mad Capsule Markets - good girl
--"uhSYbRiYwTY", -- David Bowie's "Space Oddity"
"slKNd22GGaQ", -- David Bowie (feat Nine Inch Nails) - I'm Afraid Of Americans
--"Rqs88oCYznI", -- Starfuckers inc. - Nine Inch Nails
--"irp8CNj9qBI", -- Queen - Bohemian Rhapsody
"3XwLxNT8svk", -- Stephen Malkmus -- Jojos jacket
--"LXpbrGBIGxw", -- White Zombie - More Human Than Human
--"N1MQW5e6KcE", -- Rob Zombie - Dragula
--"Ezcoy-Lz8p4", -- Rob Zombie - Living Dead Girl
--"b5HCFRhtmgc", -- Rob Zombie - Superbeast
--"kBteY3qaBSw", -- Rob Zombie - Return Of The Phantom Stranger
--"bJ9RrHMTTbw", -- White Zombie - Black Sunshine
--"fF7fRwVZQsE", -- Filter "Hey Man, Nice Shot"
--"1FV8TVe_JN8", -- Filter trip like i do
"fxrd_jZJxkg", -- henry liar
--"RzIQPBVQm_k", -- you suck
--"d_3g4QPojMc", -- how not to interview henry
"6Afjo0ICoT0", -- old manga trailer

"eBShN8qT4lk", -- (You Gotta) Fight For Your Right (To Party) (Which sux, muchly)


}


gtab.vid_ids=gtab.vid_ids or vid_ids -- keep in gtab

gtab.movie_ids=gtab.movie_ids or {}
gtab.playlists=gtab.playlists or {}
for i,v in pairs(playlists) do
	gtab.playlists[v.cat]=gtab.playlists[v.cat] or {} -- perma tables for playlists cats?
	gtab.movie_ids[v.cat]=gtab.movie_ids[v.cat] or {} -- need a perma table for each
	noir_cats[v.cat]=true -- remember list so we can add special tv rooms
end

local tv_add_lines=
{
--[[
"tv add ted ted32 ted37 ted33 ted34 ted27 ted28 ted29 ted210 ted211 ted2x",

"tv add ted34 X73kfAA8mn8 IsGKAFHwp9g kayN4YglIyI Hrj4qWg6Cko",
"tv add ted33 Ur6M4ANv_m4 UPjdhVJeJhI yWuSZVgrF98 KjZmXJLM1Zw",
"tv add ted37 hyxB5-MQjLU jxMMP1_2al0 f8mbGBq1GBY",
"tv add ted32 IdMlfjqZeOA ZmhT5aegUgg c715w6jybAE 5JLLB3fMqEw",

"tv add ted27 KMVTMuxw6QE fhJU4bxncQc zhkZDKR8Zx0 C6-vJQSXlrk",
"tv add ted28 NRNjqfG7HKw -kqFJdA-9Zo p0rziDkWbpg",
"tv add ted29 AaHysH2hODs Ct1MTiS5Jeo cajA1EhCgHI",
"tv add ted210 s3CNz6_hXr0 WAObl18bqKs vB4fm-3LuYE",
"tv add ted211 4Lo5dwzZToo -vO_AF2_jac qmal6xeAsBc",
"tv add ted2X YZolJrVHGpg MMD74e1wlAQ K78OqpZz6eI ZKus2cmyTDQ widlHgAiARo wD3J4Xx4Lfw",
]]

"tv add detour 9QpK0pwMOso qdQw9NHBtdw AKcbkhRpn4M ndFIQy1P9Dk Do08kzqxJMA swmVY5ZIikY UaX16_9BPzA",
"tv add sherlock-advent tkYzXQPEfv4 mRSH-krTPAw QC--I79-dkk A6nfwpxYMro aJFV6C8CcFo MJ4S9VsgH1w oTJZp0OXMm8 Fdzgz4UearQ HqXfZmPJhy8",
--"tv add sherlock-house WTNnrLEj33c 6uP2I3Vjs6I YaqOCsHeUF4 K4grN7VSIDQ haDZxYkDcoo Z2GiZMehTL4 sQhuGHTQBGo",
"tv add sherlock-claw KO6vvZCwnqo 5PtPCq_MiB8 aw89oOdECmc pk1pM-MClsM _TX8VLk07po cdtr7Zr9JmE G-2zsTHbqlQ 7btrer07v1I",
"tv add sherlock-pearl NxW6ctVcjuE QEVidE_sB7g YDXxZMznBes Y40ia1jF3MU Z-ecBkp6_TA pNJfQTAA-DA x8W9HoI8XnE ",
"tv add sherlock-voice VLq5f5B9AYE gQZCrqSTp2o nLvL2Ro4ZOs JsyU13zddPk 9s5De-IJM68 Uyxc8SYCRgs GhkEFVZC7zo",
--"tv add sherlock-spider odwq2u3vDN0 LKKHB3_Hu8k rw-ibQnxbmU KzbY_4YkKJ0 0pYI9HdbX6Y csKoPbvHh24 7RoKrWZ16_0",
"tv add sherlock-green C9mKB85q7eM AlwHdPA3t_o 32X6_zeFhlM D7d0afqUNmI SUX6HCk2b78 8HwF83dkEXY NkL36kPEVrQ",

"tv add lastman pSsvJ6w1GLk RtXPxo-67jc k_2u4H_xpns 9xelTBUjgiA JqfsuraLV-E HCXazbZOJeo JXZoNhclKmU THwgab2nPtk GQjzv_K3Ie8",
"tv add sherlock-neurotic atthYOqTVj4 QYSVUH_a5H8 nQNA2Jl9XnE",
"tv add doa t8-08oqiFcc z4ifx8iGQrY NrGCToze3N4 iRko8iPn2M8 ojXlKWnVtNg xXflfOJyNrw Usx1hszzInc AfC3wPYmn0o",
"tv add dick-gruesome nJ426aHvNmQ jIRwSjdz2pw wnwISa1tR_A IEcvkHS-65g hiFy-h5ClpU FGeqMlwQBl8",
--"tv add wonderful-life oqDiSpPc6r4 -mBVTbAxwO4 5zdAzn7e5Pg sdjUA5v5cMw 6j7Usm-_XLo gT4B4XfpNEo XeaeDYp5EJo BBWEh7LRgak DQPB8o-gY7I dQlU1nHZsrg IW7yTO4eDkI O5-HmDRlYRY pA9dSy7ihdY",
"tv add arsenic-lace NSBHIC-P32M CpWdbmuIDxM aOetzC6mBSY yBIVMGloBT0 YfOdqlSHIW0 Sods1_qlly4 TijuLrTfE1w ja3Q7z0xX9g qJ1y7Xebk8w 6yRxPadOb40 AufR7bqCQgI NdjOysUb1JU T1lgBjfBgUQ",
"tv add whitesuit XNFnUo7cgVw yT5N5ciRn0Y v-V6sALIBF4 CgiVzfF0WWw mGAcUFQhtVA 5oqLllTMtpw OEBZXhkNt2k Skw_KZfx0MI rd4hSs0n-HQ Bl4XqEl13rs 5qi0pHkEjp0 5QGRzLAw_ME ObfEJLj_aCY T5jVQT1Gos0 W_n12eyrC4Y qM_lF1keHNs cIYb4aj0VA4",
"tv add quatermass1 c9A-fR1lSzE 5mIhyA0k7AU foQASJDT9I8 S4w-vX_NmtA BUImUCvK_C8 PjMgJWvHr_E R9QN_pMl-dw l-e4jT1arD0 msIj_kPQ2tM",
"tv add quatermass2 ec1mh8HkEv8 CZ90cnf3IHo H_XpS6M8JE8 IO2Gw2hmHRQ ARI2gShnytE XMCsHlpSczY ousoERRs220 WA8Ccwsapkk jtbpNYjon78",
"tv add lifedeath KsXRxDQptvs Y9bjw6gGkoc 1MBg6UFAWKo IwKSH4tABOo 1v4_ck83_z0 OLFez3SEvW8 gltZZFH9OoA K_Z0BN_chE4 H9rzUUQT6-M qc83iOXszd4 lfMCiIzMQoM 0KdWfUXj-OM xwes2eQUmfI FkOpnZ5hbvI uwez1kqYB34",
"tv add quatermass3 fGfrYN1WGDk rAxo8o5sF-w uYaYn2m8780 VS_U7vC8ZoY QSkFqC87KTA bsmWfQXQdRY JCiqbz6Rr8Q QGSErPFLMLI BLh_6NJi4Jc A3j7RPKIOl8",
"tv add thething McLI4HSj6Jw emoe17H18bU czfbPHIv5eM u8Rq_DeNUTI XMr9zLT4BcI oDWGZsIQqDk vaVjeDS-1aY BBzP7TbT4mE xUMJS_CJEcA",
"tv add nightdemon PJGXvqyxZI0 ywrZE2tA8ZY kKTjanmmve0 r4huoziAuY0 xGnZjzx64_c lnj_zn5jJ64 vKdhqd7ti_w oO0xjyMf1a0 w8OKNba9sLs Y5K_EGiQ5Js",

--"tv add rebecca sYcRxW8wJPM wcLOK2musJM CSzIOxyxw5A OFh07zaidns ztuv8Cez4aE RsynXUtteOg rLFTOLezdFg -XamFfBWAQ0 wpW_5TqD0s4 fcZ0OCvp4FM rIBHmABkXgU MgjzfbEnD8s wGgJ4EUrFqA BKsCpeUuN6A xDBScQP1heU ",

"tv add still IxfnuaiXcCU RXWVI_VGK1o -mqVoDt56ic GOTnMuHnJ5w ZlBW57gmULo y4PTXOYQ-IA 0RCMq_djYfc WwoaXzZlvCI TYc1tPGZvCI",


"tv add noir lastman sherlock-neurotic doa dick-gruesome arsenic-lace whitesuit quatermass1 quatermass2 quatermass3 thething nightdemon detour sherlock-claw sherlock-green hancock-12 hancock-alpine hancock-economy quicksand smallest 4lfa still",

"tv add hancock hancock-12 hancock-alpine hancock-economy",
"tv add hancock-12 qvMaS2urmnQ jFDvaHALWAM P-91wdjBWM4 ",
"tv add hancock-alpine 2_dCktZswXA MfTin539GCQ AXSFIzak2iA ",
"tv add hancock-economy lBwqD1mTKK4 e6LTeFu7Ems fMmKoQjAV2Y ",

"tv add quicksand 0Cq5oteOV4w ybSn4fXrR_8 FvVavAAS6us MID_jA9v6DQ tBtMyPmMBJ0 G9ZqyZh5LwE g3bfcKlODHw K0GHZjbNsXI ",
"tv add smallest 8PbinwWBzDI zkkTFlVbWDg oXuHd3UHwuw Nm4Pynnm42g hyhLfFUv8d8 INUhoMYg_cg Ma3snGVX2KQ xo5x__09pCw ",
"tv add coffee BKQZQ4vwCqQ H2NgPmmxt1w ",
"tv add 4lfa 9RxyUQATeIA uOb72gAGsVg eiClVFIduK4 TjIU2NTSJBk 02jQJ8Nysf4 obYDsyVg16A lRG9eM86fTw kr0N2-Th-cE H4_Xeh4uUTk 2OhMhsyJirc",

"tv add devil_rides MjiesKYB758 kbhZryosdJ0 Qi0HD4hBRQg Nk-8zZeFVso oVhkLYiyuz4 QfFriXqdxDU 6w4r02vVrck 4AECJSwsuwE O2B7eBYM-d8 Klbgy1r7_Tk",
"tv add vampire_circus KWTgSoCJohs u9Rw9rrCcY0 L3niTvVRXHk BPGl8whfwPQ _Uat8l9PolM c5LMHMz_rfQ PihvHj9R_Jg QMcJ73BEtLs 3vD8rw2HxK0",
"tv add dracula_risen cdZf2xRXsMs KLhqDQ_5lWE 5PjM1P4OtsY eaq2J89IGRc WW0rBjpjYtY cHkOvswTTHM lZXltYzIw7M je_-PJSz0wM INGjk-ByIVs",
"tv add nosferatu MEOsb6CRvNU",
"tv add usher UV4pho5SltU",
"tv add horror_doc cniEdq4Jwaw wu0e6T8UjaU 6PdNy9oaa5g FhqiZsCMis8 giPp4bHaDlU DV965qFNiFQ qqHoUgBFiwE SZBjHq2zGHw",
"tv add childs_play hI5FjSn3qRw xlIq5X1gAAw zwvEag_ug2s SLNIafBIFL4 1bN-If6UmTU 36GRojPUdWI Muoq-vVdmYI 1lOpvpiQP_g",

"tv add ween lastman nosferatu usher devil_rides vampire_circus dracula_risen horror_doc childs_play",

"tv add 4chan F8gji6hdN-c kA16PNZ8-2g Ar4WzQ7KHak 24yFyybu3ig 2NNMHxqJp6A KRMxuCFjtKs 128IR21ZQa0 kYxlSDJZbn0 I94jZTjl_nI ssgm5rUpgUs",

}


local function get_random_vid_id(game)

	if game.vid_ids and #game.vid_ids>0 then -- play from a special list
		
		if #game.vid_ids<1 then -- list is fucked
		
			game.vid_ids=nil 
			
		else

			local idx=math.random(#game.vid_ids)
			local id=game.vid_ids[ idx ]

			if gtab.vid_infos and gtab.vid_infos[ id ] then -- we got good cached info from youtube
			
				local vi=gtab.vid_infos[ id ]
				if vi.duration==0 then
					table.remove(game.vid_ids,idx)
				else
					return id
				end
			end
			
		end
	end

local vi=game.vidinfo

	if vid_blip_ids[ vi.vid_id ] then -- alternate

		if game.vidlists and game.vidlists.random then -- room local random override

			local id=game.vidlists.random[ math.random(#game.vidlists.random) ]
			
			if id==vi.vid_id then -- avoid any loops
				return vid_blip_ids[ math.random(#vid_blip_ids) ]
			else
				return id
			end
			
		else
		
			return gtab.vid_ids[ math.random(#gtab.vid_ids) ]
			
		end
	else
	
		return vid_blip_ids[ math.random(#vid_blip_ids) ]
		
	end
end


-----------------------------------------------------------------------------
--
-- does this user have a tv visible
--
-----------------------------------------------------------------------------
local function tv_inroom(game,user)

local gnam

	if not user or not user.client or not user.room then return false end -- logged out?
	

	if user.this_game_name then -- last played version
	
		gnam=user.this_game_name
		
	else
	
		gnam=user.gamename
	
	end
	
	if gnam and string.lower(gnam)=="wetville" then
	
		if user.room.ville_game then
			gnam=user.room.ville_game.name
		else
			local aa=str_split(".",user.room.name)
			if aa[2]=="tv" then
				gnam="WetV"
			end
		end
		
	end
	
	if gnam and string.lower(gnam)=="wetv" then
	
		if game.room.name == user.wetv then -- this is the game we are looking for
			return true
		end
	end
	
	return false
end

-----------------------------------------------------------------------------
--
-- update this user with video title
--
-----------------------------------------------------------------------------
local function send_vid_title(game,user)

local vi=game.vidinfo

	if gtab.vid_infos and gtab.vid_infos[ vi.vid_id ] then -- we got good cached info from youtube
	
		if user then
		
			usercast( user , {cmd="game",gcmd="wetv",gid=0,wetv="title",play=vi.vid_id,title=gtab.vid_infos[ vi.vid_id ].title } )
		
		else
		
			gamecast( game , {cmd="game",gcmd="wetv",gid=0,wetv="title",play=vi.vid_id,title=gtab.vid_infos[ vi.vid_id ].title } )
			
			vi.vid_title=gtab.vid_infos[ vi.vid_id ].title -- remember we told everyone
			
			if game.broadcast_news and gtab.vid_infos[ vi.vid_id ].duration>60*10 then -- tell the world, but only once

				for _,r in pairs(data.rooms) do -- broadcast to all rooms
				
					if r.brain and r.brain.user and ( not r.hush_time or r.hush_time<os.time() ) then
						roomqueue(r,{cmd="lnk",frm=r.brain.user.name,
							txt=vi.vid_title.." is now starting in room "..game.room.name,
							lnk="http://play.4lfa.com/tv/"..game.room.name})
					end
				end
			
			end

		end
		
	end


end

-----------------------------------------------------------------------------
--
-- update this user with video title
--
-----------------------------------------------------------------------------
local function check_send_vid_title(game)

local vi=game.vidinfo

	if gtab.vid_infos and gtab.vid_infos[ vi.vid_id ] then -- we got good cached info from youtube
	
		if not vi.vid_title then -- tell everyone the title
		
			send_vid_title(game)
		
		end
	
	end


end

-----------------------------------------------------------------------------
--
-- update this user with video lock
--
-----------------------------------------------------------------------------
local function send_vid_lock(game,user)

local vi=game.vidinfo

	local t=(vi.vid_timelock-vi.vid_start+vi.vid_start_tim )
	if t<0 then t=0 end

	if user then

		usercast( user , {cmd="game",gcmd="wetv",gid=0,wetv="lock",play=vi.vid_id,lock=t } ) -- tell user locked length

	else
	
		gamecast( game , {cmd="game",gcmd="wetv",gid=0,wetv="lock",play=vi.vid_id,lock=t } ) -- tell user locked length
	
	end
	
end
-----------------------------------------------------------------------------
--
-- update this user with video name and position
--
-----------------------------------------------------------------------------
local function send_vid_current(game,user)

local vi=game.vidinfo

	if user then

		usercast( user , {cmd="game",gcmd="wetv",gid=0,wetv="play",play=vi.vid_id..","..force_floor(os.time()-vi.vid_start+vi.vid_start_tim) } )	
	
	else
	
		gamecast( game , {cmd="game",gcmd="wetv",gid=0,wetv="play",play=vi.vid_id..","..force_floor(os.time()-vi.vid_start+vi.vid_start_tim) } )	
	
	end

	send_vid_title(game,user)

	send_vid_lock(game,user)

end


-----------------------------------------------------------------------------
--
-- start playing a new vid, right now
--
-----------------------------------------------------------------------------
local function play_vid(game,vid_id,owner,tim)

	if not game.vidlists then tv_add_default_vids(game) end -- just make sure we have a vidlist setup
	
	game.next_queue=nil -- always clear the next queue
	
	
	
	
local vi=game.vidinfo

	if game.vidlists[vid_id] then -- use lookup table, play first vid in this list
	
		if #(game.vidlists[vid_id]) > 1 then -- must have more than one vid in the vidlist, or we ignore it as a list to remember
		
			vi.vid_list=game.vidlists[vid_id]
			vi.vid_list_idx=1
			vi.vid_list_name=vid_id
			
			vid_id = vi.vid_list[ vi.vid_list_idx ]
			
			tim=0		
		else
		
			vid_id = game.vidlists[vid_id][1]
			
		end
		
		local loopyloops=1
		
		while game.vidlists[vid_id] do -- use lookup table again? if so then pick a random vid from this list, keep going till we hit a video
		
			vid_id=vi.vid_list[ math.random(#(vi.vid_list)) ]
			
			if game.vidlists[vid_id] then -- got a new random vid
			
				if #(vi.vid_list) > 1 then -- must have more than one vid in the vidlist, or we ignore it as a list to remember
				
					vi.vid_list=game.vidlists[vid_id]
					vi.vid_list_idx=1
					vi.vid_list_name=vid_id
				end
			
				vid_id = vi.vid_list[ vi.vid_list_idx ]
				
			else
			
				vid_id=nil
				tim=0
			
			end
			
			loopyloops=loopyloops+1	if loopyloops>10 then break end -- stop loopyloops recursion
		
		end
		
	else -- should we forget the current remembered vid list?
	
		if vi.vid_list and vi.vid_list_idx then -- part of a list, try the next vid first
		
			if vi.vid_list[vi.vid_list_idx]~=vid_id then -- are we are still playing this list?
			
				vi.vid_list=nil
				vi.vid_list_idx=nil
				vi.vid_list_name=nil
		
			end
			
		else -- not part of a list
		
			vi.vid_list=nil
			vi.vid_list_idx=nil
			vi.vid_list_name=nil
				
		end
		
		
	end
	
	if not vid_id then

		vid_id = vi.vid_id
		
		vi.vid_list=nil
		vi.vid_list_idx=nil
		
	end
	
	
	local vis=gtab.vid_infos and gtab.vid_infos[ vi.vid_id ] -- if we have info, it will be here
	
	if vi.vid_owner then -- award or punish whomever played this video depending on the crowds reaction
	
		local u=get_user(vi.vid_owner)
		local c=vi.vid_cookies
		
		if vis then
		
			vis.playtime=(vis.playtime or 0)+(os.time()-vi.vid_start) -- keep track of how much it has been watched
		
 -- this is a video of less than 5 minutes that has been watched for more than 2 minutes, add it into the random pool
			if ( not vis.added ) and ( vis.playtime > 60*2 ) and ( vis.duration < 60*5 ) and ( vis.duration>12 ) then
		
-- although it might get added multiple times (this vis may be reset in 24 hours) and these popular vids are also reset on spew reloads
-- this is all temporary, mkay, nothing lasts

				vis.added=true -- only add to pool once
				table.insert(gtab.vid_ids,vi.vid_id)
				
				
				if #gtab.vid_ids > 100 then -- keep the nimber of vids in this list to a limit, posibly this could be all the same vid but is unlikley
					table.remove(gtab.vid_ids,1) -- kill off old vids :)
				end
			
dbg("ADDING default vid : "..vi.vid_id.." : "..(vi.vid_title or "").."\n")

			end

		end
		
		if u then
			
			if c > 0 then
			
				local ip=user_ip(u)
				local cap=day_flag_get(ip,"wetv_cookies") or 0
				
				c=c+1
				
				if cap<4000 then -- daily win limit, also let us check in this code without testing it, hurrah
				
					u.cookies=u.cookies+c
						
					tv_act(game,"gives "..u.name.." "..c.." cookies for a WIN.")
					
					feats_signal_wetv(u,"win",{vi=vi})
					
					day_flag_set(ip,"wetv_cookies",cap+c)
					
				end
				
						
				
			elseif c < 0  then
			
				c=c-1
				
				if vis then
				
					vis.failcost=(vis.failcost or 1 ) * -c --make repeat fails hurt, a lot
					if vis.failcost>(1*24*60) then vis.failcost=(1*24*60) end -- cap to 1 day?
					c=-vis.failcost
				
				end
			
				if u.cookies < -c then
				
					u.cookies=0
					
				else
				
					u.cookies=u.cookies+c
					
				end
			
				
				if u.cookies==0 and c<-1000 then -- and dis them if it is major fail
				
					tv_act(game,"takes the vowels from "..u.name.." for a FAIL.")				
					set_status(nil,"dis",u.name,os.time()+(60*-c))

				else
				
					tv_act(game,"takes "..(-c).." cookies from "..u.name.." for a FAIL.")
				
				end
				
				feats_signal_wetv(u,"fail",{vi=vi})
				
			end
		
		end
	
	end
	
	local ended_watching={}
	for i,v in pairs(game.voyeurs) do
		ended_watching[v]=true -- seem to have multiple links here? or possibly joined to many rooms?
	end

-- vids are scored with time watched per user, ie 60 secs viewed by 10 people == 600
	local vid_score_per_user=0
	if vi.vid_start then -- sanity
		vid_score_per_user=os.time() - vi.vid_start
	end
	for v,b in pairs(ended_watching) do
		if vi.vid_started_watching and vi.vid_started_watching[string.lower(v.name)] then -- where here at begining and end
			feats_signal_wetv(v,"watched",{vi=vi})
			
			if vis then
			
				vis.score=(vis.score or 0) + vid_score_per_user
			
			end
		end
	end
	
	
	vi.vid_start=os.time()
	vi.vid_start_tim=tim or 0
	vi.vid_id=vid_id
	vi.vid_title=nil
	vi.vid_len=-1
	vi.vid_timelock=0 -- this will be set to not 0 for protected vids
	vi.vid_yaysux=0
	vi.vid_yay=0
	vi.vid_sux=0
	vi.vid_cookies=0
	vi.vid_owner=nil
	vi.vid_idx=data.vid_idx
	vi.vid_started_watching={} -- list of users names that where in the room at the start
	data.vid_idx=data.vid_idx+1
	
	if gtab.vid_infos[vid_id] and gtab.vid_infos[vid_id].duration==0 then -- refuse to play vids flagged as broken
	
-- some lists are fucked?
--		return play_vid( game , get_random_vid_id(game) ) -- tail recursion so mostly safe? possibly :)
		return
	end
	
-- dbg(tim)
	send_vid_current(game,nil)
--	gamecast( game , {cmd="game",gcmd="wetv",gid=0,wetv="play",play=vid_id..",0"} )
	
	for i,v in pairs(game.voyeurs) do
		local u=get_user(i)
		if not u or u.wetv~=game.room.name then -- remove broken links, user must link back and be online
			game.voyeurs[i]=nil
dbg("tvfix:",i,"\n")
		else
			vi.vid_started_watching[string.lower(v.name)]=true
		end
	end
	
	if not vid_blip_ids[vi.vid_id] then -- do not spam blips
	
		local titlestr=""
		if gtab.vid_reqs and gtab.vid_infos then -- also request some info, this may take a while

			if gtab.vid_infos[vi.vid_id] then -- we know something
			
				titlestr=" \""..gtab.vid_infos[vi.vid_id]["title"].."\""
			
			end
		end
		
		
		local ownstr=""
		if owner then
			ownstr=" as requested by "..owner
			local u=get_user(owner)
			if u then feats_signal_wetv(u,"play",{vi=vi}) end
		end
		tv_say_link(game,"You are watching "..vi.vid_id..titlestr..ownstr,"http://www.youtube.com/watch?v="..vi.vid_id)
		
		
	end
	
	if gtab.vid_reqs and gtab.vid_infos then -- also request some info, this may take a while
		if not gtab.vid_infos[vi.vid_id] then -- check we dont already know something
			table.insert(gtab.vid_reqs,vi.vid_id) -- request info
		end
	end
end



-----------------------------------------------------------------------------
--
-- check and update the timelock
--
-----------------------------------------------------------------------------
local function check_vid_timelock(game)

local vi=game.vidinfo

	if vi.vid_timelock~=0 then -- adjust only if timelock is already set to a number
	
		local old=vi.vid_timelock
	
		vi.vid_timelock=vi.vid_start + game.timelock + vi.vid_yaysux*15
	
		if vi.vid_timelock < vi.vid_start + game.timelock then -- can't take it down too low
		
			vi.vid_timelock = vi.vid_start + game.timelock
			
		end
		
		if old ~= vi.vid_timelock then -- tell everyone about the change
		
			send_vid_lock(game)
		
		end
	
	end

end

-----------------------------------------------------------------------------
--
-- advance to next vid
--
-----------------------------------------------------------------------------
local function play_next_vid(game)

local vi=game.vidinfo
local vid_id

	if game.next_queue then -- a requested next video
	
		local q,qi,qr
		
		qi=0 for i,v in pairs(game.next_queue) do qi=qi+1 end -- how many
		
		qr=math.random(qi) -- pick one
	
		qi=0 for i,v in pairs(game.next_queue) do qi=qi+1 if qi==qr then q=v break end end -- pick it
		
		if q then -- play it
		
			if vi.vid_id == q.id then -- just refresh timelock
			
				vi.vid_timelock=os.time()+game.timelock -- vids set through this command are timelocked
				send_vid_lock(game)

				return
				
			else
	
				play_vid(game, q.id , q.name , q.tim or 0 )
				
				vi.vid_timelock=os.time()+game.timelock -- vids set through this command are timelocked
				vi.vid_owner=q.name
				send_vid_lock(game)

		
				local user=get_user(q.name) -- auto yay it for the person who played it
				if user then
--					user.vid_idx=vi.vid_idx
					vi.vid_yaysux=vi.vid_yaysux + (user_rank(user)+1)
					check_vid_timelock(game)
				end
			
				return
			end
		end

	end

	if vi.vid_list and vi.vid_list_idx then -- part of a list, try the next vid first
	
		vi.vid_list_idx = vi.vid_list_idx + 1		
		vid_id = vi.vid_list[ vi.vid_list_idx ]
	
	else -- curerntly a single video

		vid_id=get_random_vid_id(game)
		
	end

	if vid_id then
	
		play_vid(game,vid_id)
		
	else -- pick next video list
	
		if vi.vid_list and vi.vid_list_idx then -- part of a list

-- pick a random vid from noir list?

			if game.vidlists.noir then -- pick a random noir vid
			
				vid_id=game.vidlists.noir[ math.random(#game.vidlists.noir) ]
			
			else
		
-- restart for now		
				vi.vid_list_idx = 1		
				vid_id = vi.vid_list[ vi.vid_list_idx ]
			
			end
		
		else
		
			vid_id=nil
			
		end
		
		if vid_id then
		
			play_vid( game , vid_id )
			
		else -- pick next vid_id
				
			play_vid( game , get_random_vid_id(game) )
		
		end
	end
	
end

-----------------------------------------------------------------------------
--
-- backtrack to previous vid
--
-----------------------------------------------------------------------------
local function play_prev_vid(game)

local vi=game.vidinfo
local vid_id

	if vi.vid_list and vi.vid_list_idx then -- part of a list, try the next vid first
	
		vi.vid_list_idx = vi.vid_list_idx - 1
		if vi.vid_list_idx < 1 then vi.vid_list_idx=1 end
		
		vid_id = vi.vid_list[ vi.vid_list_idx ]
	
		play_vid(game,vid_id)
		
	else -- not part of a list so just play next

		play_next_vid(game)
		
	end
	
end

-----------------------------------------------------------------------------
--
-- start with the first vid again
--
-----------------------------------------------------------------------------
local function play_first_vid(game)

local vi=game.vidinfo
local vid_id

	if vi.vid_list and vi.vid_list_idx then -- part of a list, try the prev vid first
	
		vi.vid_list_idx = 1
		vid_id = vi.vid_list[ vi.vid_list_idx ]
	
		play_vid(game,vid_id)
		
	else -- not part of a list so just restart this one

		play_vid(game,nil)
		
	end
	
end



-----------------------------------------------------------------------------
--
-- users are now sending back video length/title sooooooo
-- we need to check who is sending what and if there is conflict then we make a decision about
-- who is telling the truth...
--
-----------------------------------------------------------------------------
local function check_vid_conflict(game,id)

	local infostring_decode=function(s)
		local l=s:find(":")
		if l then
		return tonumber(s:sub(1,l-1)),s:sub(l+1)
	end

	if gtab.vid_infos then

		local vid=gtab.vid_infos[ id ]
		
		if vid and vid.conflict then
		
			local stamp=os.time() - ( 24*60*60 ) -- votes last for 24hours (you may revote)

			local c=0
			for n,v in pairs(vid.conflict) do
				local i=0
				for u,t in pairs(v) do
					if stamp>t then v[u]=nil else i=i+1 end -- delete old votes , count anything else
				end
				if i==0 then
					vid.conflict[n]=nil -- no more votes for this one so forget it
				else
					c=c+1
				end
			end
			if c==0 then -- no more conflict
				vid.conflict=nil
			end
		
			if vid.conflict then -- pick the best and copy it out of conflict

				local best,count
				for n,v in pairs(vid.conflict) do -- find the best
					local i=0
					for u,t in pairs(v) do i=i+1 end -- count all
						if not count or count<=i then -- find the best
							best=n
							count=i
						end
					end
				end
				if best and count then -- update video info with the best values we have
					vid.duration,vid.title=infostring_decode(best)
					vid.userid="*" -- no longer belongs to a user
					vid.stamp=os.time() -- stamp with our time
				end

			end

		end
	
	end

end

local function check_vid_info(game,info)


	if not info then return end
	if not info.user then return end
	if not info.id then return end
	if not info.duration then return end
	if not info.title then return end

--print("INFO",info.id,info.duration,info.title)

-- convert what we know to a unique string that can be reveresed
	local infostring_encode=function(duration,title) return duration..":"..title end
	
	if gtab.vid_infos then

		local userid=user_idstring(info.user)
		local stamp=os.time()

		local vid=gtab.vid_infos[ info.id ]
		if not vid then -- create info
			vid={title=info.title,duration=info.duration,userid=userid,stamp=stamp}
			gtab.vid_infos[ info.id ]=vid
		end
		
		if vid then
	
--			vid.stamp=os.time()+(24*60*60) -- one day
			
			if ( vid.title ~= info.title ) or ( vid.duration ~= info.duration ) then -- conflict (possible bug, or video has changed)
			
				local conflict=vid.conflict
				if not conflict then -- setup initial conflict data for this video
					vid.conflict={}			
--					local s=infostring_encode(vid.duration,vid.title) -- put the last person to report into conflict table
--					vid.conflict[s]={}
--					vid.conflict[s][vid.userid]=vid.stamp
					conflict=vid.conflict
				end
				
				local s=infostring_encode(info.duration,info.title)
				if not conflict[s] then conflict[s]={} end -- a vote for this
				conflict[s][userid]=stamp -- who is voting
				
				check_vid_conflict(game,info.id) -- update title and time depending on the votes we now have
			
			else -- no conflict so just update userid and time stamp to last person who sent in this info
			
				vid.userid=userid
				vid.stamp=stamp
				
			end
	
		end
	end

end

-----------------------------------------------------------------------------
--
-- check if the current video has ended or is in error
--
-----------------------------------------------------------------------------
local function check_vid(game,vid_len)

	if not game.room.brain then -- kill the tv if the bot goes missing
		tv_game_clean(game.room)
		return
	end

local vi=game.vidinfo

	if gtab.vid_infos and gtab.vid_infos[ vi.vid_id ] then -- we got good cached info from youtube
	
		check_vid_conflict(game,vi.vid_id) -- make sure we are not in conflict
	
		vi.vid_len=gtab.vid_infos[ vi.vid_id ].duration
			
	else
		vi.vid_len=10 -- fake length, you tube has 10secs to respond or video gets chopped
	end
	
	if	not vi.vid_start or
		not vi.vid_len or
		( os.time()-vi.vid_start+vi.vid_start_tim >= force_floor(vi.vid_len)+1 ) then -- play next, include a small fudge time

		play_next_vid(game)

	end

	local tim=math.ceil(vi.vid_timelock-os.time())
	
	if game.next_queue and tim<=0 then -- if the time lock has expired and people have tried to play something then...
	
		play_next_vid(game)

	end
	

	check_send_vid_title(game) -- see if we should update everyone  with the video title
	

end


-----------------------------------------------------------------------------
--
-- simple vid id check and fix
--
-----------------------------------------------------------------------------
local function check_vid_id(s)
	if type(s)~="string" then return "" end
	return string.sub(string.gsub(s, "[^a-zA-Z0-9_%-]+", "" ),-11)

end

local function check_vid_name(s)

	return string.gsub(s, "[^a-zA-Z0-9_%-]+", "" )

end


-----------------------------------------------------------------------------
--
-- say help
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	tv_say_link(game,"http://data.wetgenes.com/wetlinks/help/wetv.jpg","http://help.wetgenes.com/-/Main/TVCommands",user)
	
end
for i,v in ipairs{"help"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- recommend the new taste of wetv
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	tv_say_link(game,"Click here to try the new taste of WetV. Now with 100% less blank screens","http://wet.appspot.com/wetv",user)
	
end
for i,v in ipairs{"blank","black","broken"} do
	tv_triggers[v]=tv_trigger
end


-----------------------------------------------------------------------------
--
-- say restart
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	tv_say(game,"This doesnt work any more, try saying reg retain 100 tv",user)
--[[	
	
local r=get_room(user.name)

	if ( not r ) or ( not r.retain_noir ) then
		tv_say(game,"Sorry but you will need to retain a bot for your room first.",user)
		return
	end
	
	if r then

		tv_game_setup(r)
		
		tv_say(game,"Your room now has an 4lfa quality TV set in it, lets hope it doesn't break.",user)
		return
	end
]]
end
for i,v in ipairs{"rent"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say restart
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	tv_say(game,"Sorry this doesnt work like that any more.",user)
--[[		
	if is_tv_admin(game,user.name) then

		tv_say(game,"I have thrown the TV set out of the window for you.",user)
		
		tv_game_clean(game.room)
		return
	end
]]
end
for i,v in ipairs{"defenestrate"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- lock a tv to admin use only
--
-- simple use, just toggles the admin lock for now
--
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end
	
	if is_tv_admin(game,user.name) then -- admin only
	
		if game.vidlock then
		
			game.vidlock=false
		
			tv_say(game,"This TV is now admin unlocked.",user)
		
		else
		
			game.vidlock=true
			
			tv_say(game,"This TV is now admin locked.",user)
		end
		
	else
	
		tv_say(game,"Sorry only admins can change TV locks.",user)
	
	end
	
end
for i,v in ipairs{"lock"} do
	tv_triggers[v]=tv_trigger
end


-----------------------------------------------------------------------------
--
-- makesure the name is safe to use, alpha numeric and forced lowercase only
--
-- return nil if nil is passed in or after mucking about we end up with a blank string
--
-----------------------------------------------------------------------------
local function cleanup_vidname(name)

	if not name or name=="" then return nil end
	
	name=string.gsub(string.lower(name), "[^0-9a-z%-_]+", "" )

	if not name or name=="" then return nil end
	
	return name
	
end


-----------------------------------------------------------------------------
--
-- add a new video playlist
--
-- tv add vidname id1 id2 id3 etc
--
-- this entire list can now be played by a tv play videoname command
--
--
-----------------------------------------------------------------------------
local function tv_add(game,aa)

	local flag=aa[2] or "none"


	if not game.vidlists then tv_add_default_vids(game) end -- just make sure we have a vidlist setup
	

	local vidname = cleanup_vidname(aa[3])

	if vidname and aa[4] then -- need a name and at least one vid id

		local tab={}
		local idx=4
		local vidid
		
		if flag=="plus" or flag=="sub" then
			if game.vidlists[vidname] then -- adjust
				tab=game.vidlists[vidname]
			end
		end
	
		while aa[idx] do
		
			vidid=check_vid_name(aa[idx])
			
			if vidid and vidid~="" then -- add to list
			
				if flag=="sub" then -- remove that video
				
					for i,v in pairs(tab) do -- find
						if v==vidid then
						
							table.remove(tab,i)
							break
							
						end
					end
					
				else -- add or plus puts it at the end of the table
				
					table.insert(tab,vidid)
					
				end
			end
			
			idx=idx+1
		
		end
		
		if tab[1] then -- need one video before it gets inserted

			game.vidlists[vidname]=tab
		
			tv_say(game,vidname.." contains "..(#tab).." videos.")
			return true
			
		end
	
	elseif vidname then -- just have a name, no vids, so clear video list
	
		if flag=="add" or flag=="sub" then -- must be an add or sub flag to remove
		
			game.vidlists[vidname]=nil
			
			tv_say(game,"Cleared "..vidname)
			return true
			
		end
		
	else -- say we need a name and some ids
	
		local tab={}
	
		for n,t in pairs(game.vidlists) do -- get all possible names
		
			table.insert(tab,n)
			
		end
	
		tv_say(game,"Videos : ".. (str_join_english_list(tab) or "...") )
		
	end
		
end

tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end

	if is_tv_admin(game,user.name) then -- admin only can add new list
	
		tv_add(game,aa)
		
	else
	
		tv_say(game,"Sorry only admins can add or remove video lists.",user)
	
	end
	
end
for i,v in ipairs{"add","plus","sub"} do
	tv_triggers[v]=tv_trigger
end


-----------------------------------------------------------------------------
--
-- find available vids
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if not game.vidlists then tv_add_default_vids(game) end -- just make sure we have a vidlist setup
	
	local tab={}

	local name=aa[3]
	local count=0

	local countup=function()
		count=count+1
		if count>10 then
			userqueue(user,{cmd="note",note="notice",arg1=name.." : ".. (str_join_english_list(tab) or "...")})
			tab={}
			count=0
		end
	end

	if name and game.vidlists[name] then

		for i,n in pairs(game.vidlists[name]) do -- get all possible names
		
			table.insert(tab,i..":"..n)
			countup()
		end
	
	else
	
		if game.vid_ids and game.vid_ids[1] then
		
			name="playing"

			for i,v in ipairs(game.vid_ids) do
				local t,n
				local vi=gtab.vid_infos[v]
				if vi then
					t=v
					n=vi.title
					table.insert(tab,t..":"..n)	
				else
					t=i
					n=v
					table.insert(tab,t..":"..n)	
				end
			countup()
			end
		else
	
			name="Videos"
			for n,t in pairs(game.vidlists) do -- get all possible names
				if type(t)=="table" then t=n end
				table.insert(tab,t..":"..n)		
			countup()
			end
		end
		
	end

	userqueue(user,{cmd="note",note="notice",arg1=name.." : ".. (str_join_english_list(tab) or "...")})
	
end
for i,v in ipairs{"list"} do
	tv_triggers[v]=tv_trigger
end


-----------------------------------------------------------------------------
--
-- tell people how many videos are in the lottery
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if not game.vidlists then tv_add_default_vids(game) end -- just make sure we have a vidlist setup

	local tab={}

	for n,t in pairs(game.next_queue or {}) do -- get all possible names
	
		table.insert(tab,t.name)
		
	end

	if tab[1] then
		tv_say(game,"There are "..(#tab).." users with videos in the lottery.",user)
		userqueue(user,{cmd="note",note="notice",arg1="in lottery : ".. (str_join_english_list(tab) or "...")})
	else
		tv_say(game,"There are no users with videos in the lottery.",user)
	end
	
end
for i,v in ipairs{"queue","loto","lotto"} do
	tv_triggers[v]=tv_trigger
end	
		
-----------------------------------------------------------------------------
--
-- save the current video playlist to a personal serverside config file
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end

	if is_tv_admin(game,user.name) then -- admin only can save their list
	
		local name= string.lower( aa[3] or user.name )
		name=string.gsub(name, "[^0-9a-zA-Z%-_]+", "" )
		if name=="" or not is_admin(user.name) then name=string.lower(user.name) end -- only admins get to choose where to save
		
		local fp=io.open("save/tv/"..name..".txt","w")
	
		for n,t in pairs(game.vidlists) do -- get all possible names
					
			fp:write("tv add "..n)
			
			for i,v in ipairs(t) do
			
			fp:write(" "..v)
			
			end
			
			fp:write("\n")
			
		end
		
		fp:close()
		
		tv_say(game,"This rooms video list has been saved over the file for "..name,user)
		
	else
	
		tv_say(game,"Sorry only admins can save video lists.",user)
	
	end
	
end
for i,v in ipairs{"save"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- load the current video playlist from a personal serverside config file
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end

	if is_tv_admin(game,user.name) then -- admin only can save their list
	
		local name= string.lower( aa[3] or user.name )
		name=string.gsub(name, "[^0-9a-zA-Z%-_]+", "" )
		if name=="" then name=string.lower(user.name) end -- no funny chars
	
		local fp=io.open("save/tv/"..name..".txt","r")
		
		if not fp then 
			tv_say(game,"Sorry but I could not find the file for "..name,user)
			return
		end
		
		game.tv_hush=true
		for line in fp:lines() do
			local aa=str_split(" ",line)
			if aa then tv_add(game,aa) end		
		end
		game.tv_hush=false
		
		fp:close()
		
		tv_say(game,"This rooms video list has been loaded from the file for "..name,user)
		
	else
	
		tv_say(game,"Sorry only admins can load video lists.",user)
	
	end
	
end
for i,v in ipairs{"load"} do
	tv_triggers[v]=tv_trigger
end
-----------------------------------------------------------------------------
--
-- wipe the current video playlist
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end

	if is_tv_admin(game,user.name) then -- admin only can save their list
	
		game.vidlists={}
	
		tv_say(game,"This rooms video list has been wiped.",user)
		
	else
	
		tv_say(game,"Sorry only admins can clear video lists.",user)
	
	end
	
end
for i,v in ipairs{"wipe"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say next
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

local vi=game.vidinfo

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end
	
	local tim=math.ceil(vi.vid_timelock-os.time()) -- check timelock
	if tim<=0 or is_tv_admin(game,user.name) then -- admin only can skip when timelocked

		play_next_vid(game)

	else

		tv_say(game,"Sorry only admins can skip the current video.",user)

	end
	
end
for i,v in ipairs{"next","skip"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say prev
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

local vi=game.vidinfo

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end
	
	local tim=math.ceil(vi.vid_timelock-os.time()) -- check timelock
	if tim<=0 or is_tv_admin(game,user.name) then -- admin only can skip when timelocked
	
		play_prev_vid(game)
	
	else
	
		tv_say(game,"Sorry only admins can skip the current video.",user)
	
	end
	
end
for i,v in ipairs{"prev","previous"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say rewind
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

local vi=game.vidinfo

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end
	
	local tim=math.ceil(vi.vid_timelock-os.time()) -- check timelock
	if tim<=0 or is_tv_admin(game,user.name) then -- admin only can skip when timelocked
	
		play_first_vid(game)
	
	else
	
		tv_say(game,"Sorry only admins can rewind the current video.",user)
	
	end
	
end
for i,v in ipairs{"rewind"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say restart
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

local vi=game.vidinfo

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end
	
	local tim=math.ceil(vi.vid_timelock-os.time()) -- check timelock
	if tim<=0 or is_tv_admin(game,user.name) then -- admin only can skip when timelocked
	
		play_vid(game)
	
	else
	
		tv_say(game,"Sorry only admins can restart the current video.",user)
	
	end
	
end
for i,v in ipairs{"restart"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say info
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

local vi=game.vidinfo

	local timeinfo=""
	
	if vi.vid_len>0 and vi.vid_start>0 then
	
		local t=os.time()-vi.vid_start+vi.vid_start_tim 
		
		timeinfo=" for "..t.." of "..vi.vid_len.." seconds."
	
	end

	local tim=math.ceil(vi.vid_timelock-os.time())
	
	if tim>0 then
	
		timeinfo=timeinfo.." (locked "..tim.." secs)"
	end
	
	if vi.vid_list_name then
	
		timeinfo=timeinfo.." ("..vi.vid_list_name..")"
		
	end

	local titlestr=""
	if gtab.vid_reqs and gtab.vid_infos then -- also request some info, this may take a while

		if gtab.vid_infos[vi.vid_id] then -- we know something
		
			titlestr=" \""..gtab.vid_infos[vi.vid_id]["title"].."\""
		
		end
	end
		
	tv_say_link(game,"You are watching "..vi.vid_id..titlestr..timeinfo,"http://www.youtube.com/watch?v="..vi.vid_id,user)
	
end
for i,v in ipairs{"info"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say yay or sux
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end
	
local vi=game.vidinfo
local yaysux=string.lower(aa[2])
local ret=true -- auto hide most msgs

	if vi.vid_idx~=user.vid_idx then -- may only vote once per video

		user.vid_idx=vi.vid_idx
		
		if yaysux=="yay" then
		
			vi.vid_timelock=os.time()+game.timelock -- mark this vid as timelocked
				
			vi.vid_yaysux=vi.vid_yaysux + (user_rank(user)+1)
		
			vi.vid_yay=(vi.vid_yay or 0) + 1
			vi.vid_cookies=vi.vid_cookies + 1
			
			check_vid_timelock(game)
			
			feats_signal_wetv(user,"yay",{vi=vi})

			
		elseif yaysux=="sux" then
		
			vi.vid_yaysux=vi.vid_yaysux - (user_rank(user)+1)
			
			vi.vid_sux=(vi.vid_sux or 0) + 1
			vi.vid_cookies=vi.vid_cookies - 1
			
			check_vid_timelock(game)
			
			feats_signal_wetv(user,"sux",{vi=vi})
		end
	
		ret=false -- show this msg
	end
	
	local tim=math.ceil(vi.vid_timelock-os.time())
	
	if vi.vid_owner and vi.vid_owner==user.name and yaysux=="sux" then -- suxing our own video makes it skip
	
		play_next_vid(game)
		return false
	
	elseif tim<=0 then
	
		userqueue(user,{cmd="note",note="notice",arg1="Video is not timelocked."})
	else
	
		userqueue(user,{cmd="note",note="notice",arg1="Video is timelocked for "..tim.." more seconds."})
	end
	
	return ret -- show or hide the actual command, only show if it effected the count
	
end
for i,v in ipairs{"yay","sux"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say timelock
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end

	if is_tv_admin(game,user.name) then -- admin only

		if aa[3] then
		
			game.timelock=force_floor(aa[3]) or 60
			
			check_vid_timelock(game)
		
		end
	
	else
	
		if aa[3] then
			tv_say(game,"Sorry only admins can change the timelock.",user)
			return
		end
	
	end

	tv_say(game,"Videos are play locked for "..game.timelock.." seconds.",user)
	
end
for i,v in ipairs{"timelock"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
--
-- say play
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)

	if game.vidlock and not is_tv_admin(game,user.name) then -- lock check
		userqueue(user,{cmd="note",note="notice",arg1="This TV is locked so only admins can do that."})
		return true
	end

local vi=game.vidinfo

local vid_id
local ret=false

-- allow simple way of setting time in hours or minutes
local start_tim=0
	local str=aa[4] or ""
	local ss
	if str~="" then
		ss=str_split("h",str,false)
		if ss[1] and ss[2] then -- hours
			start_tim=start_tim+(60*60*force_floor(tonumber(ss[1])))
			str=ss[2] -- the rest
		end
		if str~="" then
			ss=str_split("m",str,false)
			if ss[1] and ss[2] then -- minutes
				start_tim=start_tim+(60*force_floor(tonumber(ss[1])))
				str=ss[2] -- the rest
			end
			if str~="" then
				start_tim=start_tim+force_floor(tonumber(str))
			end
		end
	end
	if start_tim<0 then start_tim=0 end

	if aa[3] then
	
		vid_id=check_vid_name(aa[3])
		
		if vid_id then
		
local banned=gtab.ban_list and gtab.ban_list[vid_id] -- who banned it if it is banned

			if banned then
				return tv_ret(user,"Sorry but "..banned.." has banned that video.")
			
			end

			local tim=math.ceil(vi.vid_timelock-os.time())
				
				
--				tv_say(game,"Video may not be changed for "..tin.." more seconds",user)

				game.next_queue=game.next_queue or {}
				
				game.next_queue[ user_ip(user) ] = { id=vid_id , name=user.name , tim=start_tim }

				if gtab.vid_reqs and gtab.vid_infos then -- also request some info, this may take a while
					if not gtab.vid_infos[vid_id] then -- we know nothing
						table.insert(gtab.vid_reqs,vid_id) -- request info
					end
				end
		
			
			if tim>0 and not is_tv_admin(game,user.name) then -- video is locked, can not be changed yet
			
				userqueue(user,{cmd="note",note="notice",arg1="This video is timelocked for "..tim.." more seconds but your video has been added to the next video lottery. Please wait for the timelock to end to see if you win, spaming will no longer help you."})
				ret=true
				
			else
			
				play_next_vid(game)	-- this should start the vid we just added into the next_queue 
				
			end
		
		else
		
			tv_say(game,"invalid youtube video ID.",user)
		
		end
	else
	
		tv_say(game,"type tv play youtubeid **If you paste a youtube embed code here then it will be converted to this format**",user)
		
	end
	
	return ret
end
for i,v in ipairs{"play"} do
	tv_triggers[v]=tv_trigger
end


-----------------------------------------------------------------------------
--
-- say ban
--
-----------------------------------------------------------------------------
tv_trigger=function(game,user,aa)
	
	if not is_admin(user.name) then -- only admins
		userqueue(user,{cmd="note",note="notice",arg1="Sorry only site admins can do that."})
		return true
	end

local vi=game.vidinfo

local vid_id=vi.vid_id
local ret=false

local mode="ban"
	if aa[2] == "unban" then mode="unban" end
	
	if aa[3] then vid_id=check_vid_id(aa[3]) end
	
	
	if not vid_id then return tv_ret(user,"Invalid video id.") end
	
	
	if not gtab.ban_list then gtab.ban_list={} end
	
local banned=gtab.ban_list[vid_id] -- who banned it if it is banned

	
	if mode=="unban" then -- check there is a banned video to unban
	
		if not banned then 
			return tv_ret(user,vid_id.." is already unbanned.")
		end
		
	else -- check video is not already banned
		
		if banned then 
			return tv_ret(user,vid_id.." is already banned.")
		end
	end
	
			
	if mode=="ban" then -- ban it
	
		gtab.ban_list[vid_id]=user.name
		
-- go through the default vids and remove the id from there if we find it

		for i=1,#gtab.vid_ids do
		
			if gtab.vid_ids[i] and gtab.vid_ids[i] == vid_id then -- I think this loop is safe :)
			
				table.remove(gtab.vid_ids,i)
				
				i=i-1 -- do not skip the next video after removing one 
			
			end
			
		end
		
		
		tv_say(game,vid_id.." has been banned by "..user.name,user)
		return
	
	else -- unban it
	
		gtab.ban_list[vid_id]=nil
		tv_say(game,vid_id.." has been unbanned by "..user.name,user)
		return
		
	end
	
	return ret
end
for i,v in ipairs{"ban","unban"} do
	tv_triggers[v]=tv_trigger
end

-----------------------------------------------------------------------------
-- 
-- setup extra game stuffs
-- 
-----------------------------------------------------------------------------
gtab.setup = function(game)

end

-----------------------------------------------------------------------------
-- 
-- clean extra games stuff
-- 
-----------------------------------------------------------------------------
gtab.clean = function(game)


end
-----------------------------------------------------------------------------
-- 
-- called once a minute by the pulse code for all active games
-- 
-----------------------------------------------------------------------------
gtab.pulse = function(game)

--dbg("gamepulse ",game.id,"\n")

	check_vid(game)

end
-----------------------------------------------------------------------------
--
-- add a tv to this newly created room
--
-----------------------------------------------------------------------------
gtab.add_to_room = function(room)

	tv_game_setup(room)

--	room.ville_game=gtab
--	room.novillebot=true
			
--	gtab.room_names[room.name]=true
--	gtab.update()

end


-----------------------------------------------------------------------------
-- 
-- handle custom game msgs
-- 
-----------------------------------------------------------------------------
gtab.game_wetv_msg = function(game,msg,user)	

	if not game or game.name~="wetv" then		-- need to get a game to control the channel
	
		local room
		
		if user.wetv then						-- the last room we where watching
			room=get_room(user.wetv)
		end
		
		if room then							-- get the game if we got a rom
			game=room.game
		end
		
		if not game or game.name~="wetv" then	-- the above didint work so get the basic room
			room=get_room("public.tv")
			game=room.game
		end
	
	end

local ret={cmd="game",gcmd="ret",gid=msg.gid,gret="Command failed."}

	if not game or game.name~="wetv" then
		return ret
	end

	if msg.wetv=="ready" then
	
--		roomqueue(user.room,{cmd="act",frm=user.name,txt="is watching TV."})
		
		ret.gret="OK"
		
		check_vid(game)
		
		send_vid_current(game,user)
		

	elseif msg.wetv=="info" then

		local vid_id=check_vid_id(msg.video)
		local vid_duration=math.floor(tonumber(msg.duration) or 0) -- signal error with a length of 0
		local vid_title=tostring(msg.title)

		if vid_id == game.vidinfo.vid_id then -- getting info back about current video (users can only provide info about this one)

-- remember name and len
			check_vid_info(game,{user=user,id=vid_id,duration=vid_duration,title=vid_title})

-- next video?
			check_vid(game,vid_len)
		
		end

	end


	return ret
end



-----------------------------------------------------------------------------
-- 
-- try and find a you tube if from a youtube url or whatever we can sniff
-- return the youtube id or nil if not found
-- 
-----------------------------------------------------------------------------
local function string_to_youtubeid(s)

local r
local sp

	sp=string.find(s,"youtu.be/",1,true)
	if sp then
		r=string.sub(s,sp+9,sp+9+10)
	end

	sp=string.find(s,"youtube.com/watch?v=",1,true)
	if sp then
		r=string.sub(s,sp+20,sp+20+10)
	end
	
	sp=string.find(s,"youtube.com/v/",1,true)
	if sp then
		r=string.sub(s,sp+14,sp+14+10)
	end

	sp=string.find(s,"youtube.com/user/",1,true)
	if sp then
		local a=str_split("/",s)
		for i=#a,1,-1 do
			if string.len(a[i])==11 then r=a[i] break end -- first 11digit part at end of url, probably good :)
		end
	end

	sp=string.find(s,"youtube.com/embed/",1,true)
	if sp then
		r=string.sub(s,sp+18,sp+18+10)
	end
	
-- this is quite generic if the above failed
	if not r then
		sp=string.find(s,"youtube",1,true) -- check site
		if sp then
			sp=string.find(s,"v=",1,true) -- look for args
			if sp then
				r=string.sub(s,sp+2,sp+2+10)
			end
		end
	end

	if r then r=string.gsub(r, "[^a-zA-Z0-9_%-]+", "" ) end -- check chars
	
	if r and string.len(r)~=11 then r=nil end -- check length

	return r		
end

	
-----------------------------------------------------------------------------
-- 
-- watch what is being said
-- 
-----------------------------------------------------------------------------
gtab.game_room_chat = function(game,msg,user)

	if user and (not is_room_owner(game.room,user.name)) then -- owners are safe
		if msg.txt and game.room.badwords then -- ignore comands that swear
			if is_swear(string.lower(msg.txt),game.room.badwords) then
				return
			end
		end
	end
-- only pay atention to talking users, check if the text contains the words we are looking for

	if msg.cmd=="lnk"  and user then -- check for tv link commands from the bots, so video auto plays
	
		if string.sub(msg.txt,1,17)~="You are watching " then -- not the You are watching msgs which are now links
		
			local v=string_to_youtubeid(msg.lnk)
		
			if v then
				if not game.vidlock then
					roomqueue(game.room,{cmd="say",frm=user.name,txt="tv play "..v},user)
				end				
				return true -- hide this msg, it has been replaced with a new one
			end
		end
		
	elseif msg.cmd=="say" and user then -- check for tv commands
				
-- check for youtube embed spam :)		
--<object width="425" height="344"><param name="movie" value="http://www.youtube.com/v/JW58Kj822Rc&hl=en&fs=1"></param><param name="allowFullScreen" value="true"></param><embed src="http://www.youtube.com/v/JW58Kj822Rc&hl=en&fs=1" type="application/x-shockwave-flash" allowfullscreen="true" width="425" height="344"></embed></object>		

		check_vid(game) -- start a new vid?
	
		local v=string_to_youtubeid(msg.txt)
		if v then
			if not game.vidlock then
				roomqueue(game.room,{cmd="say",frm=user.name,txt="tv play "..v},user)
			end
			return true -- hide this msg, it has been replaced with a new one
		end
		
		
		local aa=str_split(" ",(msg.txt))
		
		if string.lower(aa[1])=="tv" then -- handle special commands when we are being addressed
		
			local f
			
			if aa[2] then
			
				f=tv_triggers[ string.lower(aa[2]) ] -- check for trigger
			
			end
				
			if f then return f(game,user,aa) end

		end
	end

end


-----------------------------------------------------------------------------
-- 
-- the brain will call this once a sec to provide a game update
-- 
-----------------------------------------------------------------------------
gtab.brain_update = function(game,brain)
--dbg("checking vid "..brain.user.name.."\n")
	check_vid(game) -- start a new vid?
end

-----------------------------------------------------------------------------
-- 
-- someone has joined this room
-- 
-----------------------------------------------------------------------------
gtab.game_room_join = function(game,user)

local nam="cthulhu"
		
	if game.room.brain then nam=game.room.brain.user.name end
	
	if user.brain then return end

	if user.gamename~="WetV" and user.gametype~="WetVille" and game.room then
	
		userqueue(user,{cmd="lnk",frm=nam,
		txt="This is a TV room. Click here to watch TV with everyone else.",
		lnk="http://play.4lfa.com/tv/"..game.room.name})
		
	end
	
	
-- remove from old voyeur list?

		local roomold
		local gameold
		
		if user.wetv then						-- the last room we where watching
			roomold=get_room(user.wetv)
		end
		
		if roomold then							-- get the game if we got a room
			gameold=roomold.game
		end
		
		if gameold then -- kill old voyeur mark
		
			gameold.voyeurs[user.name]=nil
		
		end

-- add to new voyeur list
	
		game.voyeurs[user.name]=user
		user.wetv=game.room.name
	
-- start vid	
	
		check_vid(game)
		
		send_vid_current(game,user)
	
	
	user.vid_idx=game.vidinfo.vid_idx -- may not vote unless you where here at the start of the vid :)
	

end

-----------------------------------------------------------------------------
-- 
-- someone has left this room
-- 
-----------------------------------------------------------------------------
gtab.game_room_part = function(game,user)

--dbg("part\n")

end

-----------------------------------------------------------------------------
-- 
-- 
-- 
-----------------------------------------------------------------------------
gtab.game_save = function(game)


end





function tv_add_default_vids(game)

	game.tv_hush=true

	game.vidlists=game.vidlists or {}
--[[
	for i,v in ipairs(tv_add_lines) do	
		local aa=str_split(" ",v)
		tv_add(game,aa)
	end
]]	
	game.tv_hush=false
	
end

--
-- setup a tv in a room
--
function tv_game_setup(room)

	if room.game and (room.game.name=="wetv") then -- make sure we have a game setup and that game is wetv
	
	-- already there, so do nothing
	
	else -- clean and build
	
		setup_gameroom(room,{ name="wetv" , arena={duration=60*60*24*365} })
		
	end
	
	local game=room.game
	
	if not game.timelock then game.timelock=10 end -- how long each video should play before people are allowed to change it
	
	if not game.vidlist then game.vidlist={} end -- what to play next
	if not game.vidinfo then game.vidinfo={} end -- what we know about what we are playing now
	
	game.flag_keep_voyeurs = true
	
	tv_add_default_vids(game)
		
	local aa=str_split(".",room.name)
	local name=string.lower(aa[1])
	name=string.gsub(name, "[^0-9a-zA-Z%-_]+", "" )
	if name and name~="" then -- load from default list
	
		local fp=io.open("save/tv/"..name..".txt","r")
		
		if fp then 
			game.tv_hush=true
			for line in fp:lines() do
				local aa=str_split(" ",line)
				if aa then tv_add(game,aa) end
			end
			game.tv_hush=false
		
			fp:close()
		end
	end

	game.brain_updateready=true
	return game
	
end


--
-- remove a tv from a room (call if the room loses its bot)
--
function tv_game_clean(room)

	clean_gameroom(room)

end

-----------------------------------------------------------------------------
--
-- this expects to be wrapped in a coroutine so it can yield
-- 
-- pickup trailers from youtube
--
-----------------------------------------------------------------------------
gtab.update_co_trailers = function()
--[[

	local vids={}

	for idx=1,5 do

		local ret=lanes_url("http://www.youtube.com/trailers?p="..idx) -- pull in video info
		
		if ret.body then -- got some trailers...
		
			local t=str_split("/watch?v=",ret.body)
			
			for i,v in ipairs(t) do
			
				if i>1 then -- never the first
				
					local id=string.sub(v,1,11) -- got id
					
					id=check_vid_id(id)
					
					if string.len(id)==11 then -- only if right length
					
						vids[id]=true -- merge
						
					end
					
				end
			
			end
		
		end
		
	end
	
	for id,b in pairs(vids) do
	
		table.insert(gtab.vid_ids,id)
				
--dbg("id=",id,"\n")
	
	end
]]
end

-----------------------------------------------------------------------------
--
-- this expects to be wrapped in a coroutine so it can yield
-- 
-- pickup trailers from youtube
--
-----------------------------------------------------------------------------
gtab.update_co_movies = function()


dbg("movie loading\n")

--[[
	for i,v in pairs(gtab.movie_ids) do -- keep table pointers but clear all movies

		if type(v)=="table" then
			for j,v in pairs(v) do
				gtab.movie_ids[i][j]=nil
			end
		else
			gtab.movie_ids[i]=nil -- live cleanup
 		end
	end
]]

--[[	for i,v in ipairs{
		"http://www.youtube.com/movies/horror?fl=f&l=en&pt=g&st=d",
		"http://www.youtube.com/movies/action-adventure?fl=f&l=en&pt=g&st=d",
		"http://www.youtube.com/movies/comedy?fl=f&l=en&pt=g&st=d",
		"http://www.youtube.com/movies/classics?fl=f&l=en&pt=g&st=d",
		"http://www.youtube.com/movies/mystery-suspense?fl=f&l=en&pt=g&st=d",
		"http://www.youtube.com/movies/animation-cartoons?fl=f&l=en&pt=g&st=d",
		"http://www.youtube.com/movies/crime?fl=f&l=en&pt=g&st=d",
--		"http://www.youtube.com/movies?fl=f&pt=fm",
		} do
]]

	for i,v in pairs(playlists) do

		local vids={}
		v.vids=vids -- remember playlist result

-- lets try and pull down a html url and scrape the fuck out of it

dbg("fetching playlist (notworking) "..i.."\n")

		-- if we use any video id, with a playlist then we get the full? playlist list of videos
		local url="https://www.youtube.com/watch?v=fdO4ea3oP8Q&list="..v.id
--		local ret=lanes_url(url) -- pull in video info source

--[[
dbg(url,"\n")
dbg(ret,"\n")
dbg(ret.code,"\n")
dbg(ret.body,"\n")
os.exit(0)
]]

-- this old way no longer works
--[[
		for p=1,10 do

dbg("fetching playlist "..i.." page "..p.."\n")

			local url="http://gdata.youtube.com/feeds/api/playlists/"..v.id.."?v=2&alt=jsonc&max-results=50&start-index="..(1+((p-1)*50))
			
			
			local ret=lanes_url(url) -- pull in video info source
			
			if ret and ret.body and type(ret.body)=="string" and ret.body~="" then -- got some movies
			
				local j=Json.Decode(ret.body)
				if j and j.data and j.data.items then else break end
				
--dbg("got some data "..#ret.body.." #"..(#j.data.items).."\n")

				for i,v in ipairs(j.data.items) do
				
					local embed=false
					
					if v.video and v.video.accessControl and v.video.accessControl.embed=="allowed" then embed=true end
					
					if v.video and v.video.status and v.video.status.value and v.video.status.value=="restricted" then
						embed=false end -- not available (probably server location) so kill it
						
					if not embed then
					
dbg("noembed "..(v.video.id).." : "..(v.video.title).."\n")

						gtab.vid_infos[v.video.id]=nil
						
					else
					
dbg("video "..(v.video.id).." : "..(v.video.title).."\n")

						local tab={}
						
						tab.id=v.video.id
						tab.title=(v.video.title)

						tab.duration=tonumber(v.video.duration)
--						if tab.duration==0 then tab.duration=60*60*3 end -- hack fix

						tab.stamp=os.time()+(60*60*24*28) -- remember long time

						if tab.title and tab.duration then
							gtab.vid_infos[tab.id]=tab
							
							vids[#vids+1]=tab -- save in list as well
						end
					
					end
				end
			end

		end
]]
	end

-- handle playlist results

	for _,pl in pairs(playlists) do -- clean out all cats
		gtab.playlists[pl.cat]={}
	end

	for _,pl in pairs(playlists) do
		local c=gtab.playlists[pl.cat]
		
		for _,vid in pairs(pl.vids) do -- merge cats
			c[#c+1]=vid
		end
		
	end
	
	if gtab.playlists.blips and gtab.playlists.blips[1] then -- replace blips
		vid_blip_ids={}
		for i,vid in pairs( gtab.playlists.blips ) do
			vid_blip_ids[ vid.id ] = vid.id
			vid_blip_ids[ #vid_blip_ids+1 ] = vid.id
		end
dbg("inserted blips "..#vid_blip_ids.."\n")
	end
	
	
	if gtab.playlists.music and gtab.playlists.music[1] then -- replace music
		if #gtab.vid_ids<30 then -- needs to be filled
		
			while #gtab.vid_ids > 1 do -- clean
				table.remove(gtab.vid_ids,1)
			end
			for i,vid in pairs( gtab.playlists.music ) do
				gtab.vid_ids[ #gtab.vid_ids+1 ] = vid.id
			end
dbg("inserted default vids "..#gtab.vid_ids.."\n")
		end
	end
	
	for n,b in pairs(noir_cats) do
	
		local t=gtab.movie_ids[n]
		
		while #t > 1 do -- clean
			table.remove(t,1)
		end

		for i,vid in pairs( gtab.playlists[n] or {} ) do -- fill in 
			t[ #t+1 ] = vid.id
		end

	end

--[[

	for i,v in pairs(noir_cats) do
	
		local vids={}
		local do_replace=false

		for p=1,1 do	

--		local ret=lanes_url("http://www.youtube.com/movies/"..v.."?fl=f&l=en&pt=nr&st=d&view=11&p="..p) -- pull in video info source
		local ret=lanes_url(v) -- pull in video info source
		
		if ret and ret.body and type(ret.body)=="string" and ret.body~="" then -- got some movies
		
			local t=str_split("ytimg.com/vi/",ret.body)
			
			for i,v in ipairs(t) do
			
				if i>1 then -- never the first
				
					local id=string.sub(v,1,11) -- got id
					
					id=check_vid_id(id)
					
					if string.len(id)==11 then -- only if right length
					
						if gtab.vid_infos[id] then -- we know this vid so we can check it first
							if gtab.vid_infos[id].duration>0 then
								vids[id]=true -- merge
								do_replace=true
							end
						else
							vids[id]=true -- merge
							do_replace=true
						end
						
					end
					
				end
			
			end
		
		end
		
		end
		
		if do_replace then
			for j,v in pairs(gtab.movie_ids[i]) do -- remove old
				gtab.movie_ids[i][j]=nil
			end

			for id,b in pairs(vids) do -- refill table with new movies
				table.insert(gtab.movie_ids[i],id)
				
--				if not gtab.vid_infos[id] then -- only ask for new vids
					table.insert(gtab.vid_reqs,id) -- request info from thepubes about video
--				end
			end
		end

dbg(i.." movies found "..(#gtab.movie_ids[i]).."\n")
	end
]]	
	


end

-----------------------------------------------------------------------------
--
-- this expects to be wrapped in a coroutine so it can yield
--
-----------------------------------------------------------------------------
gtab.update_co = function()

	local utvid=table.remove(gtab.vid_reqs,1)
	
-- nothing we can do here works anymore so just give up
	do return end

	if ( gtab.vid_infos[utvid] and gtab.vid_infos[utvid].stamp ) then
		if gtab.vid_infos[utvid].stamp >= os.time() then return end -- we have valid info already
	end
		
	if utvid then

--dbg("reading data from youtube "..utvid.."\n")
	local ret=lanes_url("http://gdata.youtube.com/feeds/api/videos/"..utvid.."?alt=json") -- pull in video info
--dbg("read    data from youtube "..utvid.."\n")
	
	if ret.body and ret.body~="" then
	
		if (ret.body:sub(1,15)=="Video not found") or (ret.body:sub(1,10)=="Invalid id") then
		
			gtab.vid_infos[utvid]={title="notfound",duration=0,stamp=os.time()+(60*60)} -- mark this id as dead from youtubes point of view, but only for a little while
dbg(#gtab.vid_reqs.." : ".."bad video id "..utvid," NOTFOUND\n")
		
		elseif not (string.find(ret.body, "yt$duration", 1, true)) then -- bad video, contains no useful info
		
--			dbg("Video not found\n")
		
--			gtab.vid_infos[utvid]={title="broken",duration=0,stamp=os.time()} -- mark this id as dead from youtubes point of view
dbg(#gtab.vid_reqs.." : ".."bad video id "..utvid,"\n")
		else
		
			if string.find(ret.body, "yt$noembed", 1, true) then -- we can not play this :(
			
--			dbg("Video not embededable\n")
			
			gtab.vid_infos[utvid]={title="noembed",duration=0,stamp=os.time()+(60*60*24*28)} -- mark this id as dead from youtubes point of view

dbg(#gtab.vid_reqs.." : ".."bad video id "..utvid.." NOEMBED","\n")

			else
	
			local info=Json.Decode(ret.body)
			
			local tab={}
			
	--			dbg(serialize(info["entry"]["title"]))
	--			dbg(serialize(info["entry"]["media$group"]["yt$duration"]))


	--			dbg(serialize(info["entry"]["media$group"]["yt$noembed"])) -- lost information, duh

				if info["entry"] then

					tab.title=info["entry"]["title"]["$t"]
					tab.duration=tonumber(info["entry"]["media$group"]["yt$duration"]["seconds"])
					if tab.duration==0 then tab.duration=60*60*3 end -- hack fix
					tab.stamp=os.time()+(60*60*24*28) -- remember long time
					tab.id=utvid

				end
			
			
				if tab.title and tab.duration then -- remember what we know, trust youtube to tell us the truth
				
					gtab.vid_infos[utvid]=tab

dbg(#gtab.vid_reqs.." : ".."new video id "..utvid.." : "..tab.title,"\n")
					
				end
			
--				dbg(serialize(tab))
			end
			
		end
	
	end
		
--dbg("read data from youtube "..utvid.."\n")

	end


end


-----------------------------------------------------------------------------
--
-- a local update function, sits in a coroutine handling youtube info requests
--
-- this functon is called whenever we get a chance
--
-----------------------------------------------------------------------------
gtab.update = function()

-- check the status of the coroutine
	if type(gtab.co)~="thread" then gtab.co=nil end -- no co yet
	if gtab.co and coroutine.status(gtab.co)=="dead" then gtab.co=nil end -- the co died

-- grab new movies from youtube once a day?
	if not day_flag_get("*","wetv_movies_done") then
		if not gtab.co then -- only when not doing anything else
		
			gtab.co=coroutine.create(gtab.update_co_movies)
		
			day_flag_set("*","wetv_movies_done")
		end
	end

	
-- WARNING -- this may exit here
	if not gtab.co and not gtab.vid_reqs[1] then return end -- do nothing else till we have a co or a request

	
	if not gtab.co then -- need to create a new co
	
		gtab.co=coroutine.create(gtab.update_co)
	
	end

	local ret,_ret=coroutine.resume(gtab.co)

	if ret~=true then
		dbg('\n'.._ret..'\n')
		gtab.co=nil
	end


-- "http://gdata.youtube.com/feeds/api/videos/JsD6uEZsIsU"

end

-----------------------------------------------------------------------------
--
-- Build and check environment
--
-- called by the main game after a spew: reload to make sure mysql tables etc exist
--
-----------------------------------------------------------------------------
gtab.build_and_check_environment = function()

dbg("checking wetv data\n")

	gtab.vid_reqs=gtab.vid_reqs or {} -- video ids we wish to know more about
	gtab.vid_infos=gtab.vid_infos or {} -- video ids we now know something about
	
local function dr(dat)

	if not get_room(dat.name) then
	
		new_room(dat)
		
	end
		
	local room=get_room(dat.name) -- update...	
	
	room.welcome=dat.welcome
	room.addnoir=dat.addnoir
	room.locked=dat.locked
	
	
	return tv_game_setup(room)
end

data.saved_color[ "reg" ]="ff00ff"

	if not data.vid_idx then data.vid_idx = 0 end -- our unique vidplay id, inc on each play
	
	if not data.vids then -- cache of video info, sent from clients or grabbed from youtube youtubeid->tab
	
		data.vids={}
	
	end

local game

local t={
	name="public.tv",
	welcome="http://data.wetgenes.com/wetlinks/help/wetv.jpg",
	addnoir="reg",
}

-- always force the first room

	dr(t)

	
-- update the other tv rooms

	for i=1,100 do
	
		local room=get_room("public.tv."..i)
		
		if not room then break end
		
		t.name="public.tv."..i
		t.welcome="http://data.wetgenes.com/wetlinks/help/wetv.jpg",
		dr(t)
	
	end
	
	game=dr{
	name="fail.tv",
	locked="guests",
	welcome="http://data.wetgenes.com/wetlinks/help/wetv.jpg",
	addnoir="reg"}
	


-- the all new movie tv chanels full o crap :)
	for i,v in pairs(noir_cats) do
		game=dr{
		name="tv."..i,
		welcome="Welcome to tv."..i.." this room is usually TV locked so just sit back and watch this movie channel.",
		addnoir="reg"}	
		game.vidlock=true
		game.vid_ids=gtab.movie_ids[i]
		game.broadcast_news=true

		local room=game.room -- add some protection
		if room and ( (room.protect_time or 0 )<os.time()+(60*60*24*10) ) then room.protect_time=os.time()+(60*60*24*10) end
		
	end

	day_flag_clear("*","wetv_movies_done")



-- make sure all tv rooms contain a tv
	for n,r in pairs(data.rooms) do
	
		local aa=str_split(".",n)
		if aa[2]=="tv" then
			
			tv_game_setup(r)
		end
	end
	
	
	
	queue_update( gtab ) -- queueing again will just replace the old one so its safe to do multiple times
	
end



function wetv_log_most_played()

	for id,v in pairs(gtab.vid_infos) do
	
		if v.score and v.score>(60*5) and v.duration and v.duration>30 then -- needs at least 60*5 secs of user view time to be logged and be a 30sec or more video
		
			if v.title then -- only when we know the title
			
				local title=string.sub(string.gsub(v.title, "[^0-9a-zA-Z%-_ .]+", "" ),1,100) -- sanitize title
				
--dbg(id," : ",v.playtime," : ",title,"\n")
-- this function store its data at the end of yesterdays logs
loglast("","topvid",id,v.score,title)
			
			end
		end
		
		v.score=0 -- reset scores after logging them
		v.failcost=nil -- clear failcost every day
		
--		if v.stamp+(60*60*48)<os.time() then -- old, kill it
--			gtab.vid_infos[id]=nil
--		end
	end

end
