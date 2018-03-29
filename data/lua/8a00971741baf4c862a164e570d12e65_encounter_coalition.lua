--[[------------------------------------------------------------------------------------------------------------------------------------------------- ]
	ENCOUNTER SCRIPT FILE: COALITION

	Last Modified:  December 23, 2009

	Globals shared with C++ module:
		ACTION - actions invoked by script (see below)
		POSTURE - obsequious, friendly, hostile
		ATTITUDE - this alien's attitude toward player (1 to 100)
		GREETING - greeting text after calling Greeting()
		STATEMENT - statement text after calling Statement()
		QUESTION - question text after calling Question()
		RESPONSE - alien's responses to all player messages
		QUESTION1..5 - choices available in a branch actiona
		Q{} - same choices but in table format
		GOTO1..5 - jump locations associated with branch choices
		G{} - same gotos but in table format

	ACTION REFERENCE:
		restart = start dialogue over at beginning
		terminate = end communication
		attack = engage in combat

	C++ module should examine all globals after any function is called.
--]]

--include common encounter functions
dofile("data/encounter/encounter_common.lua")

--[[ What do you think of this idea: I've dumped all the statements from the Coalition initial file,
	 plus a few from other files (most are the same) into this categorized statements table (could
	 do same for greetings). Using this (see below) would have one big advantage: if the statements
	 are categorized well, so that the same set of alien responses will suffice for each category,
	 we can make the player field a *table* (just like the alien response), thus greatly multiplying
	 the range of communications statement/response combinatoric possibilities. It also would allow
	 further dumping of additional statements and greetings into the categories to be propagated to
	 all scripts with essentially no effort at all (and extending communications variability yet
	 further). All the dialogue is also centralized into 1 spot for easy maintenance, saving a bit
	 on editing time.

	 The disadvantage is that then you no longer have the exact text you're replying to directly
	 above your reply, and would need to refer to the common file (where these would wind up).
	 There are not so many of many statements & greetings that this would be a big issue.

	 I put a few examples of using this into the friendly statements section; I've tested this &
	 it works OK. - ww
--]]
function SetPlayerTables()
	playerStatement= {
		obsequious= {
			remark= {
				alien= {
					"Greetings and felicitations oh kind and merciful alien.",
					"We can see that you are indeed the true race.",
					"Take pity on us who are not fit to grovel in your waste products.",
				}
			},
			comment= {
				trust= {
					"We bask in the glow of your races noble presence.",
				},
				info_exchange= {
					"Pray enlighten us with your gems of infinite wisdom.",
					"We want to bathe in your ever spewing fountain of knowledge.",
				}
			},
			plea= {
				"Please do not harm us oh most high and mighty.",
				"We truly are not worth your trouble to destroy.",
				"Please do not blast us into atomic particles.",
				"We understand that you could destroy us if you chose. I beg you not to do this.",
			}
		},

		friendly= {
			comment= {
				trust= {
					"We come in peace from Myrrdan, please trust me.",
					"Our goal is to strengthen the relationship between our peoples.",
					"Perhaps some day our young shall play and romp together in the blissful light of harmony and friendship.",
				},
				info_exchange= {
					"There is no limit to what both our races can gain from mutual exchange.",
					"We look forward to discussion of our respective histories.",
				}
			},
			remark= {
				ship= {
					"Your ship appears very irregular.",
					"Your ship seems to be very elaborate.",
					"Your ship seems to be very powerful.",
				},
				alien= {
					"It is a pleasure to speak with a friendly race such as yours.",
					"The [ALIEN] show both wisdom and discretion."
				}
			}
		},

		hostile= {
			comment= {
				general= {
					"We suggest you comply with our demands.",
					"You are making a tragic mistake.",
					"Resistance is futile. All your resources is belong to us!"
				},
				info_exchange= {
					"We demand you supply us with information.",
				}
			},
			remark= {
				ship= {
					"Your ship looks like a flying garbage scow.",
					"So, do you use tinfoil in all your ship designs?",
					"Your ship will look less aesthetically pleasing with laser burns on its hull.",
					"It's surprising to see a ship like yours anywhere outside a shipyard- or junk pile.",
					"Your ship is over-embellished and weak."
				},
				alien= {
					"What an ugly and worthless creature.",
					"The [ALIEN] are vile and weak.",
					"Your worthless race is beneath contempt."
				}
			},
			surrender_demand= {
				"Lower your shields and surrender, or face the consequences!",
				"We require immediate compliance: surrender!",
				"We will destroy you unless you submit to our demands."
			},
		},
	}

	preQuestion= {
		info= {
			obsequious= {
				"We beg you to honor our poor selves and bestow upon us a pearl of wisdom",
				"We humbly suggest that we would be interested in any little thing you might wish to tell us",
				"Mighty [ALIEN], take pity on us ignorant wretches and tell us",
			},
			friendly= {
				"Do you have any information you can share with us",
				"We would greatly appreciate some information",
				"Is there anything you can tell us",
				"We are interested in information",
				"Would you please tell us",
			},
			hostile= {
				"We require information",
				"You will tell us",
				"Our laser batteries are fully charged. I suggest you tell us",
				"Our tactical officer has a twitchy trigger finger. Give us information",
			},
		},
		desire= {
			obsequious= {
				"Noble and wise [ALIEN], take pity on our ignorant race and trade us",
			},
			friendly= {
				"We would sincerely appreciate it if you could trade us",
				"Would it be possible for you to trade us",
				"Can you trade us",
			},
			hostile= {
				"If you do not wish for some laser burns on your ship's hull, I suggest you give us",
				"We demand that you immediately hand over to us",
			},
		},
	}
end
------------------------------------------------------------------------
-- OBSEQUIOUS DIALOGUE -------------------------------------------------
------------------------------------------------------------------------
function ObsequiousDialogue()
	--add as many greetings as you want and one will be chosen randomly
	--VALID ACTIONS: terminate, attack


if (plot_stage == 1) then

	obsequiousGreetTable= {
		"We are the ancient and mighty Uhlek.  Bow and pay us homage!  Snicker, snicker.",
		"We ain't your father's Bar-zhon.",
		"Us is stupid alien the most delightful.  Now it is the message leave at the beep.",
		"Haha!  That is a laugh!  Don't try to make a chant, you berk!",
		"Silly humans, tricks are for us!",
		"Sure, we believe you.  Try pulling the wool over the goat somewhere else."
	}

elseif (plot_stage == 2) then

	obsequiousGreetTable= {
		"We are the powerful Leghk.  Upload your medical database if you want to live.",
		"We are the Bar-zhon Imperial guard.  Pay respects to us because you are among royalty.",
		"Drifting, dashing, dreaming.  We are the eternal ones. You will immediately transmit your database to us in homage.",
		"Haha!  That is a laugh!  Don't try to make a chant, you berk!",
	}

elseif (plot_stage == 3) then

	obsequiousGreetTable= {
		"We are the ancient and mighty Uhlek.  Bow and pay us homage!  Snicker, snicker.",
		"We ain't your father's Bar-zhon.",
		"Us is stupid alien the most delightful.  Now it is the message leave at the beep.",
		"Haha!  That is a laugh!  Don't try to make a chant, you berk!",
		"Silly humans, tricks are for us!",
		"Sure, we believe you.  Try pulling the wool over the goat somewhere else."
	}


elseif (plot_stage == 4) then

	obsequiousGreetTable= {
		"We are the ancient and mighty Uhlek.  Bow and pay us homage!  Snicker, snicker.",
		"We ain't your father's Bar-zhon.",
		"Us is stupid alien the most delightful.  Now it is the message leave at the beep."
	}

end


	greetings[1] = {
		action="",
		player="Hail oh mighty ones, masters of the universe.",
		alien= 	obsequiousGreetTable }
	greetings[2] = {
		action="",
		player="We bow to your wonderful magnificence and ask that you do not harm our insignificant selves.",
		alien= obsequiousGreetTable	}
	greetings[3] = {
		action="",
		player="Greetings oh highest of the high most great alien beings.",
		alien= obsequiousGreetTable }
	greetings[4] = {
		action="",
		player="We respectfully request that you identify your vastly superior selves.",
		alien= obsequiousGreetTable }
	greetings[5] = {
		action="",
		player="We humbly suggest that you may wish to identify yourselves. If not, that is perfectly o.k.",
		alien= obsequiousGreetTable }
	greetings[6] = {
		action="",
		player="Please do not harm us oh most high and mighty.",
		alien= obsequiousGreetTable }
	greetings[7] = {
		action="",
		player="Greetings and felicitations oh kind and merciful alien.",
		alien= obsequiousGreetTable }
	greetings[8] = {
		action="",
		player="Please do not blast us into atomic particles.  Take pity on us who are not fit to grovel in your waste products.",
		alien= obsequiousGreetTable }
	greetings[9] = {
		action="",
		player="We can see that you are indeed the true race.  Pray enlighten us with your gems of infinite wisdom.",
		alien= obsequiousGreetTable }
	greetings[10] = {
		action="",
		player="We truly are not worth your trouble to destroy.",
		alien= obsequiousGreetTable }
	greetings[11] = {
		action="",
		player="We want to bathe in your ever spewing fountain of knowledge.",
		alien= obsequiousGreetTable }
	greetings[12] = {
		action="",
		player="We understand that you could destroy us if you chose. I beg you not to do this.",
		alien= obsequiousGreetTable }

end

------------------------------------------------------------------------
-- FRIENDLY DIALOGUE ---------------------------------------------------
------------------------------------------------------------------------
function FriendlyDialogue()
	--add as many greetings as you want and one will be chosen randomly
	--VALID ACTIONS: terminate, attack

if (plot_stage == 1) then

	greetings[1] = {
		action="",
		player="Greetings from the planet Myrrdan. We come in peace...usually.",
		alien={"Greetings from your friendly terrorist cell.  We rarely come in peace, and even more rarely go in pieces."} }
	greetings[2] = {
		action="",
		player="I am Captain [CAPTAIN] of the starship [SHIPNAME].",
		alien={"This is Captain anonymous of the transport warship anonymous.  You won't be going now."} }
	greetings[3] = {
		action="",
		player="Hi there. How are ya? I'm Captain [CAPTAIN]. We're peaceful explorers.",
		alien={"What do you want?"} }
	greetings[4] = {
		action="",
		player="Dude, that is one odd ship you have there!",
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[5] = {
		action="",
		player="How's it going, umm ... aren't you Bar-zhon?",
		alien={"Nope."} }
	greetings[6] = {
		action="",
		--player="Your ship seems to be very powerful.",
		player= playerStatement.friendly.remark.ship[3], -- (could join ship[2] in table)
		alien={"Why thank you kind sir or madame.  Your ship doesn't."} }
	greetings[7] = {
		action="",
		--player="Your ship appears very irregular.",
		player= playerStatement.friendly.remark.ship[1], --(could join ship[2 & 3] in table)
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[8] = {
		action="",
		--player="We come in peace from Myrrdan, please trust me.",
		player= playerStatement.friendly.comment.trust[1], -- (could join trust[2])
		alien={"Oh we believe you, believe me that we believe you."} }
	greetings[9] = {
		action="",
		--player="There is no limit to what both our races can gain from mutual exchange.",
		player= playerStatement.friendly.comment.info_exchange[1], --answer needs this one.
		alien={"I would wholeheartedly agree, as long as to exchange is kept in our direction."} }
	greetings[10] = {
		action="",
		--player="Perhaps some day our young shall play and romp together in the blissful light of harmony and friendship.",
		player= playerStatement.friendly.comment.trust[3], --answer needs this one :)
		alien={"How did you come up with a whopper like that one?"} }

--[[ So using the categorized table of statements with the minimal expansion I put in, #possibilities
   rises by ~ x2. If multiple alien responses here are put into the response table (like is done with
   the obsequious greetings: could imagine answering statements[1] with both the alien response
   to statements[1] and the response to statements[2] for an additional 2 possibilities, and the
   response to statements[3] could be added to the response to statements[4] for another 2 possibilities.
   So 5 -> 13 possibilites altogether in this little example.
--]]

elseif (plot_stage == 2) then

	greetings[1] = {
		action="",
		player="Greetings from the planet Myrrdan. We come in peace...usually.",
		alien={"Greetings from your friendly terrorist cell.  We rarely come in peace, and even more rarely go in pieces."} }
	greetings[2] = {
		action="",
		player="I am Captain [CAPTAIN] of the starship [SHIPNAME].",
		alien={"This is Captain anonymous of the transport warship anonymous.  You won't be going now."} }
	greetings[3] = {
		action="",
		player="Hi there. How are ya? I'm Captain [CAPTAIN]. We're peaceful explorers.",
		alien={"Hi yourself, we're not."} }
	greetings[4] = {
		action="",
		player="Dude, that is one odd ship you have there!",
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[5] = {
		action="",
		player="How's it going, umm ... aren't you Bar-zhon?",
		alien={"Nope."} }
	greetings[6] = {
		action="",
		--player="Your ship seems to be very powerful.",
		player= playerStatement.friendly.remark.ship[3], -- (could join ship[2] in table)
		alien={"Why thank you kind sir or madame.  Your ship doesn't."} }
	greetings[7] = {
		action="",
		--player="Your ship appears very irregular.",
		player= playerStatement.friendly.remark.ship[1], --(could join ship[2 & 3] in table)
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[8] = {
		action="",
		--player="We come in peace from Myrrdan, please trust me.",
		player= playerStatement.friendly.comment.trust[1], -- (could join trust[2])
		alien={"Oh we believe you, believe me that we believe you."} }
	greetings[9] = {
		action="",
		--player="There is no limit to what both our races can gain from mutual exchange.",
		player= playerStatement.friendly.comment.info_exchange[1], --answer needs this one.
		alien={"I would wholeheartedly agree, as long as to exchange is kept in our direction."} }
	greetings[10] = {
		action="",
		--player="Perhaps some day our young shall play and romp together in the blissful light of harmony and friendship.",
		player= playerStatement.friendly.comment.trust[3], --answer needs this one :)
		alien={"How did you come up with a whopper like that one?"} }


elseif (plot_stage == 3) then

	greetings[1] = {
		action="",
		player="Greetings from the planet Myrrdan. We come in peace...usually.",
		alien={"Greetings [CAPTAIN].  Coalition interceptor responding to your hail."} }
	greetings[2] = {
		action="",
		player="I am Captain [CAPTAIN] of the starship [SHIPNAME].",
		alien={"This is Captain anonymous of the Coalition.  Responding to your hail."} }
	greetings[3] = {
		action="",
		player="Hi there. How are ya? I'm Captain [CAPTAIN]. We're peaceful explorers.",
		alien={"Hi [CAPTAIN].  Times are indeed troubling yet profitable.  Any news on this sickness?."} }
	greetings[4] = {
		action="",
		player="Dude, that is one odd ship you have there!",
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[5] = {
		action="",
		player="How's it going, umm ... aren't you Bar-zhon?",
		alien={"Nope, Coalition."} }
	greetings[6] = {
		action="",
		--player="Your ship seems to be very powerful.",
		player= playerStatement.friendly.remark.ship[3], -- (could join ship[2] in table)
		alien={"Why thank you kind sir or madame.  Your ship doesn't."} }
	greetings[7] = {
		action="",
		--player="Your ship appears very irregular.",
		player= playerStatement.friendly.remark.ship[1], --(could join ship[2 & 3] in table)
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[8] = {
		action="",
		--player="We come in peace from Myrrdan, please trust me.",
		player= playerStatement.friendly.comment.trust[1], -- (could join trust[2])
		alien={"Oh we believe you, believe me that we believe you."} }
	greetings[9] = {
		action="",
		--player="There is no limit to what both our races can gain from mutual exchange.",
		player= playerStatement.friendly.comment.info_exchange[1], --answer needs this one.
		alien={"I would wholeheartedly agree, as long as to exchange is kept in our direction."} }
	greetings[10] = {
		action="",
		--player="Perhaps some day our young shall play and romp together in the blissful light of harmony and friendship.",
		player= playerStatement.friendly.comment.trust[3], --answer needs this one :)
		alien={"How did you come up with a whopper like that one?"} }


elseif (plot_stage == 4) then

	greetings[1] = {
		action="",
		player="Greetings from the planet Myrrdan. We come in peace...usually.",
		alien={"Ho Humans of the [SHIPNAME]."} }
	greetings[2] = {
		action="",
		player="I am Captain [CAPTAIN] of the starship [SHIPNAME].",
		alien={"Welcome [SHIPNAME].  Coalition representative responding to your hail."} }
	greetings[3] = {
		action="",
		player="Hi there. How are ya? I'm Captain [CAPTAIN]. We're peaceful explorers.",
		alien={"Hi [CAPTAIN].  We of the Coalition know the [SHIPNAME]."} }
	greetings[4] = {
		action="",
		player="Dude, that is one odd ship you have there!",
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries."} }
	greetings[5] = {
		action="",
		player="How's it going, umm ... aren't you Bar-zhon?",
		alien={"Nope, Coalition.  Welcome Humans of the [SHIPNAME]."} }
	greetings[6] = {
		action="",
		--player="Your ship seems to be very powerful.",
		player= playerStatement.friendly.remark.ship[3], -- (could join ship[2] in table)
		alien={"Why thank you kind sir or madame.  It's an honor.."} }
	greetings[7] = {
		action="",
		--player="Your ship appears very irregular.",
		player= playerStatement.friendly.remark.ship[1], --(could join ship[2 & 3] in table)
		alien={"Why thank you very much.  It is my own custom model transport freighter outfitted with afterburners and state-of-the-art weaponry just ready to slice and dice and even make julian fries better then any Thrynn black box."} }
	greetings[8] = {
		action="",
		--player="We come in peace from Myrrdan, please trust me.",
		player= playerStatement.friendly.comment.trust[1], -- (could join trust[2])
		alien={"Ya did prove yourself already.  Welcome [SHIPNAME]."} }
	greetings[9] = {
		action="",
		--player="There is no limit to what both our races can gain from mutual exchange.",
		player= playerStatement.friendly.comment.info_exchange[1], --answer needs this one.
		alien={"Good deal.  Just ask."} }
	greetings[10] = {
		action="",
		--player="Perhaps some day our young shall play and romp together in the blissful light of harmony and friendship.",
		player= playerStatement.friendly.comment.trust[3], --answer needs this one :)
		alien={"Umm, sure."} }

end

end
------------------------------------------------------------------------
-- HOSTILE DIALOGUE ----------------------------------------------------
------------------------------------------------------------------------
function HostileDialogue()
	--add as many player greetings as you want and one will be chosen randomly
	--VALID ACTIONS: terminate, attack

if (plot_stage == 1) or (plot_stage == 2) or (plot_stage == 3) then

	greetings[1] = {
		action="attack",
		player="This is captain [CAPTAIN] of the starship [SHIPNAME]. Identify yourselves immediately or be destroyed.",
		alien={"Not likely, on either count."} }
	greetings[2] = {
		action="attack",
		player="This is the starship [SHIPNAME]. We are heavily armed.",
		alien={"So are we."}	}
	greetings[3] = {
		action="",
		player="This is captain [CAPTAIN] of the powerful starship [SHIPNAME]. ",
		alien={"Not just a spaceship, but a starship now is it..."} }
	greetings[4] = {
		action="attack",
		player="You will cooperate and identify yourselves immediately or be annihilated.",
		alien={"No we won't"} }
	greetings[5] = {
		action="attack",
		player="We require information. Comply or be destroyed.",
		alien={"We will not comply, but we will gladly perform the destroying."} }
	greetings[6] = {
		action="attack",
		player="Your ship is over-embellished and weak.",
		alien={"Over embellished, maybe.  Weak, never.  Here, let me demonstrate."} }
	greetings[7] = {
		action="attack",
		player="What an ugly and worthless creature.",
		alien={"I'm sorry I can't seem to hear you.  Please boost your gain knob.  Nevermind, we'll just get closer and call you right back."} }
	greetings[8] = {
		action="attack",
		player="Your ship looks like a flying garbage scow.",
		alien={"Incredible!  How could I have not noticed this?  Unfortunately our cargo bays are empty of garbage.  If you would not mind, we would greatly appreciate it if you could scrap your ship for us to haul."} }


elseif (plot_stage == 4) then

	greetings[1] = {
		action="attack",
		player="This is captain [CAPTAIN] of the starship [SHIPNAME]. Identify yourselves immediately or be destroyed.",
		alien={"If you want a romp, we're happy to accommodate."} }
	greetings[2] = {
		action="attack",
		player="This is the starship [SHIPNAME]. We are heavily armed.",
		alien={"Ohh, feel free to demonstrate."}	}
	greetings[3] = {
		action="",
		player="This is captain [CAPTAIN] of the powerful starship [SHIPNAME].",
		alien={"This powerful Coalition interceptor is also well-armed."} }
	greetings[4] = {
		action="attack",
		player="You will cooperate and identify yourselves immediately or be annihilated.",
		alien={"Why not just ask to tangle things up?  Here we are."} }
	greetings[5] = {
		action="attack",
		player="We require information. Comply or be destroyed.",
		alien={"We will not comply."} }
	greetings[6] = {
		action="attack",
		player="Your ship is over-embellished and weak.",
		alien={"Over embellished, maybe.  Weak, never.  Here, let me demonstrate."} }
	greetings[7] = {
		action="attack",
		player="What an ugly and worthless creature.",
		alien={"I'm sorry I can't seem to hear you.  Please boost your gain knob.  Nevermind, we'll just get closer and call you right back."} }
	greetings[8] = {
		action="attack",
		player="Your ship looks like a flying garbage scow.",
		alien={"Incredible!  How could I have not noticed this?  Unfortunately our cargo bays are empty of garbage.  If you would not mind, we would greatly appreciate it if you could scrap your ship for us to haul."} }

end

end

function StandardQuestions()

	--player questions / alien responses
	--VALID ACTIONS: terminate, attack, restart
	--YOURSELVES THREAD

if (plot_stage == 1) then

	questions[10000] = {
		action="jump", goto=11001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about yourselves",
		alien={"The Coalition was created because the Bar-zhon empire survives on slave labor, more the fools they are to provide us with constant recruits.  From the millions of Sabion, Bx, and Transmodra, a few escape to join us every day.  None of them possess the coordination and flying skills of those of us of the Bar-zhon race, but at least here they are treated right."}
	}
	questions[20000] = {
		action="jump", goto=21001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the other races in the galaxy",
		alien={"Ehh?  We know a bit concerning a few of the races in the sector.  Are you interested in the Tafel, the Nyssian, the Minex, the Bar-zhon, the Elowan, the Thrynn, or other pirates?"}
	}
	questions[30000] = {
		action="jump", goto=31001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the past",
		alien={"The past?  Go bother the Nyssian about the past.  Feel free to also pester the Bar-zhon, they tell a decently honest history story even though it's slanted their way a bit."}
	}
	questions[40000] = {
		action="jump", goto=41001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the Ancients",
		alien={"Fah!  I guess I could scoop the dirt on a few endurium planets." }
	}

elseif (plot_stage == 2) then -- virus plot state

	questions[10000] = {
		action="jump", goto=60000,
		player="[AUTO_REPEAT]",
		playerFragment="about yourselves",
		alien={"Your resources are badly needed in these dark days.  Surrender now or we will take what we need."}
	}
	questions[20000] = {
		action="jump", goto=60000,
		player="[AUTO_REPEAT]",
		playerFragment="about the other races in the galaxy",
		alien={"Your resources are badly needed in these dark days.  Surrender now or we will take what we need."}
	}
	questions[30000] = {
		action="jump", goto=60000,
		player="[AUTO_REPEAT]",
		playerFragment="about the past",
		alien={"Your resources are badly needed in these dark days.  Surrender now or we will take what we need."}
	}
	questions[40000] = {
		action="jump", goto=60000,
		player="[AUTO_REPEAT]",
		playerFragment="about the Ancients",
		alien={"Your resources are badly needed in these dark days.  Surrender now or we will take what we need."}
	}
	questions[50000] = {
		action="jump", goto=60000,
		player="[AUTO_REPEAT]",
		playerFragment="...",
		alien={"Your resources are badly needed in these dark days.  Surrender now or we will take what we need."}
	}


elseif (plot_stage == 3) then -- war plot state

	questions[10000] = {
		action="jump", goto=11001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about yourselves",
		alien={"The Coalition was created because the Bar-zhon empire survives on slave labor, more the fools they are to provide us with constant recruits.  From the millions of Sabion, Bx, and Transmodra, a few escape to join us every day.  The Bar-zhon's occupied workers are currently being grabbed up through their Empire to staff warships that are no more than death traps. The plague is taking ship after ship if the Minex don't get them first."}
	}
	questions[20000] = {
		action="jump", goto=21001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the other races in the galaxy",
		alien={"Ehh?  We know a bit concerning a few of the races in the sector.  More of us now in contact to survive the current onslaught.  Are you interested in the Tafel, the Nyssian, the damnable Minex, the Bar-zhon, the Elowan, the Thrynn, or other pirates?"}
	}
	questions[30000] = {
		action="jump", goto=1, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the past",
		alien={"The past?  Go bother the Nyssian about the past.  Feel free to also pester the Bar-zhon.  We are more concerned with the Minex invasion at the moment."}
	}
	questions[40000] = {
		action="jump", goto=41001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the Ancients",
		alien={"Fah!  I guess I could scoop the dirt on a few endurium planets.  Travel is difficult enough with all of these rampaging Minex about." }
	}


elseif (plot_stage == 4) then -- ancients plot state

	questions[10000] = {
		action="jump", goto=11001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about yourselves",
		alien={"The Coalition was created because the Bar-zhon empire survives on slave labor.  more the fools they are to provide us with constant recruits.  Now that the Infected Ones have picked up rampaging where the Minex left off, just about all of the remaining sane independents are coming to us for protection."}
	}
	questions[20000] = {
		action="jump", goto=21001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the other races in the galaxy",
		alien={"Ehh?  We know a bit concerning a few of the races in the sector, the ones that are still sane and haven't joined the men rampaging ones yet. Are you interested in the Tafel, the Nyssian, the Minex, the Bar-zhon, the Elowan, the Thrynn, or other pirates?"}
	}
	questions[30000] = {
		action="jump", goto=31001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the past",
		alien={"The past?  Go bother the Nyssian about the past.  Feel free to also pester the Bar-zhon. We are more worried about the present with all of the mad ones getting organized and coordinating their attacks."}
	}
	questions[40000] = {
		action="jump", goto=41001, ftest= 1,
		player="[AUTO_REPEAT]",
		playerFragment="about the Ancients",
		alien={"Fah!  I guess I could scoop the dirt on a few endurium planets.  No one else is traveling with the growing number of infected ships out there." }
	}


end

if (plot_stage == 1) then

	questions[50000] = {
		action="branch",
		choices = {
			{ text="Coalition home base location",  goto=51000 },
			{ text="<Back>",  goto=1 }
		}
	}

	questions[51000] = {
		action="jump", goto=1, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="where your home base is", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Don't you be thinking that we'd be some simple dolts now, you hear?"}
	}
end

if (plot_stage == 1) then

	questions[11001] = {
		action="branch",
		choices = {
			{ text="Why rebel?",  goto=11000 },
			{ text="Sabion, Bx, and Transmodra", goto=12000 },
			{ text="Your species", goto=13000 },
			{ text="Objectives of the Coalition", goto=14000 },
			{ text="<Back>", goto=1 }
		}
	}
	questions[11000] = {
		action="jump", goto=11001,
		player="[AUTO_REPEAT]",
		playerFragment="about why members of the Bar-zhon race rebel against them", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Because our fabulous government is a military dictatorship, and if you're not born into a family of noble birth, you are shuffled into the technician or worker caste and  given orders the rest of your life.  You have no chance to take a role in either the government or the military.  Our rebellion is as much from a desire to live in freedom as it is for anything else." }
	}

elseif (plot_stage == 3) or (plot_stage == 4) then

	questions[11001] = {
		action="branch",
		choices = {
			{ text="Why rebel?",  goto=11000 },
			{ text="Sabion, Bx, and Transmodra", goto=12000 },
			{ text="How has the virus affected you guys?", goto=13000 },
			{ text="What about the Minex warfare?", goto=14000 },
			{ text="<Back>", goto=1 }
		}
	}
	questions[11000] = {
		action="jump", goto=11101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="about why members of the Bar-zhon race rebel against them", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Because our fabulous government is a military dictatorship.  Technological advances threatened to make everyone's lives too convenient and then who would need government?  First private ownership of ships were outlawed, then weeapons, then the banks were taken over, and finally they dictated everything." }
	}
	questions[11101] = {
		action="branch",
		choices = {
			{ text="Inquire about Bar-zhon news", goto=11110 },
			{ text="Hostile coalition ships", goto=11120 },
			{ text="<Back>", goto=11001 }
		}
	}
end

if (plot_stage == 3) then

	questions[11110] = {
		action="jump", goto=11101,
		player="[AUTO_REPEAT]",
		playerFragment="how the virus is affecting the Bar-zhon",
		alien={"The Bar-zhon government is getting what they deserve.  This infection has decimated their control.  More rats than ever before are leaving a sinking ship and swelling our ranks daily." }
	}
	questions[11120] = {
		action="jump", goto=11121,
		player="[AUTO_REPEAT]",
		playerFragment="why your ships were hostile until recently",
		alien={"Sorry about that small misunderstanding.  New race appears, mysterious plague appears soon afterwards?  Looks like the Minex were behind it the whole time, softening everyone up before the invasion. " }
	}
	questions[11121] = {
		action="jump", goto=11101,
		player="Didn't you say that you needed resources?", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Actually we did say that, didn't we?  I guess that onslaught of Minex changed the situation around a bit and living allies are a bit more valuable now." }
	}

elseif (plot_stage == 4) then

	questions[11110] = {
		action="jump", goto=11101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="how the virus affected the Bar-zhon",
		alien={"The Bar-zhon society has mostly shut down.  Rebellion and madness are increasing in intensity in their society, leaving them paralyzed and unable to stop us." }
	}
	questions[11120] = {
		action="jump", goto=11121,
		player="[AUTO_REPEAT]",
		playerFragment="why your ships were hostile until recently",
		alien={"Sorry about that small misunderstanding.  New race appears, mysterious plague appears soon afterwards?  It appeared like the Minex were behind it at first but who knows now? " }
	}
	questions[11121] = {
		action="jump", goto=11101,
		player="[AUTO_REPEAT]",
		playerFragment="if you didn't say that you needed resources", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Actually we did say that, didn't we?  I guess that onslaught of Minex and Infected changed the situation around a bit and living allies are more valuable now." }
	}

end


if (plot_stage == 1) then
	questions[14000] = {
		action="jump", goto=14101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="on your goals",
		alien={"To survive by blending into the Bar-Zhon empire as best possible.  Our military and technological capacities have grown much further than the Bar-Zhon suspect.  Oh I don't care if you are a Bar-Zhon infiltrator or sympathizer, they already disbelieve their own reports and think we are only shortsighted revolutionaries." }
	}
	questions[14110] = {
		action="jump", goto=14101,
		player="[AUTO_REPEAT]",
		playerFragment="why your ships attack the Bar-zhon",
		alien={"Maintenance of the status quo.  If we stopped attacking they would get worried and to start to investigate.  If we declared all-out war there is no guarantee we would win.  Guerrilla attacks keep the mighty implacable and inflexible Bar-zhon Navy busy while we make progress elsewhere." }
	}
elseif (plot_stage == 3) then

	questions[14000] = {
		action="jump", goto=14101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="about the Minex warfare",
		alien={"These Minex buggers are causing a bit of a pinch.  They are chasing down and attacking isolated forces away from well defended worlds.  We're surviving primarily through our afterburners and are fortunate they seem to currently ignore planets.  We believe that the Minex homeworld to be located somewhere within the Pearl cluster.  Find a way to stop them, will ya?" }
	}
	questions[14110] = {
		action="jump", goto=14101,
		player="[AUTO_REPEAT]",
		playerFragment="why your ships attack the Bar-zhon",
		alien={"Our forces are currently being caught between the Bar-zhon Navy and the Minex demons.  Combat is inevitable when you run out of room." }
	}
elseif (plot_stage == 4) then

	questions[14000] = {
		action="jump", goto=14101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="about the current Minex warfare",
		alien={"Heard your race was responsible for putting the brakes on the Minex.  Shame they didn't have a cure tucked away somewhere." }
	}

	questions[14110] = {
		action="jump", goto=11001,
		player="[AUTO_REPEAT]",
		playerFragment="why your ships attack the Bar-zhon",
		alien={"We aren't attacking anyone.  With the Minex gone and the Bar-zhon paralyzed we are finally getting around to long overdue strategic positioning of our own." }
	}

end

if (plot_stage == 1) or (plot_stage == 3) or (plot_stage == 4) then
	questions[12000] = {
		action="jump", goto=12101,
		player="[AUTO_REPEAT]",
		playerFragment="about the Sabion, Bx, and Transmodra", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Losers of the last great war.  Their three home worlds were razed and made entirely uninhabitable, but radiation levels have finally started to drop to the point where those with decent equipment can explore their planets without kneeling over instantly from Delta radiation." }
	}


	questions[12101] = {
		action="branch",
		choices = {
			{ text="Home world of the Sabion",  goto=12110 },
			{ text="Home world of the Bx",  goto=12120 },
			{ text="Home world of the Transmodra",  goto=12130 },
			{ text="<Back>", goto=11001 }
		}
	}
end

if (plot_stage == 1) then
	questions[13000] = {
		action="jump", goto=11001,
		player="[AUTO_REPEAT]", ftest= 3, -- aggravating
		playerFragment="about your biology",
		alien={"Kaak!  Go bother someone who cares!" }
	}
	questions[12110] = {
		action="jump", goto=12101, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="the location of the home world of the Sabion", fragmentTable=preQuestion.desire,
		alien={"Not so hasty grave robbers.  Don't expect to fool us with your platitudes either." }
	}
	questions[12120] = {
		action="jump", goto=12101, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="the location of the home world of the Bx", fragmentTable=preQuestion.desire,
		alien={"The superb ground pounders.  Their headquarters at 58N X 96E was nigh impregnable, unfortunately none of their low level dueling abilities helped them in space. However don't be expecting us to assist grave robbers loot their world." }
	}
	questions[12130] = {
		action="jump", goto=12101, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="the location of the home world of the Transmodra", fragmentTable=preQuestion.desire,
		alien={"The mighty industrialists, oh how they fell quickly to deception.  We will not be the ones opening their world to scavengers." }
	}

	questions[14101] = {
		action="branch",
		choices = {
			{ text="Why attack the Bar-zhon?",  goto=14110 },
			{ text="Not revolutionaries?",  goto=14120 },
			{ text="Leave Bar-zhon space",  goto=14130 },
			{ text="<Back>", goto=11001 }
		}
	}

	questions[14120] = {
		action="jump", goto=14101, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="what are you if not revolutionaries",
		alien={"<Sigh>   I told you already you slow alien.  We want to live in freedom, not die in war.  Beyond that our aims are our own." }
	}
	questions[14130] = {
		action="jump", goto=14101, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="why you don't simply leave Bar-zhon space",
		alien={"Commit a mass exodus of population under the noses of a hostile force?  Expose every single ship and resource we have to counterattack?  Simply ask to remove a slave population and see if the slave masters let them go willingly?  I assume you see the problems by now." }
	}
elseif (plot_stage == 3) or (plot_stage == 4) then

	questions[13000] = {
		action="jump", goto=11001,
		player="[AUTO_REPEAT]",
		playerFragment="about how the virus has affected you guys",
		alien={"Quite the death toll until recently, however the death and madness hit everyone hard.  Keeping everyone away from each other and in isolated lockdown has limited the madness and almost stopped the death toll for the short-term." }
	}

	questions[12110] = {
		player="[AUTO_REPEAT]",
		playerFragment="on the location of the home world of the Sabion", fragmentTable=preQuestion.desire,
		alien={"Gorias 3 - 5,16.  Their primary research station was located at their temperate northern pole of the planet." }
	}
	questions[12120] = {
		action="jump", goto=12101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="the location of the home world of the Bx", fragmentTable=preQuestion.desire,
		alien={"Cian 3 - 25,205" }
	}
	questions[12130] = {
		action="jump", goto=12101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="the location of the home world of the Transmodra", fragmentTable=preQuestion.desire,
		alien={"Dian Cecht 4 - 35,139.  Bar-zhon scavengers are searching all these worlds.  Beat them to whatever they are after, ok?" }
	}

	questions[14101] = {
		action="branch",
		choices = {
			{ text="Attacking the Bar-zhon",  goto=14110 },
			{ text="Stopping the Minex",  goto=14120 },
			{ text="Minex motivations",  goto=14130 },
			{ text="<Back>", goto=11001 }
		}
	}

	questions[14120] = {
		action="jump", goto=14101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="how to stop the Minex", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"If we knew how we would have already done so.  You're new around here and still fairly neutral.  Talk to everyone else and put some ideas together." }
	}
	questions[14130] = {
		action="jump", goto=14101, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="why the Minex have declared war", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Nope, but probably it is that those guys are just way too isolated and paranoid.  Maybe with all their advanced tech they just decided they could wipe us out with their biologics and their warships." }
	}

end

if (plot_stage == 1) or (plot_stage == 3) or (plot_stage == 4) then
	questions[21001] = {
		action="branch",
		choices = {
			{ text="The Bar-zhon",  goto=21000 },
			{ text="The Tafel",  goto=22000 },
			{ text="The Nyssian",  goto=23000 },
			{ text="<More>",  goto=21002 },
			{ text="<Back>", goto=1 }
		}
	}
	questions[21002] = {
		action="branch",
		choices = {
			{ text="The Minex",  goto=24000 },
			{ text="The Thrynn and Elowan",  goto=25000 },
			{ text="The Spemin",  goto=26000 },
			{ text="Other pirates",  goto=27000 },
			{ text="<Back>", goto=1 }
		}
	}
	questions[21000] = {
		action="jump", goto=21001,
		player="[AUTO_REPEAT]",
		playerFragment="about the Bar-zhon",
		alien={"Ahh, the Bar-zhon.  Our favorite pals.  Their warships are a mite tough, but not too difficult to take down.  Missile barrages from a decent distance take them out easily enough.  If your ship is fast enough and your pilot skilled enough, keep in mind that all of their ships have only missile weapons and no close quarter lasers." }
	}
	questions[22000] = {
		action="jump", goto=22100,
		player="[AUTO_REPEAT]",
		playerFragment="about the Tafel",
		alien={"The Tafel are interesting lot.  Quite adaptive they have proven to be yet strangely unable to see the benefits of profitable ventures.  If you ever get in a scrape with them make sure you never leave a damaged or disabled ship behind you - those suckers have a uncanny ability to repair their ships faster than your shields can regenerate." }
	}
	questions[22100] = {
		action="jump", goto=21001,
		player="[AUTO_REPEAT]",
		alien={"Their ships have recently become aggressive, refusing to establish contact with outsiders,  This is not a problem since their ships are very weak yet quite dangerous in large numbers.  Fortunately a single Tafel ship has weak shields and paper thin armor.  They blow quite nicely." }
	}
	questions[23000] = {
		action="jump", goto=21001,
		player="[AUTO_REPEAT]",
		playerFragment="about the Nyssian",
		alien={"Arrogant Nyssian wanderers travel alone in their weird organic vessels.  There vessels always travel alone and have very weak weaponry.  They make good target practice with missiles if you want to shoot something more difficult than a rock, but their ships are made up virtually no salvageable material, and they're strangely effective shields take quite a beating, and no one has ever salvaged or reverse engineered them." }
	}

	questions[25000] = {
		action="jump", goto=25100,
		player="[AUTO_REPEAT]",
		playerFragment="about the Elowan",
		alien={"The Elowan be a strange folk.  Transmitted genetic memories make them impossible to tame, even when grown from seed.  Their little petty conflict with the Thrynn has undergone shifts in fortune many a time but currently they be on the losing side.  They have just recently developed some strange laser reflective armor which makes their ships highly resistant to laser damage." }
	}
	questions[25100] = {
		action="jump", goto=21002,
		player="[AUTO_REPEAT]",
		playerFragment="about the Thrynn",
		alien={"The Thrynn are a nasty sort.  Endless warfare has ground down their ships and resources but they are nasty and tenacious, and refuse to ever surrender or give up a fight.  Unless you're capable of fighting off an empire for the next hundred years it's best not to mess with them.  Their ships are well rounded, recently added missile technology balancing out their powerful laser batteries." }
	}
	questions[27000] = {
		action="jump", goto=21002,
		player="[AUTO_REPEAT]",
		playerFragment="about other pirates",
		alien={"General outlaws and pirates tend to inhabit the center of this sector.  Isn't this where you guys came from by the way?  Their equipment is patchy and badly worn, and they are not seriously a threat to anyone except the weakest merchant vessel." }
	}
end

if (plot_stage == 1) then

	questions[24000] = {
		action="jump", goto=24100,
		player="[AUTO_REPEAT]",
		playerFragment="about the Minex",
		alien={"The Minex are too much trouble, yet some fool always tries to go after them to prove themselves.  The few that return often salvage amazing technologies and are highly respected.   Of course ones so daft are often raided themselves when they return, just in case they happened to have some amazing technologies." }
	}

	questions[24100] = {
		action="jump", goto=21002,
		player="[AUTO_REPEAT]",
		alien={"If you try to tangle with those blokes, keep to your lasers.  Some blasted energy field diverts missile explosions away from them, making your missiles much less effective.  Their weapons pack a tremendous punch." }
	}

	questions[26000] = {
		action="jump", goto=21002,
		player="[AUTO_REPEAT]",
		playerFragment="about the Spemin.",
		alien={"Strange rambling blob-like creatures?  Don't bother.  Their tech is trash and they don't know anything.  Fun target practice however." }
	}


end

if (plot_stage == 1) or (plot_stage == 3) or (plot_stage == 4) then
	questions[31001] = {
		action="branch",
		choices = {
			{ text="Coalition Foundation",  goto=31002 },
			{ text="<Back>", goto=1 }
		}
	}
	questions[31002] = {
		action="jump", goto=1,
		player="[AUTO_REPEAT]",
		playerFragment="how the coalition was formed", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"The coalition is just the most recent name to what once was a political movement in Bar-zhon society.  Once the other political party took firm control of the military and the media, all of our leaders were systematically neutralized through blackmail, lies, and underhanded techniques.  For a while we were a subversive resistance movement but now all we seek is independence." }
	}
	questions[41001] = {
		action="branch",
		choices = {
			{ text="Ancients Themselves",  goto=41000 },
			{ text="Endurium Planets",  goto=42000 },
			{ text="<Back>",  goto=1 }
		}
	}

	questions[41000] = {
		action="jump", goto=41001, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="about the ancients themselves",
		alien={"I don't know or care.  Pester someone else."}
	}
	questions[42000] = {
		action="jump", goto=42001,
		player="[AUTO_REPEAT]",
		playerFragment="where endurium can be found",
		alien={"Try investigating Tafel space around Mag Rein1 - 101,15 or Aoi 4 - 167,16.  Bar-zhon space was strip-mined long ago.  Strangely enough, endurium is never located below ground, but I guess the ancients wanted us to find them easily enough!"}
	}
	questions[42001] = {
		action="jump", goto=41001,
		player="[AUTO_REPEAT]",
		alien={"Not that anyone has ever returned from there recently, but in the past there have been rumors that additional endurium rich planets can be found in the area of space past Thrynn territory."}
	}
end

if (plot_stage == 2) then


	questions[60000] = {
		action="branch",
		choices = {
			{ text="Yes, we surrender.",  goto=60001 },
			{ text="No, we will not surrender!",  goto=60002 }
		}
	}
	questions[60001] = {
		action="jump", goto=999,
		player="[AUTO_REPEAT]",
		alien={"Lower your shields and disarm any weapons and hold perfectly still for a minute, will ya?"}
	}
	questions[60002] = {
		action="jump", goto=999, -- attack
		player="[AUTO_REPEAT]",
		alien={"Have at 'em mate!"}
	}

elseif (plot_stage == 3) then


	questions[50000] = {
		action="branch",
		choices = {
			{ text="Your home base",  goto=51000 },
			{ text="Current news",  goto=52000 },
			{ text="<Back>",  goto=1 }
		}
	}

	questions[51000] = {
		action="jump", goto=50000, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="where your home base is located", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Home base?  You think we are fools?  Our outposts, population, and resources are scattered everywhere, even outside the region of space we patrol."}
	}

	questions[52000] = {
		action="jump", goto=60001,
		player="[AUTO_REPEAT]",
		playerFragment="about current events",
		alien={"You see this shiny new ship?  Minex technology, freshly salvaged.  We'll be taking over this sector if the plague doesn't wipe us off first!"}
	}

	questions[60001] = {
		action="branch",
		choices = {
			{ text="The plague", goto=61000 },
			{ text="Plague problems", goto=62000 },
			{ text="Minex technology?", goto=63000 },
			{ text="Taking over", goto=64000 },
			{ text="<Back>", goto=50000 }
		}
	}
	questions[61000] = {
		action="jump", goto=61001,
		player="[AUTO_REPEAT]",
		playerFragment="about the plague",
		alien={"Insane thing, ghastly Minex technology that acts as a mobile biological warfare laboratory.  New strains of viruses pop up everywhere customized to decimate planetary populations and turn the survivors into zombies.  Of course zombies that recover frequently for some reason mind that." }
	}
	questions[61001] = {
		action="jump", goto=60001, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		alien={"Strangest things just don't add up.  Zombie-controlled ships turn on the Minex and leave other aliens alone.  Considering the technology that causes this plague is so advanced and unstoppable, why is it so ineffective at killing isolated populations and why does it give up control of its victims?" }
	}
	questions[62000] = {
		action="jump", goto=60001,
		player="[AUTO_REPEAT]",
		playerFragment="about the problems that plague is causing",
		alien={"Problems?  How about filling every densely populated city or station with corpses?   Survivors all undergo some cyclical madness and turn on each other at unpredictable times.  Fully outfitted ships just stop communicating and desert never to return." }
	}
	questions[63000] = {
		action="jump", goto=60001,
		player="[AUTO_REPEAT]",
		playerFragment="how you are obtaining Minex technology", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Battlefield salvage is everywhere.  Minex ships do get destroyed occasionally seeing that they up and decided to attack every single other race in the sector simultaneously.  The mad ones are particularly good at leaving unsalvaged hulks everywhere." }
	}
	questions[64000] = {
		action="jump", goto=64001, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		playerFragment="how you are taking over the sector", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Future?  Ehh, we HAD long-term plans.  The simpleminded Tafel were expanding faster than any thought possible.  We have been piecing them out more and more of our technology as they expand their territory, and prepare ourselves to be their allies when they finally clash with the foolish Bar-zhon aristocrats.  Unfortunately just like the Minex, they seemed to have recently decided that all races are the enemy." }
	}
	questions[64001] = {
		action="jump", goto=60001,
		player="[AUTO_REPEAT]",
		alien={"Insane Minex fleets and unstoppable plagues sort of limit our goals to simple survival at the moment.  But the Minex tech we're salvaging is incredible!  Now if we could only survive long enough to refit our ships..." }
	}

elseif (plot_stage == 4) then


	questions[50000] = {
		action="branch",
		choices = {
			{ text="Your home base",  goto=51000 },
			{ text="Current news",  goto=52000 },
			{ text="<Back>",  goto=1 }
		}
	}

	questions[51000] = {
		action="jump", goto=50000, ftest= 3, -- aggravating
		player="[AUTO_REPEAT]",
		playerFragment="where your home base is located", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Home base?   Our system is distributed not centralized.  Our outposts, population, and resources are scattered everywhere, even outside the region of space we patrol."}
	}

	questions[52000] = {
		action="jump", goto=60001,
		player="[AUTO_REPEAT]",
		playerFragment="about current events",
		alien={"Good job with the Minex.  Now all we have to do is figure out how to survive this plague."}
	}

	questions[60001] = {
		action="branch",
		choices = {
			{ text="The plague", goto=61000 },
			{ text="Plague problems", goto=62000 },
			{ text="<Reveal Minex Secret>", goto=63000 },
			{ text="Obtain Ancient technology", goto=64000 },
			{ text="<Back>", goto=50000 }
		}
	}
	questions[61000] = {
		action="jump", goto=61001,
		player="[AUTO_REPEAT]",
		playerFragment="about the plague",
		alien={"Insane thing, ghastly technology that acts as a mobile biological warfare laboratory.  New strains of viruses pop up everywhere customized to decimate planetary populations and turn the survivors into zombies.  Of course zombies that recover, but not as frequently as before." }
	}
	questions[61001] = {
		action="jump", goto=60001, ftest= 2, -- insightful
		player="[AUTO_REPEAT]",
		alien={"Strangest things just don't add up.  Zombie-controlled ships turn on the Minex and leave other aliens alone.  Considering the technology that causes this plague is so advanced and unstoppable, why is it so ineffective at killing isolated populations and why does it give up control of its victims?" }
	}
	questions[62000] = {
		action="jump", goto=60001,
		player="[AUTO_REPEAT]",
		playerFragment="about the problems that plague is causing",
		alien={"Problems?  How about filling every densely populated city or station with corpses?   Survivors all undergo some cyclical madness and turn on each other at unpredictable times.  Fully outfitted ships just stop communicating and they're crews desert never to return." }
	}
	questions[63000] = {
		action="jump", goto=60001, ftest= 3, -- aggravating
		player="The Minex think that the ancients have a cure.",
		alien={"Insanity from the insane.  Feel free to correct us next time you talk to an Ancient one." }
	}
	questions[64000] = {
		action="jump", goto=64001,
		player="[AUTO_REPEAT]",
		playerFragment="where we could obtain ancient technology", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"Well we did acquire some positively strange device that is nearly indestructible.  Acts as a sonic disruptor and shatters endurium.  Can't be used in space and has no penetration.  Appears positively ancient however." }
	}
	questions[64001] = {
		action="jump", goto=60001, ftest=1,
		player="May we have the device?",
		alien={"No, but we will send you a holo-schematic." }
	}

end


end

function QuestDialogueinitial()

--[[
title="Military Mission #30:  We are seeking an afterburner.",
--]]
	questions[74000] = {
		action="jump", goto=74001,
		title="We are seeking an afterburner.",
		player="[AUTO_REPEAT]",
		introFragment= "Coalition vessel.  This is Captain [CAPTAIN] of the starship [SHIPNAME].  We have heard of your superlative propulsion technology.",
		playerFragment= "one of your afterburner devices.", fragmentTable= preQuestion.desire,
		alien={"I can smell an enforcer when I hear one.  Why should we deal with you and why should we give you our technology?" }
	}
	questions[74001] = {
		action="branch",
		choices = {
			{ title= "Moolah!", text="We are prepared to pay well- 20 cubic meters of Endurium.",  goto=74100 },
			{ title= "Diplomacy and Moolah!", text="We have no conflict with you. 12 cubic meters of Endurium, and our gratitude.",  goto=74200 },
			{ title= "Force and Persuasion!", text="If you do not agree to an exchange we will destroy you.",  goto=74300 },
			{ text="Nevermind, let me ask you about something else.", goto=1 }
		}
	}


	questions[74100] = {
		action="jump", goto=74001, ftest= 1,
		player="[AUTO_REPEAT]",
		alien={"We don't actually have any technologies like that.  Don't bother asking again." }
	}
	questions[74200] = {
		action="jump", goto=74001, ftest= 1,
		player="[AUTO_REPEAT]",
		alien={"Your gratitude?  Count it, go broke. Eat it, go hungry. Seek it, go mad!" }
	}
	--[[
				if (player_Endurium < 12) then
					goto_question = 74205
				elseif (ATTITUDE <= 35) then
					goto_question = 74206
				else
					goto_question = 74207
					player_Endurium= player_Endurium -12
					artifact20= artifact20 +1
				end
	]]--

	questions[74205] = {
		action="jump", goto=1,
		player="[AUTO_REPEAT]",
		alien={"Also you might just want to count your promises before you make them, you pauper!" }
	}
	questions[74206] = {
		action="jump", goto=1,
		player="[AUTO_REPEAT]",
		alien={"Besides you guys are not exactly friendly to begin with." }
	}
	questions[74207] = {
		action="jump", goto=74201,
		player="[AUTO_REPEAT]",
		alien={"Okay, well maybe I could give you a little something.  You did not obtain this from me, you ran across this and salvaged it, okay?" }
	}
	questions[74200] = {
		action="jump", goto=74201, ftest= 1,
		player="[AUTO_REPEAT]",
		alien={"Okay, well maybe I could give you a little something.  You did not obtain this from me, you ran across this and salvaged it, okay?" }
	}

	questions[74201] = {
		action="jump", goto=1, ftest= 2, -- insightful
		player="Been a pleasure dealing with you.",
		alien={"I would suggest you treat it well and ensure it doesn't get lost." }
	}


	questions[74300] = {
		action="jump", goto=74301,
		player="[AUTO_REPEAT]",
		alien={"Ha!  Good luck attempting that!  Let me give you a chance to try!" }
	}
	questions[74301] = {
		action="jump", goto=999, -- attack
		player="We do not plan on being defeated.",
		alien={"<Silence>" }
	}

--[[
title="Scientific Mission #35:  locating an exotic small planet with a massive gravity field.",
--]]

	questions[89000] = {
		action="jump", goto=89001,
		player="Help decoding fragmentary data",
		introFragment= "Coalition vessel.  This is Captain [CAPTAIN] of the research vessel [SHIPNAME].  We have need of information you may have.",
		playerFragment="information to help decode this fragmentary data", fragmentVeto= {o= {1,2}, f= {1,2,3,4}, h={1,4}},
		alien={"What's it supposed to be?" }
	}
	questions[89001] = {
		action="jump", goto=89002,
		player="The location of a planet containing exotic particles.",
		alien={"What's in it for me?" }
	}
	questions[89002] = {
		action="jump", goto=1,
		player="Umm, a very unique planet likely to contain treasure.",
		alien={"Transmit the data now.....  Wow this data stream is really trashed.....  The planet itself matches up descriptions of acidic planets but I cannot tell you anything else about its location.  " }
	}

--[[
title="Freelance Mission #29:  Hunt for the Orb - before obtaining it
--]]

		questions[93000] = {
		action="jump", goto=93001,
		player="The Bar-zhon orb",
		introFragment= "Coalition vessel.  This is Captain [CAPTAIN].",
		playerFragment="a Bar-zhon orb", fragmentTable= preQuestion.desire,
		alien={"Now why would someone like me be aware of a fine artifact like that?" }
	}
	questions[93001] = {
		action="branch",
		choices = {
			{ title= "Please?", text="We are friends, aren't we?",  goto=93100 },
			{ title= "5 Endurium", text="Maybe you could use some extra resources (5 endurium)",  goto=93200 },
			{ title= "Demand!", text="You will tell me what you know immediately.",  goto=93300 },
			{ title= "Never mind", text="Forget this.  I'm not going to bother. ", goto=1 }
		}
	}
	questions[93100] = {
		action="jump", goto=1, ftest= 1,
-- if attitude > 60 then 		goto=93105 	else 	goto=93106
		player="[AUTO_REPEAT]",
		alien={"What is this?  Pleading and scraping?" }
	}
	questions[93105] = {
		action="jump", goto=1,
		player="[AUTO_REPEAT]",
		alien={"I wouldn't say as much as good friends, but I'd rather you look into this then certain others, if you know what I mean.  One of our contacts ran across information about an unusual communication artifact that was stashed on a planet known as Lazerarp at the north pole of the planet.  Now if we knew where that planet was, we would obtain the device ourselves.  Unfortunately our sources have turned up nothing." }
	}
	questions[93106] = {
		action="jump", goto=997,
		player="[AUTO_REPEAT]",
		alien={"I can't really say I'd ever be affected by such stupidity.  Get Lost!" }
	}
	questions[93200] = {
		--endurium - 5
		action="jump", goto=1, ftest= 1,
		player="[AUTO_REPEAT]",
			alien={"That is quite generous of you.  One of our contacts overheard information about an unusual communication artifact that was stashed on a planet known as Lazerarp at the north pole of the planet.  Now if we knew where that planet was, we would obtain the device ourselves." }
	}
	questions[93300] = {
		action="jump", goto=999,
		player="[AUTO_REPEAT]",
		alien={"You think that I am worried about some sort of upstart such as yourself and your race?   I have principles and am not worried about your type.  Keep that in mind if you survive." }
	}
--[[
title="Freelance Mission #29:  Hunt for the Orb - after obtaining it
--]]


	questions[93500] = {
		action="jump", goto=93501,
		player="Can you tell us about...",
		alien={"Our scanners indicate that you are carrying the whining orb.  If you are willing to sell this to me I am ready to transport 15 cubic meters of endurium in exchange." }
	}
	questions[93501] = {
		action="branch",
		choices = {
			{ title= "15 endurium", text="Yes, I'll sell it for 15 endurium",  goto=93600 },
			{ text="No.",  goto=1 },
			{ title= "Suggest another offer", text="Are you able to give us anything else for it?",  goto=93700 },
		}
	}
	questions[93600] = {
		--artifact16 = 0,
		--endurium + 15,
		--active_quest = active_quest + 1,
		action="jump", goto=1, ftest= 1,
		player="[AUTO_REPEAT]",
		alien={"Very nice device this is.  Good doing business with you.  (Mission Completed)" }
	}
	questions[93700] = {
		--artifact16 = 0,
		--ship_laser_class = ship_laser_class + 1,
		--active_quest = active_quest + 1,
		action="jump", goto=1, ftest= 1,
		player="[AUTO_REPEAT]",
		alien={"I'm sending over my chief engineer to take a look at your weapon systems.  We might just find a way to upgrade your lasers in exchange for this device.  (Mission Completed)" }
	}

--[[
title="Freelance Mission #30:  the amazing artifact - before obtaining the spiral lens
--]]


	questions[94000] = {
		action="jump", go