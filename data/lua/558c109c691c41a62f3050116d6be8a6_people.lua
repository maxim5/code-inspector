-- KC NOIRE - RACHEL J. MORRIS, 2012 - WWW.MOOSADER.COM - GNU GPL V3

-- People will be a little more complex than Items and the Location;
-- They will have a place in a Location, similar to an Item, as well as
-- Name and Description, however you still need to be able to talk to them!
-- Your dialog options also depend on what you have in your Notebook,
-- which stores a list of items you have examined.

-- PEOPLE --
people = {}

people.bohne = {
    m_name          = "Bohne",
    m_description   = "This Bohne guy is a long haired large fellow in a metal t-shirt.\nUnfortunately, he lacked a fancy hat like I had.",
    m_onMap 		= "kenagypark.grassyField"
}

people.hamilton = {
    m_name = "Hamilton",
    m_description = "She looked quite awake, as if she had been drinkin' coffee all day.\n\t Coffee addiction is a terrible thing...",
    m_onMap = "benettis.backLounge"
}

people.kenworthy = {
    m_name = "Kenworthy",
    m_description = "This guy also lacked a hat.\n  Sittin' there in the coffee shop, but with no coffee. Hm.",
    m_onMap = "benettis.backLounge"
}

people.barista = {
    m_name = "Barista",
    m_description = "The barista was a clean-shaven young man with nice hair,\nbut lacked a fancy hat.",
    m_onMap = "benettis.frontLounge"
}

people.whited = {
    m_name = "Whited",
    m_description = "Guy with curly hair was sittin' at a table.\n\t Fit the description we had gotten of contest participants from HQ.",
    m_onMap = "tacobell.outsideDining"
}

people.employee = {
    m_name = "Employee",
    m_description = "There was a restless-looking cashier girl at the register.",
    m_onMap = "tacobell.orderingQueue"
}

people.stall = {
    m_name = "Stall",
    m_description = "There was a guy sitting in the stall, silently...\nIt was kind of unnerving.",
    m_onMap = "tacobell.bathroom"
}

-- FUNCTIONALITY --

function people_GetPeopleOnMap(mapKey)
    pplList = "|"
	for key, value in pairs(people) do
        if value.m_onMap == mapKey then
            pplList = pplList .. key .. "|"
        end
	end
	return pplList
end

function people_GetName( personKey )
    personKey = string.lower( personKey )
    return people[personKey].m_name
end

function people_GetDescriptionFromName( personName )
    personObj = people_GetPersonTableFromName( personName )
    if personObj ~= nil then
        return personObj.m_description
    end
    return ""
end

function people_GetPersonTableFromName( personName )
    for key, value in pairs( people ) do
        if string.lower( people[key].m_name ) == string.lower( personName ) then
            return people[key]
        end
    end
    return nil
end

function people_PersonExistsAndIsOnMap( personName, mapKey )
    if dialog[personName] == nil then return false end
    if people[personName] == nil then return false end
    if people[personName].m_onMap ~= mapKey then return false end
    return true
end


-- DIALOG --

-- FUNCTIONALITY --

function dialog_GetDialogObject( name, state, message, itemName )
    if state == "talk" then
        if dialog[name][state] == nil then return "" end
        if dialog[name][state]["firstMessage"]["done"] == true then
            diagTable = dialog[name][state]["otherMessage"]
        else
            diagTable = dialog[name][state]["firstMessage"]
            dialog[name][state]["firstMessage"]["done"] = true
        end
    elseif state == "ask" then
        if dialog[name][state][message] == nil then return "" end
        diagTable = dialog[name][state][message]
    elseif state == "interrogation" or state == "evidence" then
        if dialog[name][state] == nil then return "" end
        diagTable = dialog[name][state][itemName][message]
    end

    return diagTable
end

function dialog_CheckOffItem( message, name )
    book_CheckOffDialogItem( message, name )
end

function dialog_BuildDialogString( diagTable )
    retval = "|"
    -- Set next state
    retval = retval .. diagTable["nextState"] .. "|"
    -- Get all da dialawg
    for k, v in pairs( diagTable["response"] ) do
        retval = retval .. v .. "|"
    end
    return retval
end

function dialog_DrawPersonFace( diagTable )
    if diagTable["face"] ~= nil then
        for k, v in pairs( diagTable["face"] ) do
            print ( v )
        end
    end
end

function dialog_AddFlaggedItemsToNotebook( diagTable )
    if diagTable["addToNotebook"] ~= nil then
        for k, v in pairs( diagTable["addToNotebook"] ) do
            book_AddItemToNotebook( v["key"], v["name"] )
        end
    end
end

function dialog_RemoveFlaggedItemsFromNotebook( diagTable )
    if diagTable["removeFromNotebook"] ~= nil then
        for k, v in pairs( diagTable["removeFromNotebook"] ) do
            book_RemoveItemFromNotebook( v["key"] )
        end
    end
end

function dialog_SetMissionSwitch( diagTable )
    if diagTable["missionSwitch"] ~= nil then
        endingFlag = diagTable["missionSwitch"]
    end
end

function dialog_GetDialog( name, state, message, itemName )
    message = string.lower( message )
    name = string.lower( name )
    state = string.lower( state )
    itemName = string.lower( itemName )

    diagTable = dialog_GetDialogObject( name, state, message, itemName )
    if diagTable == nil or diagTable == "" then return "" end

    if state == "ask" then
        dialog_CheckOffItem( message, name )
    end

    retval = dialog_BuildDialogString( diagTable )
    dialog_DrawPersonFace( diagTable )
    dialog_AddFlaggedItemsToNotebook( diagTable )
    dialog_RemoveFlaggedItemsFromNotebook( diagTable )
    dialog_SetMissionSwitch( diagTable )

    return retval
end


-- Testing this out...  Placeholder dialog so I don't give away story :)
dialog = {}

-- BOHNE DIALOG --

dialog.bohne                = {}

dialog["bohne"]["talk"] = {
    ["firstMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Excuse me.  Did you know Mr. Richardson?",
            "Bohne:\t Yeah, he was a friend of mine.\n\t Him, me, and another friend were in the BBQ Contest together.",
            "Frank:\t Who else entered the competition that you know?",
            "Bohne:\t Kenworthy. And Whited, he entered but then withdrew."
        },
        ["addToNotebook"] = {
            ["0"] = { ["key"] = "person.kenworthy", ["name"] = "Kenworthy" },
            ["1"] = { ["key"] = "person.whited", ["name"] = "Whited" }
        },
        ["face"] = {
            "    --------  ",
            "   /___/\\___\\ ",
            "  /| o    o |\\ ",
            " | |    -   | | ",
            "/   \\  --- /   \\ ",
            "|    ------     |",
            "|     |   |     |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Hey, Bon-bon, I have more questions.",
            "Bohne:\t Shoot."
        },
        ["face"] = {
            "    --------  ",
            "   /___/\\___\\ ",
            "  /| o    o |\\ ",
            " | |    -   | | ",
            "/   \\  --- /   \\ ",
            "|    ------     |",
            "|     |   |     |"
        }
    }
}

dialog["bohne"]["ask"] = {
    ["bohne"] =    {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t So what made you decide to get into barbecue?",
            "Bohne:\t I don't usually make barbecue,\n\t I just entered so I'd have a shot at the prize.",
            "Frank:\t And what is the prize?",
            "Bohne:\t It's a cow."
        },
        ["addToNotebook"] = {
            ["0"] = { ["key"] = "item.prize", ["name"] = "Prize Cow" }
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ô    o |\\ ",
        " | |    -   | | ",
        "/   \\   u  /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["where is whited?"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Hey, do you know where Whited might be?",
            "Bohne:\t Yeah, actually. I saw him at Taco Bell before I came over here.",
            "Frank:\t Alright, thanks."
        },
        ["addToNotebook"] = {
            ["0"] = { ["key"] = "location.tacobell", ["name"] = "Taco Bell" }
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ő    ő |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["richardson"] =            {
        ["nextState"] = "interrogation",
        ["response"] = {
            "Frank:\t How well did you know Richardson?",
            "Bohne:\t Pretty well, we'd hang out and stuff.",
            "Frank:\t Any hard feelings towards him?",
            "Bohne:\t He could be an ass sometimes, but not like...\n\t enough to want to murder him."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| o    o |\\ ",
        " | |    -   | | ",
        "/   \\  \\   /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["whited"] =            {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Why did Whited drop out?",
            "Bohne:\t I don't know.  I mean, Kenworthy and I aren't barbecue gurus\n\t but I can't imagine Whited doing any good.\n\t Really, Richardson was the best barbecuer out of the group."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| a    a |\\ ",
        " | |    -   | | ",
        "/   \\    o /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["kenworthy"] =            {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Why did Kenworthy enter?",
            "Bohne:\t Well, him and I aren't that experienced in making barbecue.\n\t Not like Richardson.\n\t But the prize is this awesome cow, and we figured that we'd team up.\n\t If one of us wins the cow, we were gonna start a farm together!",
            "Frank:\t Why a farm?",
            "Bohne:\t I've just always wanted to have a cow and a farm.",
            "Frank:\t Where can I find Kenworthy?",
            "Bohne:\t Last I heard, he was going to hang out with Hamilton,\n\t so they're probably at Benetti's Coffee Experience right now."
        },
        ["addToNotebook"] = {
            ["0"] = { ["key"] = "location.benettis", ["name"] = "Benetti's Coffee Experience" }
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ^    ^ |\\ ",
        " | |    -   | | ",
        "/   \\   u  /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["smoker"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Nice smoker.",
            "Bohne:\t Yeah, I guess.",
            "Frank:\t Any idea how the meat was posioned?",
            "Bohne:\t I don't know.  Poisoned smoke? What's in the recipe?"
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| o    o |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["crumpled paper"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t I found this recipe.",
            "Bohne:\t Yeah, that looks like the recipe Richardson was bragging about.",
            "Frank:\t It's not that complex of a recipe.\n\t He really had to have it written down and with him?",
            "Bohne:\t Maybe he was still altering it.",
            "Frank:\t Altering it... with posion..."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| Ź    Ź |\\ ",
        " | |    -   | | ",
        "/   \\   _  /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["prize cow"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t What's so great about this cow?",
            "Bohne:\t It's a magical cow.",
            "Frank:\t What's magical about it?",
            "Bohne:\t It shits gold.",
            "Frank:\t Ah."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ő    ő |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["kenagy park"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Are you familiar with this park at all?",
            "Bohne:\t Yeah, it's a small park.",
            "Frank:\t Where's the best place to murder somebody, ya think?",
            "Bohne:\t Pff, fuck if I know."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ő    ő |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["benetti's coffee experience"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t So how about that Benetti's...",
            "Bohne:\t It's alright.  I don't love coffee that much though."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ő    ő |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["taco bell"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Taco Bell: Great restauraunt or greatest restauraunt?",
            "Bohne:\t Doritos man.  What's up with that?"
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ő    ő |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    },
    ["suspect recipe"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Richardson's recipe apparently doesn't have his handwriting.",
            "Bohne:\t That's weird, I didn't notice."
        },
        ["face"] = {
        "    --------  ",
        "   /___/\\___\\ ",
        "  /| ő    ő |\\ ",
        " | |    -   | | ",
        "/   \\  --- /   \\ ",
        "|    ------     |",
        "|     |   |     |"
        }
    }
}

dialog["bohne"]["interrogation"] = {
    ["richardson"] = {
        ["truth"] = {
            ["nextState"] = "ask",
            ["response"] = {
                "Frank:\t Everybody can be, sometimes."
            },
            ["face"] = {
            "    --------  ",
            "   /___/\\___\\ ",
            "  /| o    o |\\ ",
            " | |    -   | | ",
            "/   \\  \\   /   \\ ",
            "|    ------     |",
            "|     |   |     |"
            }
        },
        ["doubt"] = {
            ["nextState"] = "ask",
            ["response"] = {
                "Frank:\t I think there's something more to that.\n\t Sure he could be an ass, but he did something, didn't he?",
                "Bohne:\t Besides your normal assy stuff, nothing specific.\n\t Nothing to make ME want to kill him.",
                "Frank:\t Whattabout somebody else?",
                "Bohne:\t Maybe, I don't know what he'd have to do that's extreme\n\t enough to warrant that, though."
            },
            ["face"] = {
            "    --------  ",
            "   /___/\\___\\ ",
            "  /| ő    ő |\\ ",
            " | |    -   | | ",
            "/   \\  --- /   \\ ",
            "|    ------     |",
            "|     |   |     |"
            }

        },
        ["lie"] = {
            ["nextState"] = "ask",
            ["response"] = {
                "Frank:\t FESS UP!\n\t You murdered 'im, didn't ya?!\n\t You wanted that prize so bad,\n\t you'd KILL for it, and then escape the cops!",
                "Bohne:\t Dude, I'm right here.",
                "Frank:\t Your escape plan was foiled!",
                "Bohne:\t By who? I was the one who discovered Richardson\n\t laying there and called the cops.",
                "Frank:\t Right. Sorry, got a bit excited there.\n\t It's my hat, it's just so awesome...\n\t ...Riles me up..."
            },
            ["face"] = {
            "    --------  ",
            "   /___/\\___\\ ",
            "  /| ň    ó |\\ ",
            " | |    -   | | ",
            "/   \\  ___ /   \\ ",
            "|    ------     |",
            "|     |   |     |"
            }
        }
    }
}

dialog["bohne"]["evidence"] = {
    ["crumpled paper"] = {
        ["crumpled paper"] = {
            ["nextState"] = "ask",
            ["response"] = {
                "Frank:\tThis CRUMPLED PAPER I FOUND!",
                "Bohne:\tYou can't prove it with the same item!"
            }
        },
        ["smoker"] = {
            ["nextState"] = "ask",
            ["response"] = {
                "Frank:\tIt's THIS SMOKER!",
                "Bohne:\tYou got me!! D'oh!"
            },
            ["addToNotebook"] = {
                ["0"] = { ["key"] = "item.toilet", ["name"] = "toilet" }
            }
        },
        ["bohne"] = {
            ["nextState"] = "ask",
            ["response"] = {
                "Frank:\tMy proof is that you exist!",
                "Bohne:\tThat's not good enough!"
            }
        }
    }
}

-----------------
--- KENWORTHY ---
-----------------
dialog["kenworthy"] = {}

dialog["kenworthy"]["talk"] = {
    ["firstMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Hey there, contestant Kenworthy.",
            "Kenworthy:\t Hey, you a cop?",
            "Frank:\t\t I'm a detective! I am detective Frank."
        },
        ["face"] = {
    "      -----------",
    "     |-_-_-_-_-_-|",
    "     | -       _ |",
    "     | o       o |",
    "    (|     -     |)",
    "      \\    -    /",
    "        -------",
    "          |  |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Kenworthy:\t\t Hey there Detective F."
        },
        ["face"] = {
    "      -----------",
    "     |-_-_-_-_-_-|",
    "     |           |",
    "     | o       o |",
    "    (|     -     |)",
    "      \\    u    /",
    "        -------",
    "          |  |"
        }

    }
}

dialog["kenworthy"]["ask"] = {
    ["suspect recipe"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Hamilton said this recipe doesn't\n\t\t look like Richardson's handwriting.",
            "Kenworthy:\t Here, let me take a look at that.",
            "Frank:\t\t What's it look like?",
            "Kenworthy:\t Well, it sort of looks like Whited's handwriting, I guess.",
            "Frank:\t\t Who dropped out of the competition?",
            "Kenworthy:\t Yeah, but this was the recipe Richardson was going to use.",
            "Frank:\t\t Are you sure?",
            "Kenworthy:\t Yeah, he was always bragging about it.",
            "Frank:\t\t Any idea where Whited is?",
            "Kenworthy:\t Nuhuh, I haven't seen him for at least a week."
        },
        ["addToNotebook"] = {
            ["0"] = { ["key"] = "item.whereIsWhited", ["name"] = "Where is Whited?" }
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | ô       o |",
            "    (|     -     |)",
            "      \\    _    /",
            "        -------",
            "          |  |"
        }
    },
    ["kenagy park"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Like Kenagy Park?",
            "Kenworthy:\t Yeah, it's fine.",
            "Frank:\t\t If you were gonna murder somebody there,\n\t\t where would you do it?",
            "Kenworthy:\t Do you think I'm the murderer?",
            "Frank:\t\t No.  Not at all..."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | Ź       Ź |",
            "    (|     -     |)",
            "      \\    _    /",
            "        -------",
            "          |  |"
        }
    },
    ["benetti's coffee experience"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Bet'cha love this coffee stuff.",
            "Kenworthy:\t No, not really. I'm just here 'cuz Hamilton wanted to come."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    u    /",
            "        -------",
            "          |  |"
        }
    },
    ["taco bell"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Taco Bell is my favorite restauraunt.",
            "Kenworthy:\t Eheh, I don't think Taco Bell constitutes a restauraunt.",
            "Frank:\t\t Everybody's a critic, man."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | a       a |",
            "    (|     -     |)",
            "      \\    \\    /",
            "        -------",
            "          |  |"
        }
    },
    ["smoker"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t What kinda smoker you usin' for your barbecue?",
            "Kenworthy:\t Um. A good one. Or two."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    _    /",
            "        -------",
            "          |  |"
        }
    },
    ["bohne"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Friends with Bohne?",
            "Kenworthy:\t Yeah, for a while.",
            "Frank:\t\t Feel like murdering 'im?",
            "Kenworthy:\t No, dude. What's with you?"
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    ^    /",
            "        -------",
            "          |  |"
        }
    },
    ["crumpled paper"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t What do you think of Richardson's recipe?",
            "Kenworthy:\t It sounded pretty good, I was looking forward to trying it."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     | /       \\ |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    \\    /",
            "        -------",
            "          |  |"
        }
    },
    ["richardson"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t So it turns out Richardson is really alive.\n\t\t Just taked 'is death.",
            "Kenworthy:\t Really?",
            "Frank:\t\t No, that's a lie. Surprised?\n\t\t Thought you offed 'im good?",
            "Kenworthy:\t Aggh, leave me alone, man."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | O       o |",
            "    (|     -     |)",
            "      \\    o    /",
            "        -------",
            "          |  |"
        }
    },
    ["kenworthy"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t So tell me about yourself.",
            "Kenworthy:\t Well, I like to take long walks on the beach,\n\t\t my favorite color is doggy color...",
            "Frank:\t\t Alright then."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     | ^       _ |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    v    /",
            "        -------",
            "          |  |"
        }
    },
    ["whited"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t What's Whited's hair look like?",
            "Kenworthy:\t It's curly, generally."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    -    /",
            "        -------",
            "          |  |"
        }
    },
    ["hamilton"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t What do you think a' Hamilton?",
            "Kenworthy:\t The cat or the girl?",
            "Frank:\t\t Both?",
            "Kenworthy:\t\t Well, the cat's an asshole.\n\t And the girl is obsessed with Spyro. So yeah."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\    u    /",
            "        -------",
            "          |  |"
        }
    },
    ["prize cow"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t So tell me about this prize cow.",
            "Kenworthy:\t It's only the most amazing cow ever!",
            "Frank:\t\t Why is it so special?",
            "Kenworthy:\t It's made of pure uranium!"
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | O       O |",
            "    (|     -     |)",
            "      \\    u    /",
            "        -------",
            "          |  |"
        }
    },
    ["where is whited?"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Do you know where Whited is?",
            "Kenworthy:\t I don't know, maybe at a WWE match or something.",
            "Frank:\t\t Is there anything like that around here?",
            "Kenworthy:\t No. Kansas City sucks."
        },
        ["face"] = {
            "      -----------",
            "     |-_-_-_-_-_-|",
            "     |           |",
            "     | o       o |",
            "    (|     -     |)",
            "      \\   ___   /",
            "        -------",
            "          |  |"
        }
    }
}

----------------
--- HAMILTON ---
----------------

dialog["hamilton"] = {}
dialog["hamilton"]["talk"] = {
    ["firstMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Excuse me.. Are you a barbecue competition participant?",
            "Hamilton:\t No, I'm just hanging out with my friend Kenworthy.",
            "Frank:\t\t Did you know Richardson?",
            "Hamilton:\t Yeah.",
            "Frank:\t\t Do you mind if I ask you some questions?",
            "Hamilton:\t Go ahead."
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Excuse me, ms. Hamilton.",
            "Hamilton:\t Hi, Frank."
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |   ^  ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\     -    /     |",
            "  |     --------      |",
            " |        |  |         |"
        }
    }
}

dialog["hamilton"]["ask"] = {
    ["crumpled paper"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t I found this recipe in the park.",
            "Hamilton:\t Ahh.  Whose is this?",
            "Frank:\t\t It's Richardson's recipe.",
            "Hamilton:\t This doesn't look like Richardson's handwriting.",
            "Frank:\t\t It doesn't?  Whose does it look like?",
            "Hamilton:\t I don't know, it just doesn't look like Richardson's.\n\t\t We used to do homework together.",
            "Frank:\t\t Alright, thanks."
        },
        ["addToNotebook"] = {
            ["0"] = { ["key"] = "item.suspectrecipe", ["name"] = "Suspect Recipe" }
        },
        ["removeFromNotebook"] = {
            ["0"] = { ["key"] = "item.CrumpledPaper", ["name"] = "Crumpled Paper" }
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |   ^  ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\     .    /     |",
            "  |     --------      |",
            " |        |  |         |"
        }
    },
    ["kenagy park"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Do you like parks?",
            "Hamilton:\t Yeah, they're good for stealing hats."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\     u    /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["benetti's coffee experience"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t So what's your EXPERIENCE with Benetti's?",
            "Hamilton:\t It's a nice little coffee shop.",
            "Frank:\t\t Anyone ever DIE there from BARBECUE POISONING?",
            "Hamilton:\t It's a coffee shop! They serve coffee and pasteries!",
            "Frank:\t\t Well, I think there oughta be a BBQ-and-Coffee-Shop!"
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |   \\  ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\     _    /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["taco bell"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t I love Taco Bell so much",
            "Hamilton:\t ..."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["smoker"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Do you cook barbecue with a smoker?",
            "Hamilton:\t Nuhuh"
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["bohne"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Bohne has such lovely hair.",
            "Hamilton:\t Yes, he's like a fair maiden."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[^]--[^]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["richardson"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t If Richardson weren't dead,\n\t\t what career path would you have chosen for him?",
            "Hamilton:\t Mario.",
            "Frank:\t\t Good choice."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |   _  ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    )o(   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["kenworthy"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Are you a big fan of Kenworthy?",
            "Hamilton:\t Yes, he has all the best songs."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    U   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["whited"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t What was I talking about?",
            "Hamilton:\t Whited",
            "Frank:\t\t How did you know?",
            "Hamilton:\t I become psychic when I'm tired."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["hamilton"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t You think, therefore you are.",
            "Hamilton:\t Cool."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["prize cow"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t What do you think of the prize cow?",
            "Hamilton:\t It's a pretty neat cow.\n\t\t It's like the Swiss Army Knife of cows."
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    O     /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["where is whited?"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t Do you know where Whited is?",
            "Hamilton:\t No, I haven't seen him in a while."
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\    ---   /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    },
    ["suspect recipe"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t\t So definitely not Richardson's handwriting?",
            "Hamilton:\t I don't think it is."
        },
        ["face"] = {
            "      _____  _____",
            "     /     \/     \\",
            "    / -----\\       \\",
            "   / |      ---\\  | \\",
            "  |  |--[o]--[o]-\\|  |",
            "  /  |      -     |   \\",
            " |    \\     n    /     |",
            "  |     --------      |",
            " |        |  |         |"

        }
    }
}


--------------
--- EXTRAS ---
--------------

dialog["barista"] = {}
dialog["barista"]["talk"] = {
    ["firstMessage"] = {
        ["nextState"] = "end",
        ["response"] = {
            "I ordered a coffee from the nice-looking Barista Man."
        },
        ["face"] = {
            "  |\\\\||\\\\//||//",
            "  |           |",
            " (|  o     o  |)",
            "  |     -     |",
            "   \\    u    /",
            "    \\_______/",
            "       | |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "end",
        ["response"] = {
            "I bought a cookie from the Barista."
        },
        ["face"] = {
            "  |\\\\||\\\\//||//",
            "  |           |",
            " (|  o     o  |)",
            "  |     -     |",
            "   \\    u    /",
            "    \\_______/",
            "       | |"
        }
    }
}

dialog["stall"] = {}
dialog["stall"]["talk"] = {
    ["firstMessage"] = {
        ["nextState"] = "end",
        ["response"] = {
            "I tried to talk to the man through the stall door...",
            "Frank:\t Hey man, you doing OK in there?",
            "...",
            "His feet fidgeted some, but he didn't respond to me."
        },
        ["face"] = {
            "    -------------",
            "   /##         ##\\",
            "  |### ?     ? ###|",
            "  |       __      |",
            "  (      ----     )",
            "   \\_____________/",
            "        |    |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "end",
        ["response"] = {
            "Frank:\t Really, dude. You OK?",
            "Man:\t MotherF#*@&, leave me the FUCK alone alright?!\n\t I'm on a toilet in Taco Bell, how do ya think I'm doin'?!",
            "Frank:\t Sorry I just--",
            "Man:\t What kinda nut job talks to people in a stall? Fuck off!"
        },
        ["face"] = {
            "    -------------",
            "   /## \\    /  ##\\",
            "  |### >     < ###|",
            "  |       __      |",
            "  (       __      )",
            "   \\_____________/",
            "        |    |"
        }
    }
}

dialog["employee"] = {}
dialog["employee"]["talk"] = {
    ["firstMessage"] = {
        ["nextState"] = "end",
        ["response"] = {
            "I ordered a bean burrito from the lady."
        },
        ["face"] = {
            "     @@@@@@@",
            "   @@@@@@@@@@@",
            "   |@     @@@|",
            "  (| o     o |)",
            "   |    -    |",
            "    \\   _   /",
            "      -----",
            "       | |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "end",
        ["response"] = {
            "I ordered one of those quaint non-alcoholic margaritas.\nHow novel."
        },
        ["face"] = {
            "     @@@@@@@",
            "   @@@@@@@@@@@",
            "   |@     @@@|",
            "  (| o     o |)",
            "   |    -    |",
            "    \\   _   /",
            "      -----",
            "       | |"
        }
    }
}

--------------
--- WHITED ---
--------------

dialog["whited"] = {}
dialog["whited"]["talk"] = {

    ["firstMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Are you Whited?",
            "Whited:\t Yeah, what's up?",
            "Frank:\t I have some questions, if that's ok.",
            "Whited:\t What about?"
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["otherMessage"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Hey Whit. I'm not done with you yet.",
            "Whited:\t What, man?"
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@_  @_   |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    }
}

dialog["whited"]["ask"] = {
    ["suspect recipe"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Hey, check out this recipe.",
            "Whited:\t Yeah?",
            "Frank:\t Look familiar to you?",
            "Whited:\t Nah..."
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[a]-[a]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["richardson"] = {
        ["nextState"] = "interrogation",
        ["response"] = {
            "Frank:\t Did you know that Richardson was killed?",
            "Whited:\t N-No."
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[.]-[.]--|",
            "   |     _     |",
            "    \\   ___   /",
            "      -------",
            "        | |"
        }
    },
    ["kenagy park"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Go to the park often?",
            "Whited:\t Fr--euh-- no. Not usually.\n\t Only to walk dogs."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |-[ o]-[ o]-|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["benetti's coffee experience"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Like coffee?",
            "Whited:\t Yep."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    o    /",
            "      -------",
            "        | |"
        }
    },
    ["taco bell"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Like Taco Bell?",
            "Whited:\t Sometimes."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["smoker"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t So when you quit the competition,\n\t what did you do with your smoker?",
            "Whited:\t I still have it, it's in my garage."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["bohne"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Bohne said you are an alien from outer space.",
            "Whited:\t Well, shoot. What gave it away?"
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@\\  @/   |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["kenworthy"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Kenworthy says \"Hi\".",
            "Whited:\t Could you tell him that I say hi back?"
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["whited"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t So have a computer?",
            "Whited:\t Yeah, I'm borrowing one.",
            "Frank:\t From who?",
            "Whited:\t This girl Morris.",
            "Frank:\t How long you had it?",
            "Whited:\t Probably like forever.",
            "Frank:\t Ever plan on giving it back?",
            "Whited:\t No idea."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |-[o ]-[o ]-|",
            "   |     _     |",
            "    \\    _    /",
            "      -------",
            "        | |"
        }
    },
    ["hamilton"] = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Hamilton likes coffee.",
            "Whited:\t Yep."
        },
        ["addToNotebook"] = {
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    },
    ["prize cow"]    = {
        ["nextState"] = "ask",
        ["response"] = {
            "Frank:\t Did you really want that prize cow?",
            "Whited:\t A prize isn't that important. It's the journey."
        },
        ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
        }
    }
}

dialog["whited"]["interrogation"] = {
    ["richardson"] = {
        ["truth"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Hey, here's my card. Thanks for talking to me.\n\t If you can think of anything else about Richardson, please give me a call."
            },
            ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[o]-[o]--|",
            "   |     _     |",
            "    \\    u    /",
            "      -------",
            "        | |"
            },
            ["missionSwitch"] = "badEnding" -- Arbitrary name
        },
        ["doubt"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t I'm on to you, guy. I think that you posioned Richardson.\n\t Why would you do somethin' like that? He bully you to withdraw?",
                "Whited:\t NO! What's crazy!",
                "Frank:\t Just admit to it! Ya did it, and you used a chainsaw!",
                "Whited:\t A CHAINSAW to POISON him?!",
                "Frank:\t I've seen it done, kid!",
                "White:\t I didn't do anything, bro!",
                "Frank:\t Aggh, Whit.  Yer makin' this harder than it needs ta' be!"
            },
            ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[ň]-[ó]--|",
            "   |     _     |",
            "    \\    o    /",
            "      -------",
            "        | |"
            },
            ["missionSwitch"] = "badEnding" -- Arbitrary name
        },
        ["lie"] = {
            ["nextState"] = "evidence",
            ["response"] = {
                "Frank:\t How did you not hear that Richardson was killed?\n\t After the murder was reported, our guys contacted each participant\n\t to get alibis.",
                "Whited:\t W- well, I never got a call.",
                "Frank:\t Impossible!  I think you're holdin' somethin' from me, Whit.",
                "Whited:\t Yeah? Like what?"
            },
            ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[ň]-[ó]--|",
            "   |     _     |",
            "    \\    -    /",
            "      -------",
            "        | |"
            },
        }
    }
}

dialog["whited"]["evidence"] = {
    ["richardson"] = {
        ["suspect recipe"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Bohne says this is Richardson's recipe,\n\t but Kenworthy said that it looks like YOUR handwriting.\n\t Is it your handwriting?",
                "Whited:\t N- Yeah. Yes, it's my handwriting.",
                "Frank:\t Why was Richardson cookin' your recipe?\n\t Were you helping him CHEAT?",
                "Whited:\t Fuck no, I wasn't helping him cheat.",
                "Frank:\t But it's YOUR recipe, kid.",
                "Whited:\t I wouldn't help him out if it meant only having to move one inch!",
                "Frank:\t Why the animosity, Whit?  Richardson do somethin' to you?",
                "Whited:\t That asshole.",
                "Frank:\t Yeah?",
                "Whited:\t I worked HARD on that recipe!\n\t On all my AMAZING recipes!\n\t He won his first contest with MY recipe, and then everyone thought\n\t his cooking was AMAZING!",
                "\t But that was MY recipe!\n\t I was going to enter to PROVE that I'm better than him,\n\t with his stolen recipe, but he fuckin' swiped this one, too!",
                "Frank:\t So ya' offed 'im?",
                "Whited:\t Fuckin' ass!  Barbecue is MY LIFE! He took that from me!"
            },
            ["missionSwitch"] = "goodEnding",
            ["face"] = {
            "   @@@@@@@@@@@@@",
            "  @| @@   @    |@",
            "   |--[>]-[<]--|",
            "   |     _     |",
            "    \\    ^    /",
            "      -------",
            "        | |"
            }
        },
        ["richardson"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Richardson told me!",
                "Whited:\t What kind of detective are you?! Dumbass!"
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[.]-[.]--|",
                "   |     _     |",
                "    \\   ___   /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["kenagy park"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Kenagy Park was full of people who WITNESSED you do it!",
                "Whited:\t You're such a liar!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |-[ o]-[ o]-|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["benetti's coffee experience"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Benettis!",
                "Whited:\t What?! You're an idiot!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    o    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["taco bell"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Taco Bell, duh!",
                "Whited:\t What?!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["smoker"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t The Smoker was laced with Whited DNA!",
                "Whited:\t I never touched Richardson's smoker! You lie!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["bohne"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Bohne said you did it!",
                "Whited:\t Nice try, Frank! You're a moron!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@\\  @/   |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["kenworthy"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Kenworthy told me you did it!",
                "Whited:\t Kenworthy doesn't know anything!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["whited"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Your FACE tells me you killed him!",
                "Whited:\t What kind of detective are you?!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |-[o ]-[o ]-|",
                "   |     _     |",
                "    \\    _    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["hamilton"] = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t Hamilton told me you killed Richardson!",
                "Whited:\t Nice try, Frank!"
            },
            ["addToNotebook"] = {
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        },
        ["prize cow"]    = {
            ["nextState"] = "end",
            ["response"] = {
                "Frank:\t The prize cow, man. You were willing to KILL to get it!",
                "Whited:\t YOU are CRAZY!"
            },
            ["face"] = {
                "   @@@@@@@@@@@@@",
                "  @| @@   @    |@",
                "   |--[o]-[o]--|",
                "   |     _     |",
                "    \\    -    /",
                "      -------",
                "        | |"
            },
            ["missionSwitch"] = "badEnding"
        }
    }
}

-- EOF

