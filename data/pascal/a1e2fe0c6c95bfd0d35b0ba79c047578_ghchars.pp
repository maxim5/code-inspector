unit ghchars;
	{This unit handles the constants, functions, and}
	{attributes associated with Character gears.}
{
	GearHead: Arena, a roguelike mecha CRPG
	Copyright (C) 2005 Joseph Hewitt

	This library is free software; you can redistribute it and/or modify it
	under the terms of the GNU Lesser General Public License as published by
	the Free Software Foundation; either version 2.1 of the License, or (at
	your option) any later version.

	The full text of the LGPL can be found in license.txt.

	This library is distributed in the hope that it will be useful, but
	WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
	General Public License for more details.

	You should have received a copy of the GNU Lesser General Public License
	along with this library; if not, write to the Free Software Foundation,
	Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
}

interface
	{ ******************************** }
	{ ***  CHARACTER  DEFINITIONS  *** }
	{ ******************************** }
	{ G = GG_Character }
	{ S = Undefined    }
	{ V = Threat Rating (used for wandering monsters and other encounters) }

uses
	gears;

Type
	SkillDesc = Record
		Name: String;
		DispName: PString;
		Stat: Byte;
	end;

Const
	STAT_Reflexes	= 1;
	STAT_Body	= 2;
	STAT_Speed	= 3;
	STAT_Perception	= 4;
	STAT_Craft	= 5;
	STAT_Ego	= 6;
	STAT_Knowledge	= 7;
	STAT_Charm	= 8;

	NAG_StatImprovementLevel = 11;	{ Counts how many times a stat has been }
					{ improved through experience. }

resourcestring
	STATNAME__BODY = 'Body';
	STATNAME_CHARM = 'Charm';
	STATNAME_CRAFT = 'Craft';
	STATNAME_EGO = 'Ego';
	STATNAME_KNOWLEDGE = 'Knowledge';
	STATNAME_PERCEPTION = 'Perception';
	STATNAME_REFLEXES = 'Reflexes';
	STATNAME_SPEED = 'Speed';

const
	StatName: Array [1..NumGearStats] of String = (
		'Reflexes','Body','Speed','Perception',
		'Craft','Ego','Knowledge','Charm'
	);

	StatDispName: Array [1..NumGearStats] of PString = (
		@STATNAME_Reflexes, @STATNAME__Body, @STATNAME_Speed, @STATNAME_Perception,
		@STATNAME_Craft, @STATNAME_Ego, @STATNAME_Knowledge, @STATNAME_Charm
	);

resourcestring
	STAT_1 = 'Reflexes measures grace and accuracy. It determines most combat rolls.';
	STAT_2 = 'Body measures your character''s size and strength. It determines your health and stamina levels.';
	STAT_3 = 'Speed measures how quickly you can move and react to your environment.';
	STAT_4 = 'Perception measures your general awareness and spatial perception. It is used to spot hidden models.';
	STAT_5 = 'Craft measures your aptitude with tools and practical technology. It is used by most of the repair skills.';
	STAT_6 = 'Ego measures your character''s willpower and self-reliance. It helps determine stamina and mental levels.';
	STAT_7 = 'Knowledge measures your general level of education, and also your aptitude for studying complex matters.';
	STAT_8 = 'Charm measures how charismatic your character is.';

const
	STAT_desc: array[1..NumGearStats] of PString = (
		@STAT_1, @STAT_2, @STAT_3, @STAT_4, @STAT_5,
		@STAT_6, @STAT_7, @STAT_8);

resourcestring
	STATRANK1 = 'Hopeless';
	STATRANK2 = 'Pathetic';
	STATRANK3 = 'Typical';
	STATRANK4 = 'Good';
	STATRANK5 = 'Great';
	STATRANK6 = 'Amazing';
	STATRANK7 = 'Legendary';

const
	STATRANK: array[1..7] of PString = (
		@STATRANK1, @STATRANK2, @STATRANK3, @STATRANK4, @STATRANK5,
		@STATRANK6, @STATRANK7);

	{ This is the normal maximum stat value in character creation. }
	NormalMaxStatValue = 14;

	{ Here are the Mecha System definitions for skills. }
	MS_Maneuver = 1;
	MS_Targeting = 2;
	MS_Sensor = 3;

	NAG_Skill = 1;
	{ S = Skill Name }
	{ V = Skill Rank }

	NAS_FreeSkillPoints = -1;
	NAS_FreeStatPoints = -2;


	NAG_CharDescription = 3;

	NAS_Gender = 0;
	NAV_Male = 0;
	NAV_Female = 1;
    NAV_Nonbinary = 2;
    NAV_Undefined = 3;

resourcestring
	GENDERNAME_FEMALE = 'Female';
	GENDERNAME_MALE = 'Male';
	GENDER_Nonbinary = 'Nonbinary';
	GENDER_Undefined = 'Undefined';

const
	GenderName: Array[0..3] of String = ( 'Male' , 'Female', 'Nonbinary', 'Undefined' );
	GenderDispName: Array[0..3] of PString = ( @GENDERNAME_Male, @GENDERNAME_Female, @GENDER_Nonbinary, @GENDER_Undefined );

	NAS_DAge = 1;	{ CharDescription/Delta age - Offset from 20. }

	NAS_CharType = 2;	{ Character type - PC or NPC. }
	NAV_CTPrimary = 0;	{ default value }
	NAV_CTLancemate = 1;	{ lancemate }

    NAS_RomanceType = 3;
    NAV_RT_NoOne = 0;
    NAV_RT_Male = 1;
    NAV_RT_Female = 2;
    NAV_RT_Anyone = 3;

    NAS_Sentience = 4;
    NAV_IsCharacter = 0;
    NAV_IsMonster = 1;

	{ CharDescription / Personality Traits }
	Num_Personality_Traits = 7;
	NAS_Heroic = -1;	{ CharDescription/ Heroic <-> Villanous }
	NAS_Lawful = -2;	{ CharDescription/ Lawful <-> Chaotic }
	NAS_Sociable = -3;	{ CharDescription/ Sociable (Old-TypeName:Assertive) <-> Shy }
	NAS_Easygoing = -4;	{ CharDescription/ Easygoing <-> Passionate }
	NAS_Cheerful = -5;	{ CharDescription/ Cheerful <-> Melancholy }
	NAS_Renowned = -6;	{ CharDescription/ Renowned <-> Wangtta (Old-TypeName:Hopeless) }
	NAS_Pragmatic = -7;	{ CharDescription/ Pragmatic <-> Spiritual }

resourcestring
	PTRAITNAME_CHAOTIC = 'Chaotic';
	PTRAITNAME_CHEERFUL = 'Cheerful';
	PTRAITNAME_EASYGOING = 'Easygoing';
	PTRAITNAME_HEROIC = 'Heroic';
	PTRAITNAME_LAWFUL = 'Lawful';
	PTRAITNAME_MELANCHOLY = 'Melancholy';
	PTRAITNAME_PASSIONATE = 'Passionate';
	PTRAITNAME_PRAGMATIC = 'Pragmatic';
	PTRAITNAME_RENOWNED = 'Renowned';
	PTRAITNAME_SHY = 'Shy';
	PTRAITNAME_SOCIABLE = 'Sociable';
	PTRAITNAME_SPIRITUAL = 'Spiritual';
	PTRAITNAME_VILLAINOUS = 'Villainous';
	PTRAITNAME_WANGTTA = 'Wangtta';

const
	PTraitName: Array [1..Num_Personality_Traits,1..2] of String = (
	(	'Heroic','Villainous'		),
	(	'Lawful','Chaotic'		),
	(	'Sociable','Shy'		),
	(	'Easygoing','Passionate'	),
	(	'Cheerful','Melancholy'		),
	(	'Renowned','Wangtta'		),
	(	'Pragmatic','Spiritual'		)
	);

	PTraitDispName: Array [1..Num_Personality_Traits,1..2] of PString = (
	(	@PTRAITNAME_HEROIC, @PTRAITNAME_VILLAINOUS	),
	(	@PTRAITNAME_LAWFUL, @PTRAITNAME_CHAOTIC		),
	(	@PTRAITNAME_SOCIABLE, @PTRAITNAME_SHY		),
	(	@PTRAITNAME_EASYGOING, @PTRAITNAME_PASSIONATE	),
	(	@PTRAITNAME_CHEERFUL, @PTRAITNAME_MELANCHOLY	),
	(	@PTRAITNAME_RENOWNED, @PTRAITNAME_WANGTTA	),
	(	@PTRAITNAME_PRAGMATIC, @PTRAITNAME_SPIRITUAL	)
	);

	NAG_Experience = 4;

	NAS_TotalXP = 0;
	NAS_SpentXP = 1;
	NAS_Credits = 2;
	NAS_FacXP = 3;
	NAS_FacLevel = 4;
	NAS_Skill_XP_Base = 100;	{ For skill-specific XP awards. }
					{ S = 100 + Skill Index }

	NAG_Condition = 9;
	NAS_StaminaDown = 0;
	NAS_MentalDown = 1;
	NAS_Hunger = 2;
	NAS_MoraleDamage = 3;
	{ Overload affects mecha rather than characters, but it's }
	{ a condition so it's grouped in with these. }
	NAS_Overload = 4;
	NAS_CyberTrauma = 5;


	MORALE_HPRegen = 5;
	MORALE_RepSmall = 10;
	MORALE_RepBig = 30;

	FOOD_MORALE_FACTOR = 15;

	{ Hunger penalty is calibrated to start roughly twelve hours }
	{ after your last meal. }
	Hunger_Penalty_Starts = 70;

	{For now, all skills will be hardcoded into the game binary.}
	{At some point in time I may have them defined in an}
	{external text file, but since the application of these}
	{skills have to be hard coded I don't see why the data}
	{for them shouldn't be as well.}
	NumSkill = 41;

	USAGE_Repair = [15, 16, 20, 22, 23, 24];
	{ player can't use cybertech }
	USAGE_Repair_p = USAGE_Repair - [24];
	USAGE_Clue = [14, 29, 32, 34, 37];
	Usable_Skills = USAGE_Repair_p + USAGE_Clue + [35, 38, 40, 41];

	Mek_Maneuver_Skills = [3, 4, 5, 25];
	Mek_Targeting_Skills = [1, 2];
	Mek_Sensor_Skills = [11, 18];

resourcestring
	SKILLMAN_ARMED_COMBAT = 'Armed Combat';
	SKILLMAN_ATHLETICS = 'Athletics';
	SKILLMAN_AWARENESS = 'Awareness';
	SKILLMAN_BIO_TECHNOLOGY = 'Bio Technology';
	SKILLMAN_CODE_BREAKING = 'Code Breaking';
	SKILLMAN_CONCENTRATION = 'Concentration';
	SKILLMAN_CONVERSATION = 'Conversation';
	SKILLMAN_CYBERTECH = 'Cybertech';
	SKILLMAN_DODGE = 'Dodge';
	SKILLMAN_DOMINATE_ANIMAL = 'Dominate Animal';
	SKILLMAN_ELECTRONIC_WARFARE = 'Electronic Warfare';
	SKILLMAN_FIRST_AID = 'First Aid';
	SKILLMAN_FLIRTATION = 'Flirtation';
	SKILLMAN_GENERAL_REPAIR = 'General Repair';
	SKILLMAN_HEAVY_WEAPONS = 'Heavy Weapons';
	SKILLMAN_INITIATIVE = 'Initiative';
	SKILLMAN_INTIMIDATION = 'Intimidation';
	SKILLMAN_INVESTIGATION = 'Investigation';
	SKILLMAN_LEADERSHIP = 'Leadership';
	SKILLMAN_MARTIAL_ARTS = 'Martial Arts';
	SKILLMAN_MECHA_ARTILLERY = 'Mecha Artillery';
	SKILLMAN_MECHA_ENGINEERING = 'Mecha Engineering';
	SKILLMAN_MECHA_FIGHTING = 'Mecha Fighting';
	SKILLMAN_MECHA_GUNNERY = 'Mecha Gunnery';
	SKILLMAN_MECHA_PILOTING = 'Mecha Piloting';
	SKILLMAN_MECHA_REPAIR = 'Mecha Repair';
	SKILLMAN_MECHA_WEAPONS = 'Mecha Weapons';
	SKILLMAN_MEDICINE = 'Medicine';
	SKILLMAN_MYSTICISM = 'Mysticism';
	SKILLMAN_PERFORMANCE = 'Performance';
	SKILLMAN_PICK_POCKETS = 'Pick Pockets';
	SKILLMAN_RESISTANCE = 'Resistance';
	SKILLMAN_ROBOTICS = 'Robotics';
	SKILLMAN_SCIENCE = 'Science';
	SKILLMAN_SHOPPING = 'Shopping';
	SKILLMAN_SMALL_ARMS = 'Small Arms';
	SKILLMAN_SPOT_WEAKNESS = 'Spot Weakness';
	SKILLMAN_STEALTH = 'Stealth';
	SKILLMAN_SURVIVAL = 'Survival';
	SKILLMAN_UNKNOWN_SKILL = 'Unknown Skill';
	SKILLMAN_VITALITY = 'Vitality';
	SKILLMAN_WEIGHT_LIFTING = 'Weight Lifting';

const
	Skillname: array[1..Numskill] of String = (
		'Mecha Gunnery', 'Mecha Artillery', 'Mecha Weapons', 'Mecha Fighting', 'Mecha Piloting',
		'Small Arms', 'Heavy Weapons', 'Armed Combat', 'Martial Arts', 'Dodge',
		'Awareness', 'Initiative', 'Vitality', 'Survival', 'Mecha Repair',
		'Medicine', 'Electronic Warfare', 'Spot Weakness', 'Conversation', 'First Aid',
		'Shopping', 'Bio Technology', 'General Repair', 'Cybertech', 'Stealth',
		'Athletics', 'Flirtation', 'Intimidation', 'Science', 'Concentration',
		'Mecha Engineering', 'Code Breaking', 'Weight Lifting', 'Mysticism', 'Performance',
		'Resistance', 'Investigation', 'Robotics', 'Leadership', 'Dominate Animal',
		'Pick Pockets'
	);
	SkillDispName: Array [1..NumSkill] of PString = (
		@SKILLMAN_MECHA_GUNNERY,
		@SKILLMAN_MECHA_ARTILLERY,
		@SKILLMAN_MECHA_WEAPONS,
		@SKILLMAN_MECHA_FIGHTING,
		@SKILLMAN_MECHA_PILOTING,

		{ Skills 6 - 10 }
		@SKILLMAN_SMALL_ARMS,
		@SKILLMAN_HEAVY_WEAPONS,
		@SKILLMAN_ARMED_COMBAT,
		@SKILLMAN_MARTIAL_ARTS,
		@SKILLMAN_DODGE,

		{ Skills 11 - 15 }
		@SKILLMAN_AWARENESS,
		@SKILLMAN_INITIATIVE,
		@SKILLMAN_VITALITY,
		@SKILLMAN_SURVIVAL,
		@SKILLMAN_MECHA_REPAIR,

		{ Skills 16 - 20 }
		@SKILLMAN_MEDICINE,
		@SKILLMAN_ELECTRONIC_WARFARE,
		@SKILLMAN_SPOT_WEAKNESS,
		@SKILLMAN_CONVERSATION,
		@SKILLMAN_FIRST_AID,

		{ Skills 21 - 25 }
		@SKILLMAN_SHOPPING,
		@SKILLMAN_BIO_TECHNOLOGY,
		@SKILLMAN_GENERAL_REPAIR,
		@SKILLMAN_CYBERTECH,
		@SKILLMAN_STEALTH,

		{ Skills 26 - 30 }
		@SKILLMAN_ATHLETICS,
		@SKILLMAN_FLIRTATION,
		@SKILLMAN_INTIMIDATION,
		@SKILLMAN_SCIENCE,
		@SKILLMAN_CONCENTRATION,

		{ Skills 31 - 35 }
		@SKILLMAN_MECHA_ENGINEERING,
		@SKILLMAN_CODE_BREAKING,
		@SKILLMAN_WEIGHT_LIFTING,
		@SKILLMAN_MYSTICISM,
		@SKILLMAN_PERFORMANCE,

		{ Skills 36 - 40 }
		@SKILLMAN_RESISTANCE,
		@SKILLMAN_INVESTIGATION,
		@SKILLMAN_ROBOTICS,
		@SKILLMAN_LEADERSHIP,
		@SKILLMAN_DOMINATE_ANIMAL,

		{ Skills 41 - 45 }
		@SKILLMAN_PICK_POCKETS
	);

	Skill_stat: Array[1..NumSkill] of Byte = (
		STAT_Reflexes, STAT_Perception, STAT_Reflexes, STAT_Speed, STAT_Reflexes,
		STAT_Reflexes, STAT_Perception, STAT_Reflexes, STAT_Body, STAT_Speed,
		{11-}
		STAT_Perception, STAT_Speed, STAT_Body, STAT_Craft, STAT_Craft,
		STAT_Knowledge, STAT_Craft, STAT_Craft, STAT_Charm, STAT_Craft,
		{21-}
		STAT_Charm, STAT_Knowledge, STAT_Craft, STAT_Ego, STAT_Speed,
		STAT_Body, STAT_Charm, STAT_Ego, STAT_Knowledge, STAT_Ego,
		{31-}
		STAT_Knowledge, STAT_Craft, STAT_Body, STAT_Ego, STAT_Charm,
		STAT_Ego, STAT_Perception, STAT_Knowledge, STAT_Charm, STAT_Ego,
		{41-}
		STAT_Craft
	);

	stat_skill: Array[1..NumGearStats] of set of Byte = (
{Reflexes}	[1, 3, 5, 6, 8],
{Body}		[9, 13, 26, 33],
{Speed}		[4, 10, 12, 25],
{Perception}	[2, 7, 11, 37],
{Craft}		[14, 15, 17, 18, 20, 23, 32, 41],
{Ego}		[24, 28, 30, 34, 36, 40],
{Knownledge}	[16, 22, 29, 31, 38],
{Charm}		[19, 21, 27, 35, 39]
	);

    NAS_Awareness = 11;
    NAS_Initiative = 12;
	NAS_Vitality = 13;
	NAS_MechaRepair = 15;
    NAS_ElectronicWarfare = 17;
    NAS_SpotWeakness = 18;
    NAS_Stealth = 25;
	NAS_WeightLifting = 33;
	NAS_Performance = 35;
	NAS_Resistance = 36;
	NAS_Investigation = 37;
	NAS_Robotics = 38;
	NAS_Leadership = 39;
	NAS_DominateAnimal = 40;
	NAS_PickPockets = 41;

	NAG_Talent = 16;
	NumTalent = 28;

	NAS_StrengthOfFaith = 1;
	NAS_BodyBuilder = 2;
	NAS_ScientificMethod = 3;
	NAS_Presence = 4;
	NAS_KungFu = 5;
	NAS_HapKiDo = 6;
	NAS_Anatomist = 7;
	NAS_HardAsNails = 8;
	NAS_StuntDriving = 9;
	NAS_Savant = 10;
	NAS_Bishounen = 11;
	NAS_Diplomatic = 12;
	NAS_BornToFly = 13;
	NAS_SureFooted = 14;
	NAS_RoadHog = 15;
	NAS_Scavenger = 16;
	NAS_Idealist = 17;
	NAS_BusinessSense = 18;
	NAS_AnimalTrainer = 19;
	NAS_JackOfAll = 20;
	NAS_CombatMedic = 21;
	NAS_Rage = 22;
	NAS_Ninjitsu = 23;
	NAS_HullDown = 24;
	NAS_GateCrasher = 25;
	NAS_Extropian = 26;
	NAS_CyberPsycho = 27;
	NAS_Sniper = 28;

	{ Talent pre-requisites are described as follows: The first }
	{ coordinate lists the skill (if positive) or stat (if negative) }
	{ needed to gain the talent. The second coordinate lists the }
	{ minimum value required. If either coordinate is zero, this }
	{ talent has no pre-requisites. }
	{ NEW: Personality traits may be specified as -8 + Trait Number }
	TALENT_PreReq: Array [1..NumTalent,1..2] of Integer = (
	( 34 , 5 ) , ( 33 , 5 ) , ( 29 , 5 ) , ( 35 , 5 ), ( 9 , 5 ),
	( 9 , 5 ) , ( 16 , 5 ) , ( 36 , 5 ) , ( -3 , 15 ) , ( 0 , 0 ),
	( -8 , 15 ) , ( -6 , 15 ) , ( 5 , 5 ) , ( 5 , 5 ) , ( 5 , 5 ),
	( 15 , 5 ), ( 0 , 0 ), ( 21 , 5 ), ( 40 , 5 ), (-STAT_Craft,15),
	( 20 , 5 ), ( -13 , -25 ), ( 25 , 5 ), ( 17 , 5 ), ( -12 , -25 ),
	( 24 , 5 ), ( -2 , 15 ), ( 18 , 5 )
	);

resourcestring
	TALENT1 = 'Strength of Faith';
	TALENT2 = 'Body Builder';
	TALENT3 = 'Scientific Method';
	TALENT4 = 'Presence';
	TALENT5 = 'Kung Fu';
	TALENT6 = 'Hap Ki Do';
	TALENT7 = 'Anatomist';
	TALENT8 = 'Hard As Nails';
	TALENT9 = 'Stunt Driving';
	TALENT10 = 'Savant';
	TALENT11 = 'Beautiful One';
	TALENT12 = 'Diplomatic';
	TALENT13 = 'Born To Fly';
	TALENT14 = 'Sure Footed';
	TALENT15 = 'Road Hog';
	TALENT16 = 'Tech Vulture';
	TALENT17 = 'Idealist Blood';
	TALENT18 = 'Business Sense';
	TALENT19 = 'Animal Trainer';
	TALENT20 = 'Jack of all Trades';
	TALENT21 = 'Combat Medic';
	TALENT22 = 'Badass';
	TALENT23 = 'Ninjitsu';
	TALENT24 = 'Hull Down';
	TALENT25 = 'Gate Crasher';
	TALENT26 = 'Extropian';
	TALENT27 = 'Cyber-Psycho';
	TALENT28 = 'Sniper';
	TALENTDESC1 = 'You have the power of faith on your side. (+2 Ego)';
	TALENTDESC2 = 'You are totally pumped up. (+2 Body)';
	TALENTDESC3 = 'Your scientific training has made you very good at discovering new things. (+2 Knowledge)';
	TALENTDESC4 = 'You have mastered a commanding stage presence. (+2 Charm)';
	TALENTDESC5 = 'Your hands are lethal weapons. (+3 Penetration for Martial Arts attacks)';
	TALENTDESC6 = 'You are an expert at self-defense. (May block attacks using Martial Arts)';
	TALENTDESC7 = 'Because of your anatomical knowledge, you are able to target an opponent''s vital points. (+1 Penetration versus living targets)';
	TALENTDESC8 = 'You don''t feel pain like other folks. (All attacks against PC are at -2 Penetration)';
	TALENTDESC9 = 'As long as you''re going fast, you can find a way to avoid most attacks. (may reroll dodge if traveling at full speed)';
	TALENTDESC10 = 'You have more skills than is normal for a person with your intelligence. (can learn 5 more skills without penalty)';
	TALENTDESC11 = 'You are so good looking that it''s literally impossible to hate you. (reaction score never drops below 0, may use Flirtation skill with all NPCs)';
	TALENTDESC12 = 'In conversation, you can avoid controversial topics. (may avoid reaction loss due to personality clash)';
	TALENTDESC13 = 'You are an expert with flying mecha. (+3 Mecha Piloting while flying or jumping)';
	TALENTDESC14 = 'You are an expert with walkers and zoanoids. (+2 Mecha Piloting while walking)';
	TALENTDESC15 = 'You are an expert with cars and other wheeled vehicles. (+2 Mecha Piloting while rolling)';
	TALENTDESC16 = 'When awarded salvage, you''ll grab everything worth taking. (can salvage individual modules from defeated mecha)';
	TALENTDESC17 = 'Your family heritage includes some genetic engineering. (+1 to three random stats)';
	TALENTDESC18 = 'You are very good at negotiating favorable deals. (+25% to most mission cash rewards)';
	TALENTDESC19 = 'You are good at teaching your pets how to do tricks. (new pets gain XP bonus)';
	TALENTDESC20 = 'You can do a little bit of everything. (may use unknown skills without -2 penalty)';
	TALENTDESC21 = 'You are capable of performing emergency medicine in dangerous places. (First Aid and Medicine attempts take 1/3 normal time)';
	TALENTDESC22 = 'The angrier you get, the harder you fight. (Bonus to combat skills while bad morale)';
	TALENTDESC23 = 'Your surprise attacks are deadly. (damage bonus when attacking unseen)';
	TALENTDESC24 = 'As long as you are walking or rolling, you can position your mecha so as to prevent critical hits. (All attacks against walking or rolling mecha are at -3 Penetration)';
	TALENTDESC25 = 'You''re particularly good at smashing things. (+2 penetration against inanimate objects)';
	TALENTDESC26 = 'You are philosophically prepared to deal with the loss of your humanity. (may have one implant per 3 points Cybertech with no chance of disfunction)';
	TALENTDESC27 = 'Your body is adjusted to cyberware, it''s just your mind that suffers. (May avoid trauma by spending MP)';
	TALENTDESC28 = 'One shot is all you ever need. (bonus to single fire damage rolls)';

const
	TALENTNAME: array[1..NumTalent] of PString = (
		@TALENT1, @TALENT2, @TALENT3, @TALENT4, @TALENT5,
		@TALENT6, @TALENT7, @TALENT8, @TALENT9, @TALENT10,
		@TALENT11, @TALENT12, @TALENT13, @TALENT14, @TALENT15,
		@TALENT16, @TALENT17, @TALENT18, @TALENT19, @TALENT20,
		@TALENT21, @TALENT22, @TALENT23, @TALENT24, @TALENT25,
		@TALENT26, @TALENT27, @TALENT28);
	TALENTDESC: array[1..NumTalent] of PString = (
		@TALENTDESC1, @TALENTDESC2, @TALENTDESC3, @TALENTDESC4, @TALENTDESC5,
		@TALENTDESC6, @TALENTDESC7, @TALENTDESC8, @TALENTDESC9, @TALENTDESC10,
		@TALENTDESC11, @TALENTDESC12, @TALENTDESC13, @TALENTDESC14, @TALENTDESC15,
		@TALENTDESC16, @TALENTDESC17, @TALENTDESC18, @TALENTDESC19, @TALENTDESC20,
		@TALENTDESC21, @TALENTDESC22, @TALENTDESC23, @TALENTDESC24, @TALENTDESC25,
		@TALENTDESC26, @TALENTDESC27, @TALENTDESC28);

Procedure InitChar(Part: GearPtr);
Function CharBaseDamage( PC: GearPtr; CBod: Integer ): Integer;
Function CharStamina( PC: GearPtr ): Integer;
Function CharMental( PC: GearPtr ): Integer;
Function CharCurrentStamina( PC: GearPtr ): Integer;
Function CharCurrentMental( PC: GearPtr ): Integer;

Function RandomName: String;
procedure RollStats( PC: GearPtr; Pts: Integer);
function RandomPilot( StatPoints , SkillRank: Integer ): GearPtr;
function RandomSoldier( StatPoints , SkillRank: Integer ): GearPtr;

Function NumberOfSkills( PC: GearPtr ): Integer;
Function NumberOfSkillSlots( PC: GearPtr ): Integer;
Function TooManySkillsPenalty( PC: GearPtr; N: Integer ): Integer;
Function SkillAdvCost( PC: GearPtr; CurrentLevel: Integer ): LongInt;

function IsLegalCharSub( SPC, Part: GearPtr ): Boolean;

Function PersonalityTraitDesc( Trait,Level: Integer ): String;
Function NPCTraitDesc( NPC: GearPtr ): String;
Function JobAgeGenderDesc( NPC: GearPtr ): String;

Function CanLearnTalent( PC: GearPtr; T: Integer ): Boolean;
Function NumFreeTalents( PC: GearPtr ): Integer;
Procedure ApplyTalent( PC: GearPtr; T: Integer );

Function RepairSkillIndex(Skill:Integer):Integer;
Function ClueSkillIndex(Skill:Integer): Integer;

implementation

uses
	sysutils, math, strutils,
{$IFDEF DEBUG}
	errmsg,
{$ENDIF DEBUG}
	i18nmsg,texutil;

Procedure InitChar(Part: GearPtr);
	{PART is a newly created Character record.}
	{Initialize its stuff.}
begin
	if IsInvalidGear(Part) then Exit;

	{Default scale for a PC is 0.}
	Part^.Scale := 0;

	{ Default material for a PC is "meat". }
	GSetNAtt( Part, NAG_GearOps , NAS_Material , NAV_Meat );
end;

Function CharBaseDamage( PC: GearPtr; CBod: Integer ): Integer;
	{Calculate the number of general HPs that a character}
	{can take.}
var
	HP: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);

	{Error check- make sure we have a character here.}
	if PC^.G <> GG_Character then Exit(0);
	HP := ( CBod + 5 ) div 2;

	{ Add the Vitality skill. }
	HP := HP + GNAttValue( PC, NAG_Skill , 13 );

	CharBaseDamage := HP;
end;

Function CharStamina( PC: GearPtr ): Integer;
	{Calculate the number of stamina points that a character has.}
var
	SP: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);

	{Error check- make sure we have a character here.}
	if PC^.G <> GG_Character then Exit(0);

	{ basic stamina rating is equal to the average between BODY and EGO. }
	SP := ( PC^.Stat[ STAT_Body ] + PC^.Stat[ STAT_Ego ] + 5 ) div 2;

	{ Add the Athletics skill. }
	SP := SP + GNAttValue( PC, NAG_Skill , 26 ) * 3;

	CharStamina := SP;
end;

Function CharMental( PC: GearPtr ): Integer;
	{Calculate the number of mental points that a character has.}
var
	MP: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);

	{Error check- make sure we have a character here.}
	if PC^.G <> GG_Character then Exit(0);

	{ basic mental rating is equal to the average between }
	{ KNOWLEDGE and EGO. }
	MP := ( PC^.Stat[ STAT_Knowledge ] + PC^.Stat[ STAT_Ego ] + 5 ) div 2;

	{ Add the Concentration skill. }
	MP := MP + GNAttValue( PC, NAG_Skill , 30 ) * 3;

	CharMental := MP;
end;

Function CharCurrentStamina( PC: GearPtr ): Integer;
	{ Return the current stamina value for this character. }
var
	it: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);

	{Error check- make sure we have a character here.}
	if PC^.G <> GG_Character then Exit(0);

	it := CharStamina( PC ) - GNAttValue( PC, NAG_Condition , NAS_StaminaDown );
	if it < 0 then it := 0;
	CharCurrentStamina := it;
end;

Function CharCurrentMental( PC: GearPtr ): Integer;
	{ Return the current mental value for this character. }
var
	it: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);

	{Error check- make sure we have a character here.}
	if PC^.G <> GG_Character then Exit(0);

	it := CharMental( PC ) - GNAttValue( PC, NAG_Condition , NAS_MentalDown );
	if it < 0 then it := 0;
	CharCurrentMental := it;
end;


Function RandomName: String;
	{Generate a random name for a character.}
Const
	SyllableList: Array [1..120] of String = (
		'Jo','Sep','Hew','It','Seo','Eun','Suk','Ki','Kang','Cho',
		'Ai','Bo','Ca','Des','El','Fas','Gun','Ho','Ia','Jes',
		'Kep','Lor','Mo','Nor','Ox','Pir','Qu','Ra','Sun','Ter',
		'Ub','Ba','Tyb','War','Bac','Yan','Zee','Es','Vis','Jang',
		'Vic','Tor','Et','Te','Ni','Mo','Bil','Con','Ly','Dam',
		'Cha','Ro','The','Bes','Ne','Ko','Kun','Ran','Ma','No',
		'Ten','Do','To','Me','Ja','Son','Love','Joy','Ken','Iki',
		'Han','Lu','Ke','Sky','Wal','Jen','Fer','Le','Ia','Chu',
		'Tek','Ubu','Roi','Har','Old','Pin','Ter','Red','Ex','Al',
		'Alt','Rod','Mia','How','Phi','Aft','Aus','Tin','Her','Ge',
		'Hawk','Eye','Ger','Ru','Od','Jin','Un','Hyo','Leo','Star',
		'Buck','Ers','Rog','Eva','Ova','Oni','Ami','Ga','Cyn','Mai'

	);

	VowelList: Array [1..6] of String = (
		'A','E','I','O','U','Y'
	);

	ConsonantList: Array [1..21] of String = (
		'B','C','D','F','G','H','J','K','L','M','N',
		'P','Q','R','S','T','V','W','X','Y','Z'
	);
var
	it: String;

	Procedure Syl(lc:boolean = False);
	var
		S: String;
	begin
		if Random(16) = 0 then
			S := RandomFrom(VowelList)
		else if Random(20) = 0 then begin
			if Random(3) = 0 then
				S := RandomFrom(ConsonantList) + LowerCase( RandomFrom(VowelList) )
			else if Random(2) = 0 then
				S := RandomFrom(VowelList) + LowerCase( RandomFrom(ConsonantList) )
			else if Random(2) = 0 then
				S := RandomFrom(ConsonantList) + LowerCase( RandomFrom(VowelList) + RandomFrom(ConsonantList) )
			else
				S := RandomFrom(VowelList) + LowerCase( RandomFrom(ConsonantList) + RandomFrom(VowelList) );
		end else
			S := RandomFrom(SyllableList);
		if lc then S := LowerCase(S);
		it := it + S;
	end;
begin
	Syl;
	{A basic name is two syllables stuck together.}
	if Random(100) <> 0 then
		Syl(True);

	{Uncommon names may have 3 syllables.}
	if ( Random(8) > Length(it) ) then
		Syl(True)
	else if Random(30) = 0 then
		Syl(True);

	{Short names may have a second part. This isn't common.}
	if (Length(it) < 9) and (Random(16) = 0) then begin
		it := it + ' ';
		Syl;
		if Random(3) <> 0 then Syl(True);
	end;

	{ Random chance of random anime designation. }
	if Random(1000) = 0 then it := it + ' - ' + RandomFrom(ConsonantList);

	RandomName := it;
end;

procedure RollStats( PC: GearPtr; Pts: Integer);
	{ Randomly allocate PTS points to all of the character's }
	{ stats.  Advancing stats past maximum }
	{ rank takes two stat points per rank instead of one. }
	{ Hopefully, this will be clear once you read the implementation... }
var
	T: Integer;	{ A loop counter. }
	STemp: Array [1..NumGearStats] of Integer;
	{ I always name my loop counters T, in honor of the C64. }
begin
	{ Error Check - Is this a character!? }
	if ( PC = Nil ) or ( PC^.G <> GG_Character ) then Exit;

	{ Set all stat values to minimum. }
	if Pts >= NumGearStats then begin
{$IF SIZEOF(Integer) = 4}
		FillDWord(STemp, Length(STemp), 1);
{$ELSE}
		FillWord(STemp, Length(STemp), 1);
{$ENDIF}
		Pts := Pts - NumGearStats;
	end else
		FillChar(STemp, sizeof(STemp), 0);

	{ Keep processing until we run out of stat points to allocate. }
	while Pts > 0 do begin
		{ T will now point to the stat slot to improve. }
		T := Random( NumGearStats ) + 1;

		{ If the stat selected is under the max value, }
		{ improve it. If it is at or above the max value, }
		{ there's a one in three chance of improving it. }
		if ( STemp[T] + PC^.Stat[ T ] ) < NormalMaxStatValue then begin
			Inc( STemp[T] );
			Dec( Pts );

		end else if Random(2) = 0 then begin
			Inc( STemp[T] );
			Pts := Pts - 2;

		end;
	end;

	{ Add the STemp values to the stat baseline. }
	for t := 1 to NumGearStats do PC^.Stat[t] := PC^.Stat[t] + STemp[t];
end;

function RandomPilot( StatPoints , SkillRank: Integer ): GearPtr;
	{ Create a totally random mecha pilot, presumably so that }
	{ the PC pilots will have someone to thwack. }
const
	PS: Array [1..5] of Integer = (
		10,12,17,18,25
	);
var
	T: Integer;
begin
	{ Generate record. }
	result := NewGear( Nil );
	if not Assigned(result) then Exit;
	InitChar( result );
	result^.G := GG_Character;

	{ Roll some stats for this character. }
	RollStats( result , StatPoints );

	{ Set all mecha skills to equal SkillRank. }
	for T := 1 to 5 do
		GSetNAtt( result, NAG_Skill , T , SkillRank );

	{ Add a specialty. }
	GSetNAtt( result, NAG_Skill , RandomFrom(PS) , SkillRank );

	{ Generate a random name for the character. }
	GSetSAtt( result, 'Name', RandomName );
end;

function RandomSoldier( StatPoints , SkillRank: Integer ): GearPtr;
	{ Create a totally random fighter, presumably so that }
	{ the PC fighters will have someone to thwack. }
var
	NPC: GearPtr;
	T: Integer;
begin
	{ Generate record. }
	NPC := NewGear( Nil );
	if NPC = Nil then Exit( Nil );
	InitChar( NPC );
	NPC^.G := GG_Character;

	{ Roll some stats for this character. }
	RollStats( NPC , StatPoints );

	{ Set all mecha skills to equal SkillRank. }
	for t := 6 to 10 do begin
		GSetNAtt( NPC, NAG_Skill , T , SkillRank );
	end;

	{ Generate a random name for the character. }
	GSetSAtt( NPC, 'Name', RandomName );

	{ Return a pointer to the character record. }
	RandomSoldier := NPC;
end;

Function NumberOfSkills( PC: GearPtr ): Integer;
	{ Return the number of skills this PC knows. }
var
	T,N: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);
	N := 0;
	for t := 1 to NumSkill do begin
		if GNAttValue( PC, NAG_Skill , T ) > 0 then Inc( N );
	end;
	NumberOfSkills := N;
end;

Function NumberOfSkillSlots( PC: GearPtr ): Integer;
	{ Return the number of skill slots this PC has. }
var
	N: Integer;
begin
	if IsInvalidGear(PC) then Exit(0);
	N := ( ( PC^.STat[ STAT_Knowledge ] * 6 ) div  5 + 5 );
	if GNAttValue( PC, NAG_Talent , NAS_Savant ) <> 0 then N := N + 5;
	NumberOfSkillSlots := N;
end;

Function TooManySkillsPenalty( PC: GearPtr; N: Integer ): Integer;
	{ Return the % XP penalty that this character will suffer. }
begin
	if IsInvalidGear(PC) then Exit(0);
	N := N - NumberOfSkillSlots( PC );
	N := N * 10 - 5;
	if N < 0 then N := 0;
	TooManySkillsPenalty := N;
end;

Function SkillAdvCost( PC: GearPtr; CurrentLevel: Integer ): LongInt;
	{ Return the cost, in XP points, to improve this skill by }
	{ one level. }
const
	chart: Array [1..15] of LongInt = (
		100,100,200,300,400,
		500,800,1300,2100,3400,
		5500,8900,14400,23300,37700
	);
var
	SAC: Int64;
	N: LongInt;
begin
	if IsInvalidGear(PC) then PC := NIL;

	{ The chart lists skill costs according to desired level, }
	{ not current level. So, modify things a bit. }
	Inc( CurrentLevel );

	{ Range check - after level 15, it all costs the same. }
	if CurrentLevel < 1 then CurrentLevel := 1;
	if CurrentLevel <= 15 then begin
		{ Base level advance cost is found in the chart. }
		SAC := chart[ CurrentLevel ];
	end else begin
		{ Base level advance cost is found in the chart. }
		SAC := chart[ 15 ] * ( ( CurrentLevel - 15 ) * ( CurrentLevel - 15 ) + 1 );
	end;

	{ May be adjusted upwards if PC has too many skills... }
	if ( PC <> Nil ) and ( PC^.G = GG_Character ) then begin
		N := TooManySkillsPenalty( PC , NumberOfSkills( PC ) );
		if N > 0 then
			SAC := ( SAC * ( 100 + N ) ) div 100;
	end;

	SkillAdvCost := EnsureRange(SAC, Low(LongInt), High(LongInt));
end;

function IsLegalCharSub( SPC, Part: GearPtr ): Boolean;
	{ Return TRUE if the specified part can be a subcomponent of }
	{ SPC, false if it can't be. }
begin
	if IsInvalidGear(Part) then Exit(False);
	if ( Part^.G = GG_Module ) then IsLegalCharSub := True
	else if ( Part^.G = GG_Modifier ) then IsLegalCharSub := AStringHasBString( GSAttValue( Part, 'TYPE' ) , 'CHARA' )
	else IsLegalCharSub := False;
end;

resourcestring
	PERSONALITYTRAITDESC_EXTREMELY = 'Extremely %s';
	PERSONALITYTRAITDESC_SLIGHTLY = 'Slightly %s';
	PERSONALITYTRAITDESC_VERY = 'Very %s';

Function PersonalityTraitDesc( Trait,Level: Integer ): String;
	{ Return a string which describes the nature & intensity of this }
	{ personality trait. }
var
	msg_pt, msg_lv: String;
begin
	if ( Level = 0 ) or ( Trait < 1 ) or ( Trait > Num_Personality_Traits ) then exit('');
	if Level > 0 then begin
		msg_pt := PTraitDispName[ Trait , 1 ]^;
	end else begin
		msg_pt := PTraitDispName[ Trait , 2 ]^;
	end;

	case Abs(Level) of
		0..24: msg_lv := PersonalityTraitDesc_Slightly;
		25..49: msg_lv := '%s';
		50..74: msg_lv := PersonalityTraitDesc_Very;
		else msg_lv := PersonalityTraitDesc_Extremely
	end;

	PersonalityTraitDesc := Format( msg_lv, [msg_pt] );
end;

Function NPCTraitDesc( NPC: GearPtr ): String;
	{ Describe this NPC's characteristics. This function is used }
	{ for selecting characters for plots & stuff. }
	{ - Age ( Young < 20yo , Old > 40yo ) }
	{ - Gender ( Male, Female ) }
	{ - Personality Traits }
	{ - Exceptional Stats }
	{ - Job }
var
	it: String;
	T,V: Integer;
begin
	if ( NPC = Nil ) or ( NPC^.G <> GG_Character ) then begin
		NPCTraitDesc := '';
	end else begin
		{ PATCH_I18N: Don't translate it. }
		it := 'GENDER:' + GenderName[ GNATtValue( NPC, NAG_CharDescription , NAS_Gender ) ];

		T := GNAttValue( NPC, NAG_CharDescription , NAS_Dage );
		if T < 0 then begin
			it := it + ' young';
		end else if T >= 20 then begin
			it := it + ' old';
		end;

		{ Add descriptors for character traits. }
		for t := 1 to Num_Personality_Traits do begin
			V := GNAttValue( NPC, NAG_CHarDescription , -T );
			if V >= 10 then begin
				it := it + ' ' + PTraitName[ T , 1 ];
			end else if V <= -10 then begin
				it := it + ' ' + PTraitName[ T , 2 ];
			end;
		end;

		{ Add the job description. }
		it := it + ' ' + GSAttValue( NPC, 'JOB' );

		{ Add a note if this NPC has a mecha. }
		if GSATTValue( NPC, 'MECHA' ) <> '' then begin
			it := it + ' HASMECHA';
		end;

		{ Add descriptors for high stats. }
		for t := 1 to 8 do begin
			if NPC^.Stat[ T ] > 13 then it := it + ' ' + StatName[ T ];
		end;

		NPCTraitDesc := it;
	end;
end;

Function CanLearnTalent( PC: GearPtr; T: Integer ): Boolean;
	{ Return TRUE if the PC can learn this talent, or FALSE otherwise. }
begin
	if IsInvalidGear(PC) then Exit(False);

	{ The talent must be within the legal range in order to be }
	{ learned. }
	if not inRange( T , 1, NumTalent ) then Exit(False);

	{ The PC can't learn the same talent twice. }
	if GNAttValue( PC, NAG_Talent , T ) <> 0 then exit(False);
	if ( Talent_PreReq[ T , 1 ] = 0 ) or ( Talent_PreReq[ T , 2 ] = 0 ) then exit(True);

	if ( Talent_PreReq[ T , 1 ] > 0 ) then begin
		CanLearnTalent := GNAttValue( PC, NAG_Skill , Talent_PreReq[ T , 1 ] ) >= Talent_PreReq[ T , 2 ];

	end else if ( Talent_PreReq[ T , 1 ] < -8 ) then begin
		if Talent_PreReq[ T , 2 ] < 0 then begin
			CanLearnTalent := GNAttValue( PC, NAG_CharDescription , Talent_PreReq[ T , 1 ] + 8 ) <= Talent_PreReq[ T , 2 ];
		end else begin
			CanLearnTalent := GNAttValue( PC, NAG_CharDescription , Talent_PreReq[ T , 1 ] + 8 ) >= Talent_PreReq[ T , 2 ];
		end;

	end else begin
		CanLearnTalent := PC^.Stat[ Abs( Talent_PreReq[ T , 1 ] ) ] >= Talent_PreReq[ T , 2 ];

	end;
end;

Function NumFreeTalents( PC: GearPtr ): Integer;
	{ Return the number of talents the PC can learn. }
var
	N: Integer;

	Function countTalent(constref node: NANode; arg: Pointer):Boolean;
	begin
		inc(N);
		result := True;
	end;
var
	XP: LongInt;
begin
	if IsInvalidGear(PC) then Exit(0);

	{ Start by counting the number of talents the PC currently has. }
	N := 0;
	traverseGNAtt(PC, NAG_Talent, @countTalent, nil);

	{ Subtract this from the total number of talents the PC can get, }
	{ based on Experience. }
	XP := GNAttValue( PC, NAG_Experience , NAS_TotalXP );
	if XP > 100000 then N := 5 - N
	else if XP > 60000 then N := 4 - N
	else if XP > 30000 then N := 3 - N
	else if XP > 10000 then N := 2 - N
	else N := 1 - N;

	NumFreeTalents := N;
end;

Procedure ApplyIdealism( PC: GearPtr );
	{ Apply a +1 modifier to three random stats. }
var
	T,T2,S: Integer;
	StatDeck: Array [1..NumGearStats] of Integer;
begin
	if IsInvalidGear(PC) then Exit;

	{ Start by shuffling the statdeck. }
	for t := 1 to NumGearStats do StatDeck[ t ] := T;
	for t := 1 to NumGearStats do begin
		S := Random( NumGearStats ) + 1;
		T2 := StatDeck[ t ];
		StatDeck[ t ] := StatDeck[ S ];
		StatDeck[ S ] := T2;
	end;

	{ Select the first three stats off the top of the deck. }
	Inc( PC^.Stat[ StatDeck[ 1 ] ] );
	Inc( PC^.Stat[ StatDeck[ 2 ] ] );
	Inc( PC^.Stat[ StatDeck[ 3 ] ] );
end;

Procedure ApplyTalent( PC: GearPtr; T: Integer );
	{ Apply the listed talent to the PC, invoking any special effects }
	{ if needed. }
begin
	if IsInvalidGear(PC) then Exit;

	{ Start with an error check. }
	if not inRange(T, 1, NumTalent) then Exit;

	{ Record the talent. }
	GSetNAtt( PC, NAG_Talent , T , 1 );

	{ Depending on the talent, do various effects. }
	with PC^ do Case T of
		NAS_StrengthOfFaith:
			inc(Stat[ STAT_Ego ], 2);
		NAS_BodyBuilder:
			inc(Stat[ STAT_Body ], 2);
		NAS_ScientificMethod:
			inc(Stat[ STAT_Knowledge ], 2);
		NAS_Presence:
			inc(Stat[ STAT_Charm ], 2);
		NAS_Idealist:
			ApplyIdealism( PC );
	end;
end;

resourcestring
	JOBAGEGENDERDESC_FMT = '%0:d year old %1:s %2:s.';

Function JobAgeGenderDesc( NPC: GearPtr ): String;
	{ Return the Job, Age, and Gender of the provided character in }
	{ a nicely formatted string. }
var
	Job: String;
	G: Integer;
begin
	if IsInvalidGear(NPC) then Exit;

	job := GSAttValue( NPC, 'JOB' );
	if job = '' then job := 'Another job';
	G := GNAttValue( NPC, NAG_CharDescription , NAS_Gender );
	JobAgeGenderDesc := Format( JobAgeGenderDesc_FMT, [
		GNAttValue( NPC, NAG_CharDescription , NAS_DAge ) + 20,
		IfThen(G <> NAV_Undefined, GenderDispName[ G ]^, ''),
		I18N_Name(job)
		] );
end;

Function RepairSkillIndex(Skill:Integer):Integer;
var
	tmp:Integer;
begin
	result := 1;
	for tmp in USAGE_Repair_p do begin
		if tmp = Skill then exit;
		Inc(result);
	end;
	result := 0;
end;

Function ClueSkillIndex(Skill:Integer): Integer;
var
	tmp:Integer;
begin
	result := 1;
	for tmp in USAGE_Clue do begin
		if tmp = Skill then exit;
		Inc(result);
	end;
	result := 0;
end;


initialization
{$IFDEF DEBUG}
	ErrorMessage_fork('DEBUG: ghchars.pp');
{$ENDIF DEBUG}

finalization
{$IFDEF DEBUG}
	ErrorMessage_fork('DEBUG: ghchars.pp(finalization)');
{$ENDIF DEBUG}

end.
