/////////////////////////////////////////////////////////////////////////////////////////////
///<Filename>  		SCredits.cpp			<\Filename>
///<Date>      		10/29/2011			<\Date>
///<Author>   		Mitchel Weisrock	<\Author>
///<Purpose>
///		Credits
///<\Purpose>
/////////////////////////////////////////////////////////////////////////////////////////////

#include "SCredits.h"
#include "../../Sound/SoundDefinitions.h"
#include "../../Sound/SoundSystem.h"
#include "../../Event/Types/ESetSoundVolume.h"

#include "../../Event/EventSystem.h"
#include "../../Event/Types/ELoadSound.h"
#include "../../Event/Types/EUnloadSound.h"
#include "../../Event/Types/EPlaySound.h"
#include "../../Sound/SoundSystem.h"

#include "../../Input/GameInput.h"

#include "../../Main/MainEngine.h"

SCredits::SCredits()
{
	Credit devCredits[MAX_DEV];
	Credit artCredits[MAX_ART];
	Credit thanksCredits[MAX_THANKS];


	//480 height on images
	devCredits[RICHARD_SMALL].Load		("Assets/Images/Credits/2D_Credits_Smalls.png", 150, 265, "TECHNICAL LEAD", "RICHARD SMALL",
		"I want to thank my sister, Kristen, the most. \nI would never have even had this opportunity \nto go to this school without her. \nMy father is a patient man who \nhas put up with me for the past 3 years, \nhelping me get through this school. \nMy father is a good man.");
	
	devCredits[BEN_VALENA].Load			("Assets/Images/Credits/2D_Credits_Ben.png", 150, 265, "EFFECTS MASTER", "BEN VALENA",
		"Thanks to all my family \nwho have been supportive in \nmaking sure I had a roof over my head, \nfood to eat, and checking to make sure \nI am still alive, every once in a while. \nThanks to my team for being awesome and \nproductive(usually), for all the laughs and \njokes, the fights and late nights, the puns \nand DnD nights, and most of all the memories. \nI am thinking its Muffin Time.");
	
	devCredits[REBECCA_HOWELL].Load		("Assets/Images/Credits/2D_Credits_Rebecca.png", 150, 265, "COLLISION GURU", "REBECCA HOWELL",
		"I would like to thank Kevin for \nkeeping me on track. I love you! \nAlso, thank you to my parents for supporting \nme along the way and motivating me \nto make something of myself. \nTo the team, it is thanks to you \nguys and your hard work that we were \nable to make this happen!");
	
	devCredits[JAMES_YOUNT].Load		("Assets/Images/Credits/2D_Credits_James.png", 150, 265, "FATHER OF MACHINES ", "JAMES YOUNT",
		"Thank you to my parents for being so supportive \nduring this endeavor and to my team \nfor being so cool and teaching me \nthat art and dev are truly two people \nseparated by a common language attempting to \nperform rocket surgery while \nunderwater. Muffin Time?");
	
	devCredits[BRIAN_ARPIDONE].Load		("Assets/Images/Credits/2D_Credits_Brian.png", 150, 265, "ASSET LEAD", "BRIAN ARPIDONE",
		"Thanks to my family \nfor all the support. \nFor my mom, who made sure I was eating, \nfor my sister, who made sure I was well-dressed, \nand for my brother, who was always there \nto swap a good programming horror story. \nEspecially thanks to my team, who taught \nme the best lesson of all: \nMuffin time is 6:30.");
	
	devCredits[JERRY_SCOUGHTON].Load	("Assets/Images/Credits/2D_Credits_Jerry.png", 150, 265, "SOUND ENGINEER", "JERRY SCOUGHTON",
		"I would like to thank my mother for playing the \nKings Quests Series with my sister and I \nwhile we were kids which put the passion of computers \ninto me at an early age.  \nI would like to thank my ex-wife for the \nfinal kick that put me into school at \nFull Sail University. I would like my girlfriend for putting \nup with my abnormal work hours I have \nput in while at school. \nhmmmmm should I get a hair cut now? Naahhhhhhhh....=D");
	
	devCredits[CHRIS_BYRNES].Load		("Assets/Images/Credits/2D_Credits_Chris.png", 150, 265, "CAMERA MAN", "CHRIS BYRNES",
		"It has been quite a trip. \nThank you to all of my teachers and \nlab instructors who helped me along. \nTo all the people I met and all \nthe friends I made good luck in the \nfuture with all your endeavors. \nFinally I want to thank my \nMom and Dad for helping me through school \nand supporting me the whole time!");
	
	devCredits[MITCHEL_WEISROCK].Load	("Assets/Images/Credits/2D_Credits_Mitchel.png", 150, 265, "INTERFACE GUY", "MITCHEL WEISROCK",
		"I would like to say \nthank you to my parents \nand grandparents for supporting \nme going to school \nand helping me stick with it. Also we \nneed more storyboards.");

	
	
	artCredits[GABRIEL_CARDOSO].Load	("Assets/Images/Credits/2D_Credits_Gabe.png", 150, 265, "FOREIGN LIASON", "GABRIEL CARDOSO",
		"I would like to thank my parents \nand my family, also my friends.");
	
	artCredits[LOUIS_DORAN].Load		("Assets/Images/Credits/2D_Credits_Louis.png", 150, 265, "ENVIRONMENT ARTIST", "LOUIS DORAN",
		"So what youre saying is...");
	
	artCredits[ERIC_HOLLOWAY].Load		("Assets/Images/Credits/2D_Credits_Eric.png", 150, 265, "PROP LEAD", "ERIC HOLLAWAY",
		"I can not believe how far I have come in the past 2 years. \nI would not have been able to do it without the \nlove and support from my family through this journey. \nThanks to Amanda for never giving up on me and sticking \naround even in the worst of times, I love you! \nTo all my friends and especially my teammates, I love you all \nand could not be happier with the result of game project!  \nLETS DO THIS!");
	
	artCredits[RYAN_NASH].Load			("Assets/Images/Credits/2D_Credits_Ryan.png", 150, 265, "ART LEAD", "RYAN NASH",
		"What a ride! Thanks to all my teachers \nand friends here at Full Sail, \nthanks to the team for such an amazing experience, \nthanks to all the people who prayed \nfor me especially my peeps back home, \nthanks to Jess for keeping me sane (<3), \nthanks to my dad for being so supportive \nand sharing knowledge (love you dad!), \nand thanks to God for pouring out endless blessing. \nLove to all!");
	
	artCredits[ROBERT_REATGUI].Load		("Assets/Images/Credits/2D_Credits_Rob.png", 300, 265, "EFFECTS/CHARACTER ARTIST",
		"ROBERT REATGUI", "Thanks to everyone who \nhelped me get here, \nbut its just the beginning. \nP.S. Hey guys I got like 5 more \nthings to add to the game, \nand its not gonna be easy to put in.");

	
	
	thanksCredits[LAUREN_HOLLAWAY].Load		(NULL, 0, 0, "ANIMATION EXPERT", "LAUREN HOLLOWAY",
		"Taken from us far too early");
	thanksCredits[DAVID_HOBGOOD].Load		(NULL, 0, 0, "THE AMAZING MAN", "DAVID HOBGOOD",
		"David lead us to greatness");
	thanksCredits[MIKE_LEBO].Load			(NULL, 0, 0, "PRODUCTION LEAD", "MIKE LEBO",
		"Your help with animations is never complete!");
	thanksCredits[CARLOS_LUGO].Load			(NULL, 0, 0, "EXECUTIVE PRODUCER", "CARLOS LUGO",
		"The guy who rated our game as   NOT BAD");
	thanksCredits[PHIL_MARUNOWSKI].Load		(NULL, 0, 0, "ART DIRECTOR", "PHIL MARUNOWSKI",
		"Leading the art team to \nthe pinnacle of thier ability");
	thanksCredits[TROY_SIMPSON].Load		(NULL, 0, 0, "SOUND INTERN", "TROY SIMPSON",
		"Music is your game");
	thanksCredits[PATRICK_KELLY].Load		(NULL, 0, 0, "GOD OF PRODUCERS", "PATRICK KELLY",
		"Being a hard ass with a purpose");
	thanksCredits[RYAN_EDWARDS].Load		(NULL, 0, 0, "THE BEARD OF LOVE", "RYAN EDWARDS",
		"Your beard is what kept us going");
	thanksCredits[JASON_HINDERS].Load		(NULL, 0, 0, "TECHNICAL LEAD PRODUCER", "JASON HINDERS",
		"Always stating the hard truth");
	thanksCredits[JUSTIN_MURPHY].Load		(NULL, 0, 0, "THE RENDERING EXPERT", "JUSTIN MURPHY",
		"Normal maps... Thank You!");
	thanksCredits[CASEY_COFFMAN].Load		(NULL, 0, 0, "SOUND ENGINEER", "CASEY COFFMAN",
		"Your sounds make the game");
	thanksCredits[ROD_MOYE].Load			(NULL, 0, 0, "DESIGN LEAD PRODUCER", "ROD MOYE",
		"Making sure we get the core game in");
	thanksCredits[CHRIS_MARKS].Load			(NULL, 0, 0, "VOICE ACTOR", "CHRIS MARKS",
		"Your voice makes the game sound amazing");
	thanksCredits[GP_STAFF].Load			(NULL, 0, 0, "GP STAFF", "GP STAFF",
		"Always making sure our lives \nwere as hard as possible, \nbut always leading us \ntowards an amazing game");

	thanks.push_back("Rokos Bane Team");
	thanks.push_back("Life Team");
	thanks.push_back("Dinocalypse Team"); 
	thanks.push_back("Our wonderful pets");
	thanks.push_back("All the game dev teachers");
	thanks.push_back("All the game art teachers");
	thanks.push_back("Moes");
	thanks.push_back("Blueberry Muffins");

	for(int i = 0; i < MAX_DEV; ++i)
	{
		credits[DEV_TEAM].push_back(devCredits[i]);
	}

	for(int i = 0; i < MAX_ART; ++i)
	{
		credits[ART_TEAM].push_back(artCredits[i]);
	}

	for(int i = 0; i < MAX_THANKS; ++i)
	{
		credits[SPECIAL_THANKS].push_back(thanksCredits[i]);
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Enter </Summary>
///
/// <Param name="previousState">	The previous state </Param>
///
/// <Remarks>
///		This will be called whenever this state has been pushed onto the stack
///	</Remarks>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Enter(const HashString& previousState)
{
	// Play Music for GamePlayState
	EPlaySound * soundEvent = EventSystem::GetInstance()->SendEvent<EPlaySound>(EPlaySound::EVENTID);
	soundEvent->typeOfSound = MUSIC;
	soundEvent->whichSound = CREDITS_BACKGROUND;

	RenderSystem::GetInstance()->LoadTexture(groupPhoto, "Assets/Images/2D_Credits_GroupPhoto.png");
	RenderSystem::GetInstance()->LoadTexture(forsakenLogo, "Assets/Images/2D_Start_ForsakenStudios.png");

	RenderSystem::GetInstance()->LoadTexture(mayaLogo, "Assets/Images/2D_Credits_Maya.png");
	RenderSystem::GetInstance()->LoadTexture(photoshopLogo, "Assets/Images/2D_Credits_PhotoShop.png");
	RenderSystem::GetInstance()->LoadTexture(directxLogo, "Assets/Images/2D_Credits_DirectX.png");
	RenderSystem::GetInstance()->LoadTexture(fmodLogo, "Assets/Images/2D_Credits_FMOD.png");
	RenderSystem::GetInstance()->LoadTexture(visualstudioLogo, "Assets/Images/2D_Credits_VisualStudio.png");
	
	extraThanksPos = 950.0f;
	showExit = false;
	fadeOut = false;
	fadeTime = 0.0f;
	fadeTimer.Reset();
	showExit = false;
	skipTimer.Reset();

	currentType = DEV_TEAM;
	currentState = CREDITS_FORSAKENLOGO_FADEIN;
	switchTimer.Reset();
	switchFade = 0.0f;
	currentCredit = 0;
	showPicture = true;

	RenderSystem::GetInstance()->SetScreenAlpha(0.0f);

	EUnloadSound * soundUnloadEvent = EventSystem::GetInstance()->SendEvent<EUnloadSound>(EUnloadSound::EVENTID);
	soundUnloadEvent->typeOfSound = MUSIC;
	soundUnloadEvent->whichSound = MAINMENU_BACKGROUND;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Exit </Summary>
///
/// <Remarks>
///		This will be called whenever this state is popped from the stack
///	</Remarks>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Exit()
{
	EUnloadSound * soundEvent = EventSystem::GetInstance()->SendEvent<EUnloadSound>(EUnloadSound::EVENTID);
	soundEvent->typeOfSound = MUSIC;
	soundEvent->whichSound = CREDITS_BACKGROUND;

	RenderSystem::GetInstance()->SetScreenAlpha(1.0f);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Resume </Summary>
///
/// <Param name="previousState">	The previous state </Param>
///
/// <Remarks>
///		This will be called whenever this state resumes its place at the top of the stack
///	</Remarks>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Resume(const HashString& previousState)
{
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Override </Summary>
///
/// <Remarks>
///		This will be called whenever this state stops being at the top of the stack
///	</Remarks>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Override()
{
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Input </Summary>
///
/// <Param name="delta">	Delta time value </Param>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Input()
{
	if(GameInput::GetInstance()->Enter() && showExit)
	{
		fadeOut = true;
	}

	if(GameInput::GetInstance()->Enter())
	{
		showExit = true;
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Update </Summary>
///
/// <Param name="delta">	Delta time value </Param>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Update()
{
	if(fadeOut)
	{
		fadeTimer.Update();
		fadeTime -= fadeTimer.GetAvgDelta() * 100.0f;

		if(fadeTime <= 0.0f)
		{
			fadeTime = 0.0f;
			fadeTimer.Reset();
			SceneSystem::GetInstance()->ChangeScene(SMainMenu::SCENEID);
		}

		return;
	}

	if(showExit)
	{
		skipTimer.Update();

		if(skipTimer.GetTotalTime() >= 3.0f)
		{
			showExit = false;
			skipTimer.Reset();
		}
	}

	switchTimer.Update();

	switch(currentState)
	{
	case CREDITS_TOOLS_FADEIN:
		{
			switchFade += switchTimer.GetAvgDelta();

			if(switchFade >= 1.0f)
			{
				switchFade = 1.0f;
				switchTimer.Reset();
				currentState = CREDITS_TOOLS_HOLD;
			}
		}
		break;
	case CREDITS_TOOLS_FADEOUT:
		{
			switchFade -= switchTimer.GetAvgDelta();

			if(switchFade <= 0.0f)
			{
				switchFade = 0.0f;
				switchTimer.Reset();
				currentState = CREDITS_DONE;
			}
		}
		break;
	case CREDITS_TOOLS_HOLD:
		{
			if(switchTimer.GetTotalTime() >= 5.0f)
			{
				switchTimer.Reset();
				currentState = CREDITS_TOOLS_FADEOUT;
			}
		}
		break;
	case CREDITS_FORSAKENLOGO_FADEIN:
		{
			switchFade += switchTimer.GetAvgDelta();

			if(switchFade >= 1.0f)
			{
				switchFade = 1.0f;
				switchTimer.Reset();
				currentState = CREDITS_FORSAKENLOGO_HOLD;
			}
		}
		break;
	case CREDITS_FORSAKENLOGO_FADEOUT:
		{
			switchFade -= switchTimer.GetAvgDelta();

			if(switchFade <= 0.0f)
			{
				switchFade = 0.0f;
				switchTimer.Reset();
				currentState = CREDITS_FADE_IN;
			}
		}
		break;
	case CREDITS_FORSAKENLOGO_HOLD:
		{
			if(switchTimer.GetTotalTime() >= 5.0f)
			{
				switchTimer.Reset();
				currentState = CREDITS_FORSAKENLOGO_FADEOUT;
			}
		}
		break;
	case CREDITS_FADE_IN:
		{
			switchFade += switchTimer.GetAvgDelta();

			if(switchFade >= 1.0f)
			{
				switchFade = 1.0f;
				switchTimer.Reset();
				currentState = CREDITS_HOLD;
			}
		}
		break;
	case CREDITS_FADE_OUT:
		{
			switchFade -= switchTimer.GetAvgDelta();

			if(switchFade <= 0.0f)
			{
				switchFade = 0.0f;
				switchTimer.Reset();
				currentState = CREDITS_FADE_IN;

				if(showPicture)
				{
					showPicture = false;
					break;
				}

				currentCredit++;

				switch(currentType)
				{
				case DEV_TEAM:
					{
						if(currentCredit >= MAX_DEV)
						{
							currentType++;
							currentCredit = 0;
						}
					}
					break;
				case ART_TEAM:
					{
						if(currentCredit >= MAX_ART)
						{
							currentType++;
							currentCredit = 0;
							switchTimer.SetSpeed(2.5f);
						}
					}
					break;
				case SPECIAL_THANKS:
					{
						if(currentCredit >= MAX_THANKS)
						{
							currentType++;
							currentCredit = 0;
						}
					}
					break;
				}

				if(currentType >= MAX_CREDIT_TYPES)
				{
					currentState = CREDITS_EXTRA;
					switchTimer.SetSpeed(1.0f);
					currentType = 0;
				}

				if(credits[currentType][currentCredit].doesPicExist)
				{
					showPicture = true;
				}
			}
		}
		break;
	case CREDITS_HOLD:
		{
			float wait = 7.5f;

			if(showPicture)
				wait = 3.0f;

			if(switchTimer.GetTotalTime() >= wait)
			{
				switchTimer.Reset();
				currentState = CREDITS_FADE_OUT;
			}
		}
		break;
	case CREDITS_EXTRA:
		{
			extraThanksPos -= switchTimer.GetAvgDelta()  * 75.0f;

			if(extraThanksPos + (thanks.size() * 150.0f) < -100.0f)
			{
				switchTimer.Reset();
				currentState = CREDITS_TOOLS_FADEIN;
			}
		}
		break;
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// <Summary>	Render </Summary>
///
/// <Param name="delta">	Delta time value </Param>
////////////////////////////////////////////////////////////////////////////////////////////////////
void SCredits::Render()
{
	if(showExit)
	{
		sRenderString str(FONT_GUNBLADE, "Press Enter to Exit", 5, 700, 0.7f);
		RenderSystem::GetInstance()->RenderForward(&str);
	}

	if( currentState == CREDITS_DONE )
	{
		groupPhoto.nY = 38;
		RenderSystem::GetInstance()->RenderForward(&groupPhoto);
		return;
	}

	if( currentState == CREDITS_EXTRA )
	{
		sRenderString extraThanks(FONT_GUNBLADE, "E X T R A  T H A N K S", MainEngine::GetInstance()->windowWidth / 2.0f,
			extraThanksPos - 250.0f, 1.5f, D3DCOLOR_ARGB(255, 255, 69, 0), true );

		RenderSystem::GetInstance()->RenderForward(&extraThanks);

		for(unsigned i = 0; i < thanks.size(); ++i)
		{
			sRenderString str(FONT_GUNBLADE, thanks[i].c_str(), MainEngine::GetInstance()->windowWidth / 2.0f,
				extraThanksPos + (150.0f * i), 1.0f, D3DCOLOR_ARGB(255, 255, 69, 0), true );

			RenderSystem::GetInstance()->RenderForward(&str);
		}

		return;
	}

	if(	currentState == CREDITS_FORSAKENLOGO_FADEIN ||
		currentState == CREDITS_FORSAKENLOGO_FADEOUT ||
		currentState == CREDITS_FORSAKENLOGO_HOLD )
	{
		forsakenLogo.dwColor = D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);
		forsakenLogo.nX = 130.0f;
		forsakenLogo.nY = 100.0f;
		forsakenLogo.fScaleX = 0.75f;
		forsakenLogo.fScaleY = 0.75f;

		sRenderString str(FONT_GUNBLADE, "C R E D I T S", MainEngine::GetInstance()->windowWidth / 2.0f,
			20.0f, 2.5f, D3DCOLOR_ARGB((int)(switchFade * 255), 255, 69, 0), true );

		RenderSystem::GetInstance()->RenderForward(&str);
		RenderSystem::GetInstance()->RenderForward(&forsakenLogo);
		return;
	}

	if(	currentState == CREDITS_TOOLS_FADEIN ||
		currentState == CREDITS_TOOLS_FADEOUT ||
		currentState == CREDITS_TOOLS_HOLD )
	{
		fmodLogo.dwColor =			D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);
		photoshopLogo.dwColor =		D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);
		mayaLogo.dwColor =			D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);
		directxLogo.dwColor =		D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);
		visualstudioLogo.dwColor =  D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);

		fmodLogo.nX = 45.0f;
		fmodLogo.nY = 200.0f;

		photoshopLogo.nX = 25.0f;
		photoshopLogo.nY = 480.0f;

		mayaLogo.nX = 300.0f;
		mayaLogo.nY = 480.0f;

		directxLogo.nX = 590.0f;
		directxLogo.nY = 200.0f;
		directxLogo.fScaleX = 0.35f;
		directxLogo.fScaleY = 0.5f;

		visualstudioLogo.nX = 580.0f;
		visualstudioLogo.nY = 480.0f;
		visualstudioLogo.fScaleX = 0.8f;
		//visualstudioLogo.fScaleY = 0.5f;

		sRenderString str(FONT_GUNBLADE, "T O O L S", MainEngine::GetInstance()->windowWidth / 2.0f,
			20.0f, 2.5f, D3DCOLOR_ARGB((int)(switchFade * 255), 255, 69, 0), true );

		RenderSystem::GetInstance()->RenderForward(&str);
		RenderSystem::GetInstance()->RenderForward(&fmodLogo);
		RenderSystem::GetInstance()->RenderForward(&photoshopLogo);
		RenderSystem::GetInstance()->RenderForward(&mayaLogo);
		RenderSystem::GetInstance()->RenderForward(&directxLogo);
		RenderSystem::GetInstance()->RenderForward(&visualstudioLogo);
		return;
	}

	sRenderString type;

	switch(currentType)
	{
	case DEV_TEAM:
		{
			type = sRenderString(FONT_GUNBLADE, "D E V  T E A M",
						MainEngine::GetInstance()->windowWidth / 2.0f, 10.0f, 1.5f,
						D3DCOLOR_ARGB(255, 255, 69, 0), true );
		}
		break;
	case ART_TEAM:
		{
			type = sRenderString(FONT_GUNBLADE, "A R T  T E A M",
						MainEngine::GetInstance()->windowWidth / 2.0f, 10.0f, 1.5f,
						D3DCOLOR_ARGB(255, 255, 69, 0), true );
		}
		break;
	case SPECIAL_THANKS:
		{
			type = sRenderString(FONT_GUNBLADE, "S P E C I A L  T H A N K S",
						MainEngine::GetInstance()->windowWidth / 2.0f, 10.0f, 1.5f,
						D3DCOLOR_ARGB(255, 255, 69, 0), true );
		}
		break;
	}

	int titleFade = (int)(switchFade * 255);
	float namePos = 120.0f;
	float titlePos = 200.0f;
	float postPos = 300.0f;

	if(currentType != SPECIAL_THANKS)
	{
		if(currentState == CREDITS_FADE_IN && showPicture == false)
		{
			titleFade = 255;
		}
		else if(currentState == CREDITS_FADE_OUT && showPicture == true)
		{
			titleFade = 255;
		}
	}
	else
	{
		namePos = 225.0f;
		titlePos = 350.0f;
		postPos = 500.0f;
	}

	sRenderString name(FONT_GUNBLADE, credits[currentType][currentCredit].name.c_str(),
		MainEngine::GetInstance()->windowWidth / 2.0f, namePos, 1.0f, D3DCOLOR_ARGB(titleFade, 255, 69, 0), true );

	sRenderString title(FONT_GUNBLADE, credits[currentType][currentCredit].title.c_str(),
		MainEngine::GetInstance()->windowWidth / 2.0f, titlePos, 0.8f, D3DCOLOR_ARGB(titleFade, 255, 255, 255), true );

	float scale = 0.65f;

	if( (currentType == DEV_TEAM && currentCredit == JERRY_SCOUGHTON) ||
		(currentType == DEV_TEAM && currentCredit == BRIAN_ARPIDONE) ||
		(currentType == DEV_TEAM && currentCredit == JAMES_YOUNT) ||
		(currentType == ART_TEAM && currentCredit == ERIC_HOLLOWAY ) ||
		(currentType == ART_TEAM && currentCredit == RYAN_NASH ))
	{
		scale = 0.5f;
	}

	sRenderString post(FONT_GUNBLADE, credits[currentType][currentCredit].post.c_str(),
		MainEngine::GetInstance()->windowWidth / 2.0f, postPos,
		scale, D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255), true, true );

	RenderSystem::GetInstance()->RenderForward(&type);
	RenderSystem::GetInstance()->RenderForward(&title);
	RenderSystem::GetInstance()->RenderForward(&name);
	if(showPicture)
	{
		credits[currentType][currentCredit].picture.dwColor = D3DCOLOR_ARGB((int)(switchFade * 255), 255, 255, 255);
		RenderSystem::GetInstance()->RenderForward(&credits[currentType][currentCredit].picture);
	}
	else
	{
		RenderSystem::GetInstance()->RenderForward(&post);
	}
}