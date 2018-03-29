unit ICQWorks {TICQClient, TICQDb constants & procedures};
{(C) Alex Demchenko(alex@ritlabs.com)}
{Modified by Oleg Kozachok (oleg@kozachok.net.ua) 2006-2007}

{Modified by Tsar Ioann XIII (Volkov Ioann) 2008-2009}
{ http://progs.volkov.spb.su/ticqclient/ }


{$R-}                           //Remove range checking
{$DEFINE USE_ASM}               //Remove this line to use pascal code instead of assembler (where it's possible)

interface
uses
  Windows, Classes;

const
  MAX_DATA_LEN = 8192;          //Maximum packet size


  //Online statuses
  S_ONLINE      = $00000000;    //Online
  S_INVISIBLE   = $00000100;    //Invisible
  S_AWAY        = $00000001;    //Away
  S_NA          = $00000005;    //N/A
  L_S_NA        = $00000004;    // added by eraser 8.7.03
  S_OCCUPIED    = $00000011;    //Occupied
  L_S_OCCUPIED  = $00000010;    // added by eraser 8.7.03
  S_DND         = $00000013;    //Do Not Disturb
  L_S_DND       = $00000012;    // added by eraser 8.7.03
  S_FFC         = $00000020;    //Free For Chat
  S_OFFLINE     = $FFFFFFFF;    //The user is offline. / Set status to offline

  //Volkov Ioann got this statuses by OnStatusChange from QIP, works through ass
  S_QIP_HOME = 268455936;
  S_QIP_WORK = 268460032;
  S_QIP_DEPRESSION = 268451840;
  S_QIP_ANGRY = 268447744;
  S_QIP_EATING = 268443649;
  S_QIP_NA = 268435460;
  S_QIP_OCCUPIED = 268435472;
  S_QIP_DND = 268435458;
  S_QIP_INVISIBLE = 268435712;

  //Volkov Ioann got this statuses from: http://yxu.org.ru/index.php?do=comments&id=1&cat=icq_inf
  S_HOME = $00005000;
  S_WORK = $00006000;
  S_DEPRESSION = $00004000;
  S_ANGRY = $00003000;
  S_EATING = $00002001;

  SF_BIRTH      = $00080000;    //Birtday

  //Flags used with statuses
  S_SHOWIP      = $00020000;    //show ip (for older clients), IP edit removed in ICQ2000a+ :)
  S_WEBAWARE    = $00030000;    //do not show status from the www
  S_ALLOWDCONN  = $00000000;    //allow direct connection with everyone
  S_ALLOWDAUTH  = $10000000;    //allow direct connection upon authorization
  S_ALLOWDLIST  = $20000000;    //allow direct connection with users in contact list

  //Message types
  M_PLAIN         = $01;        //Plain message
  M_CHAT          = $02;        //Chat request
  M_FILE          = $03;        //File request
  M_URL           = $04;        //URL
  M_AUTH_REQ      = $06;        //Auth request
  M_AUTH_DENIED   = $07;        //Deny auth
  M_AUTH_GIVEN    = $08;        //Authorize
  M_WEB_PAGE      = $0D;
  M_EMAIL_EXPRESS = $0E;
  M_CONTACTS      = $13;
  M_ADVANCED      = $1A;        //Seems to be an advanced-msg type (contacts, file requests, etc)

  //Genders
  GEN_FEMALE    = 1;            //Gender: Female
  GEN_MALE      = 2;            //Gender: Male

  //CLI_TOICQSRV commands
  CMD_ACKOFFMSG = $3E;
  CMD_REQOFFMSG = $3C;
  CMD_REQINFO   = $7D0;

  TFLAPSZ: Word = 6;            //Size of FLAP header
  TSNACSZ: Word = 10;           //Size of SNAC header

  //SRV UIN Flags
  U_NORMAL         = $0000;     //Normal list entry
  U_VISIBLE_LIST   = $0002;     //User in visible list
  U_INVISIBLE_LIST = $0003;     //User in invisible list
  U_IGNORE_LIST    = $000e;     //User in ignore list


  ACC_NORMAL      = $0;         //Normally accepted
  ACC_NO_OCCUPIED = $9;         //Not accepted, occupied
  ACC_NO_DND      = $A;         //Not accepted, dnd
  ACC_AWAY        = $4;         //Accepted but away
  ACC_NA          = $E;         //Accepted but NA
  ACC_CONTACTLST  = $C;         //Accepted to contact list (no blink in tray)

  //Auto message requests
  GET_AWAY        = $E8;        //Get away message
  GET_OCCUPIED    = $E9;        //Get occupied message
  GET_NA          = $EA;        //Get N/A message
  GET_DND         = $EB;        //Get DND message
  GET_FFC         = $EC;        //Get FFC(Free For Chat) message

  //Message flags
  MFLAG_NORMAL    = $01;        //Normal message
  MFLAG_AUTO      = $03;        //Auto-message flag
  MFLAG_MULTI     = $80;        //This is multiple recipients message

  //Buddy types
  BUDDY_NORMAL    = $0000;      //A normal contact list entry
  BUDDY_GROUP     = $0001;      //A larger group header
  BUDDY_IGNORE    = $000e;      //A contact on the ignore list
  BUDDY_INVISIBLE = $0003;      //A contact on the invisible list
  BUDDY_VISIBLE   = $0002;      //A contact on the visible list

  //SSL errors
  ERRSSL_NOTFOUND = $0002;      //User not found
  ERRSSL_EXISTS   = $0003;      //Added buddy already exists
  ERRSSL_AUTH     = $000e;      //User not authorized
  ERRSSL_OTHER    = $000a;      //Other SSL error
  ERRSSL_NOERROR  = $0000;      //No error (changed successfully)

  TLV_AUTH        = $02F8;  //   uint8      User authorization permissions
  TLV_WEBAWARE    = $030C;  //   uint8      User 'show web status' permissions
  // common
  TLV_FIRSTNAME   = $0140;  //   sstring    User firstname
  TLV_LASTNAME    = $014A;  //   sstring    User lastname
  TLV_NICKNAME    = $0154;  //   sstring    User nickname
  TLV_EMAIL       = $015E;  //   ecombo     User email
  TLV_GENDER      = $017C;  //   uint8      User gender
  TLV_MARITAL     = $033E;  //   uint8      User marital status
  TLV_LANGUAGE    = $0186;  //   uint16     User spoken language
  TLV_CITY        = $0190;  //   sstring    User home city name
  TLV_STATE       = $019A;  //   sstring    User home state abbr
  TLV_COUNTRY     = $01A4;  //   uint16     User home country code
  TLV_COMPANY     = $01AE;  //   sstring    User work company name
  TLV_DEPARTMENT  = $01B8;  //   sstring    User work department name
  TLV_POSITION    = $01C2;  //   sstring    User work position (title)
  TLV_OCUPATION   = $01CC;  //   uint16     User work ocupation code
  TLV_PASTINFO    = $01D6;  //   icombo     !! User affilations node
  TLV_AFFILATIONS = $01FE;  //   icombo     !! User past info node
  TLV_INTERESTS   = $01EA;  //   icombo     User interests node
  TLV_HOMEPAGE    = $0212;  //   sstring    User homepage category/keywords

  // changeinfo
  TLV_AGE         = $0172;  //   uint16     User age
  TLV_URL         = $0213;  //   sstring    User homepage url
  TLV_BIRTH       = $023A;  //   bcombo     User birthday info (year, month, day)
  TLV_ABOUT       = $0258;  //   sstring    User notes (about) text
  TLV_STREET      = $0262;  //   sstring    User home street address
  TLV_ZIPCODE     = $026C;  //   uint32     User home zip code
  TLV_PHONE       = $0276;  //   sstring    User home phone number
  TLV_FAX         = $0280;  //   sstring    User home fax number
  TLV_MOBILE      = $028A;  //   sstring    User home cellular phone number
  TLV_WORKSTREET  = $0294;  //   sstring    User work street address
  TLV_WORKCITY    = $029E;  //   sstring    User work city name
  TLV_WORKSTATE   = $02A8;  //   sstring    User work state name
  TLV_WORKCOUNTRY = $02B2;  //   uint16     User work country code
  TLV_WORKZIPCODE = $02BC;  //   uint32     User work zip code
  TLV_WORKPHONE   = $02C6;  //   sstring    User work phone number
  TLV_WORKFAX     = $02D0;  //   sstring    User work fax number
  TLV_WORKURL     = $02DA;  //   sstring    User work webpage url
  TLV_TIMEZONE    = $0316;  //   uint8      User GMT offset
  TLV_ORGCITY     = $0320;  //   sstring    User originally from city
  TLV_ORGSTATE    = $032A;  //   sstring    User originally from state
  TLV_ORGCOUNTRY  = $0334;  //   uint16     User originally from country (code)

  ICQClientVer_Major = 1;       //Major version of component
  ICQClientVer_Minor = 30;      //Minor version of component
  ICQClientVer_Build = '';



  // Internal Constants
  ICQ_PROTOCOL_VER = $000B;
  CLIENT_MD5_STRING = 'AOL Instant Messenger (SM)';



const
  //Volkov Ioann added some constant client capabilities
  caps_cl_qip2005 = #$56#$3F#$C8#$9#$B#$6F#$41#$51#$49#$50#$20#$32#$30#$30#$35#$61;
  caps_cl_qip_infium = #$7C#$73#$75#$02#$C3#$BE#$4F#$3E#$A6#$9F#$01#$53#$13#$43#$1E#$1A;
  caps_cl_qip_pda_symbian = #$51#$AD#$D1#$90#$72#$04#$47#$3D#$A1#$A1#$49#$F4#$A3#$97#$A4#$1F;
  caps_cl_qip_pda_windows = #$56#$3F#$C8#$09#$0B#$6F#$41#$51#$49#$50#$20#$20#$20#$20#$20#$21;
  caps_cl_kopete = #$4B#$6F#$70#$65#$74#$65#$20#$49#$43#$51#$20#$20#$00#$0C#$00#$01;
  caps_cl_licq = #$09#$49#$13#$49#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_cl_jimm = #$4A#$69#$6D#$6D#$20#$30#$2E#$35#$2E#$31#$62#$00#$00#$00#$00#$00;
  caps_cl_trillian = #$F2#$E7#$C7#$F4#$FE#$AD#$4D#$FB#$B2#$35#$36#$79#$8B#$DF#$00#$00;
  caps_cl_simm = #$53#$49#$4D#$20#$63#$6C#$69#$65#$6E#$74#$20#$20#$00#$09#$04#$80;
  caps_cl_macicq = #$DD#$16#$F2#$02#$84#$E6#$11#$D4#$90#$DB#$00#$10#$4B#$9B#$4B#$7D;
  caps_cl_icq6 = #$01#$38#$CA#$7B#$76#$9A#$49#$15#$88#$F2#$13#$FC#$00#$97#$9E#$A8;
  caps_cl_miranda_im = #$4D#$69#$72#$61#$6E#$64#$61#$4D#$00#$07#$07#$00#$00#$03#$0A#$0D; //version 0.7.7.0, ICQ(0.3.10.13)
  caps_cl_miranda2_im = #$4D#$69#$72#$61#$6E#$64#$61#$4D#$22#$22#$22#$22#$33#$33#$33#$33; //version 34.34.34.34, ICQ(51.51.51.51)

  //Volkov Ioann added just some constant capabilities
  caps_xtraz = #$1A#$09#$3C#$6C#$D7#$FD#$4E#$C5#$9D#$51#$A6#$47#$4E#$34#$F5#$A0;
  caps_typing_notifications = #$56#$3F#$C8#$09#$0B#$6F#$41#$BD#$9F#$79#$42#$26#$09#$DF#$A2#$F3;
  caps_xtraz_multiuser_chat = #$67#$36#$15#$15#$61#$2D#$4C#$07#$8F#$3D#$BD#$E6#$40#$8E#$A0#$41;  //Xtraz Multiuser Chat
  caps_tzers = #$B2#$EC#$8F#$16#$7C#$6F#$45#$1B#$BD#$79#$DC#$58#$49#$78#$88#$B9;  //TZERS
  caps_utf8 = #$09#$46#$13#$4E#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;  //UTF8 Messages
  caps_avatar = #$09#$46#$00#$00#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_file_transfer = #$09#$46#$13#$4C#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;  //File Transfer
  caps_qip_protectmsg = #$D3#$D4#$53#$19#$8B#$32#$40#$3B#$AC#$C7#$D1#$A9#$E2#$B5#$81#$3E;  //QIP Protect Msg
  caps_rtf_messages = #$97#$B1#$27#$51#$24#$3C#$43#$34#$AD#$22#$D6#$AB#$F7#$3F#$14#$92;  //RTF Messages
  caps_lite = #$17#$8C#$2D#$9B#$DA#$A5#$45#$BB#$8D#$DB#$F3#$BD#$BD#$53#$A1#$0A;

  caps_aim_chat = #$74#$8F#$24#$20#$62#$87#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_interoperate = #$09#$46#$13#$4D#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_send_file = #$09#$46#$13#$43#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_get_file = #$09#$46#$13#$48#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_is_icq = #$09#$46#$13#$44#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_server_relay = #$09#$46#$13#$49#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_voice = #$09#$46#$13#$41#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_im_image = #$09#$46#$13#$45#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;
  caps_aim_buddy_icon = #$09#$46#$13#$46#$4C#$7F#$11#$D1#$82#$22#$44#$45#$53#$54#$00#$00;

  
  XStatusesCount = 35;
  CapXStatus: array [1..XStatusesCount] of String = (
    (#$01#$D8#$D7#$EE#$AC#$3B#$49#$2A#$A5#$8D#$D3#$D8#$77#$E6#$6B#$92),
    (#$5A#$58#$1E#$A1#$E5#$80#$43#$0C#$A0#$6F#$61#$22#$98#$B7#$E4#$C7),
    (#$83#$C9#$B7#$8E#$77#$E7#$43#$78#$B2#$C5#$FB#$6C#$FC#$C3#$5B#$EC),
    (#$E6#$01#$E4#$1C#$33#$73#$4B#$D1#$BC#$06#$81#$1D#$6C#$32#$3D#$81),
    (#$8C#$50#$DB#$AE#$81#$ED#$47#$86#$AC#$CA#$16#$CC#$32#$13#$C7#$B7),
    (#$3F#$B0#$BD#$36#$AF#$3B#$4A#$60#$9E#$EF#$CF#$19#$0F#$6A#$5A#$7F),
    (#$F8#$E8#$D7#$B2#$82#$C4#$41#$42#$90#$F8#$10#$C6#$CE#$0A#$89#$A6),
    (#$80#$53#$7D#$E2#$A4#$67#$4A#$76#$B3#$54#$6D#$FD#$07#$5F#$5E#$C6),
    (#$F1#$8A#$B5#$2E#$DC#$57#$49#$1D#$99#$DC#$64#$44#$50#$24#$57#$AF),
    (#$1B#$78#$AE#$31#$FA#$0B#$4D#$38#$93#$D1#$99#$7E#$EE#$AF#$B2#$18),
    (#$61#$BE#$E0#$DD#$8B#$DD#$47#$5D#$8D#$EE#$5F#$4B#$AA#$CF#$19#$A7),
    (#$48#$8E#$14#$89#$8A#$CA#$4A#$08#$82#$AA#$77#$CE#$7A#$16#$52#$08),
    (#$10#$7A#$9A#$18#$12#$32#$4D#$A4#$B6#$CD#$08#$79#$DB#$78#$0F#$09),
    (#$6F#$49#$30#$98#$4F#$7C#$4A#$FF#$A2#$76#$34#$A0#$3B#$CE#$AE#$A7),
    (#$12#$92#$E5#$50#$1B#$64#$4F#$66#$B2#$06#$B2#$9A#$F3#$78#$E4#$8D),
    (#$D4#$A6#$11#$D0#$8F#$01#$4E#$C0#$92#$23#$C5#$B6#$BE#$C6#$CC#$F0),
    (#$60#$9D#$52#$F8#$A2#$9A#$49#$A6#$B2#$A0#$25#$24#$C5#$E9#$D2#$60),
    (#$63#$62#$73#$37#$A0#$3F#$49#$FF#$80#$E5#$F7#$09#$CD#$E0#$A4#$EE),
    (#$1F#$7A#$40#$71#$BF#$3B#$4E#$60#$BC#$32#$4C#$57#$87#$B0#$4C#$F1),
    (#$78#$5E#$8C#$48#$40#$D3#$4C#$65#$88#$6F#$04#$CF#$3F#$3F#$43#$DF),
    (#$A6#$ED#$55#$7E#$6B#$F7#$44#$D4#$A5#$D4#$D2#$E7#$D9#$5C#$E8#$1F),
    (#$12#$D0#$7E#$3E#$F8#$85#$48#$9E#$8E#$97#$A7#$2A#$65#$51#$E5#$8D),
    (#$BA#$74#$DB#$3E#$9E#$24#$43#$4B#$87#$B6#$2F#$6B#$8D#$FE#$E5#$0F),
    (#$63#$4F#$6B#$D8#$AD#$D2#$4A#$A1#$AA#$B9#$11#$5B#$C2#$6D#$05#$A1),
    (#$2C#$E0#$E4#$E5#$7C#$64#$43#$70#$9C#$3A#$7A#$1C#$E8#$78#$A7#$DC),
    (#$10#$11#$17#$C9#$A3#$B0#$40#$F9#$81#$AC#$49#$E1#$59#$FB#$D5#$D4),
    (#$16#$0C#$60#$BB#$DD#$44#$43#$F3#$91#$40#$05#$0F#$00#$E6#$C0#$09),
    (#$64#$43#$C6#$AF#$22#$60#$45#$17#$B5#$8C#$D7#$DF#$8E#$29#$03#$52),
    (#$16#$F5#$B7#$6F#$A9#$D2#$40#$35#$8C#$C5#$C0#$84#$70#$3C#$98#$FA),
    (#$63#$14#$36#$ff#$3f#$8a#$40#$d0#$a5#$cb#$7b#$66#$e0#$51#$b3#$64),
    (#$b7#$08#$67#$f5#$38#$25#$43#$27#$a1#$ff#$cf#$4c#$c1#$93#$97#$97),
    (#$dd#$cf#$0e#$a9#$71#$95#$40#$48#$a9#$c6#$41#$32#$06#$d6#$f2#$80),
    (#$D4#$E2#$B0#$BA#$33#$4E#$4F#$A5#$98#$D0#$11#$7D#$BF#$4D#$3C#$C8),
    (#$CD#$56#$43#$A2#$C9#$4C#$47#$24#$B5#$2C#$DC#$01#$24#$A1#$D0#$CD),
    (#$00#$72#$D9#$08#$4A#$D1#$43#$DD#$91#$99#$6F#$02#$69#$66#$02#$6F)
  );

  nameXStatus: array[0..XStatusesCount-1] of String =(
    'Angry',
    'Taking a bath',
    'Tired',
    'Party',
    'Drinking beer',
    'Thinking',
    'Eating',
    'Watching TV',
    'Meeting',
    'Coffee',
    'Listening to music',
    'Business',
    'Shooting',
    'Having fun',
    'On the phone',
    'Gaming',
    'Studying',
    'Shopping',
    'Feeling sick',
    'Sleeping',
    'Surfing',
    'Browsing',
    'Working',
    'Typing',
    'Picnic',
    'Cooking',
    'Smoking',
    'Im high',
    'On WC',
    'To be or not to be',
    'Watching pro7 on TV',
    'Love',
    'RuSearch',
    'RuLove',
    'RuJournal'
  );

  // Message Capability IDs
  MCAP_GUIDs: array [0..2] of string = (
        (#$09#$46#$13#$49#$4c#$7f#$11#$d1#$82#$22#$44#$45#$53#$54#$00#$00),
        (#$09#$46#$13#$44#$4c#$7f#$11#$d1#$82#$22#$44#$45#$53#$54#$00#$00),
        (#$09#$46#$13#$43#$4c#$7f#$11#$d1#$82#$22#$44#$45#$53#$54#$00#$00)
  );

  // Plugin Type GUIDs
  PSIG_GUIDs: array [0..2] of string = (
     (#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00),
     (#$a0#$e9#$3f#$37#$4f#$e9#$d3#$11#$bc#$d2#$00#$04#$ac#$96#$dd#$96),
     (#$10#$cf#$40#$d1#$4f#$e9#$d3#$11#$bc#$d2#$00#$04#$ac#$96#$dd#$96)
  );

  // Message Plugin Type GUIDs
  MessageGUIDs: array [0..7] of string = (
    (#$BE#$6B#$73#$05#$0F#$C2#$10#$4F#$A6#$DE#$4D#$B1#$E3#$56#$4B#$0E),
    (#$81#$1a#$18#$bc#$0e#$6c#$18#$47#$a5#$91#$6f#$18#$dc#$c7#$6f#$1a),
    (#$F0#$2D#$12#$D9#$30#$91#$D3#$11#$8D#$D7#$00#$10#$4B#$06#$46#$2E),
    (#$37#$1C#$58#$72#$E9#$87#$D4#$11#$A4#$C1#$00#$D0#$B7#$59#$B1#$D9),
    (#$2A#$0E#$7D#$46#$76#$76#$D4#$11#$BC#$E6#$00#$04#$AC#$96#$1E#$A6),
    (#$01#$E5#$3B#$48#$2A#$E4#$D1#$11#$B6#$79#$00#$60#$97#$E1#$E2#$94),
    (#$BF#$F7#$20#$B2#$37#$8E#$D4#$11#$BD#$28#$00#$04#$AC#$96#$D9#$05),
    (#$3b#$60#$b3#$ef#$d8#$2a#$6c#$45#$a4#$e0#$9c#$5a#$5e#$67#$e8#$65)
  );

type
  //Volkov Ioann added type for NewXStatus
  T_NewXStatus = (X_NONE, X_ANGRY, X_DUCK, X_TIRED, X_PARTY, X_BEER, X_THINK,
  X_EAT, X_TV, X_FRIENDS, X_COFFEE, X_MUSIC, X_BUSINESS, X_CINEMA, X_FUNNY,
  X_PHONE, X_GAMES, X_COLLEGE, X_SHOP, X_SICK, X_SLEEP, X_SURFING, X_INTERNET,
  X_WORK, X_TYPING, X_UNK, X_PPC, X_MOBILE, X_MAN, X_WC, X_SMOKE, X_SEX,
  X_SEARCH, X_GEOMETRY, X_LOVE, X_RUSEARCH, X_RULOVE, X_RUJOURNAL);

const
  //Volkov Ioann added constants for NewXStatuses
  NewXStatus: array[0..36] of record
    dopstr : T_NewXStatus;
    dopxstr : String;
  end = (
  (dopstr: x_shop; dopxstr: 'icqmood0'),
  (dopstr: x_duck; dopxstr: 'icqmood1'),
  (dopstr: x_tired; dopxstr: 'icqmood2'),
  (dopstr: x_party; dopxstr: 'icqmood3'),
  (dopstr: x_beer; dopxstr: 'icqmood4'),
  (dopstr: x_think; dopxstr: 'icqmood5'),
  (dopstr: x_eat; dopxstr: 'icqmood6'),
  (dopstr: x_tv; dopxstr: 'icqmood7'),
  (dopstr: x_friends; dopxstr: 'icqmood8'),
  (dopstr: x_coffee; dopxstr: 'icqmood9'),
  (dopstr: x_music; dopxstr: 'icqmood10'),
  (dopstr: x_business; dopxstr: 'icqmood11'),
  (dopstr: x_cinema; dopxstr: 'icqmood12'),
  (dopstr: x_funny; dopxstr: 'icqmood13'),
  (dopstr: x_phone; dopxstr: 'icqmood14'),
  (dopstr: x_games; dopxstr: 'icqmood15'),
  (dopstr: x_college; dopxstr: 'icqmood16'),
  (dopstr: x_sick; dopxstr: 'icqmood17'),
  (dopstr: x_sleep; dopxstr: 'icqmood18'),
  (dopstr: x_surfing; dopxstr: 'icqmood19'),
  (dopstr: x_internet; dopxstr: 'icqmood20'),
  (dopstr: x_work; dopxstr: 'icqmood21'),
  (dopstr: x_typing; dopxstr: 'icqmood22'),
  (dopstr: x_angry; dopxstr: 'icqmood23'),
  //---- Here XStatuses in ICQ6 end ----
  (dopstr: x_unk; dopxstr: 'icqmood24'),
  (dopstr: x_ppc; dopxstr: 'icqmood25'),
  (dopstr: x_mobile; dopxstr: 'icqmood26'),
  (dopstr: x_sleep; dopxstr: 'icqmood27'),
  (dopstr: x_wc; dopxstr: 'icqmood28'),
  (dopstr: x_search; dopxstr: 'icqmood29'),
  (dopstr: x_geometry; dopxstr: 'icqmood30'),
  (dopstr: x_love; dopxstr: 'icqmood31'),
  (dopstr: x_rusearch; dopxstr: 'icqmood32'),
  (dopstr: x_rulove; dopxstr: 'icqmood33'),
  (dopstr: x_rujournal; dopxstr: 'icqmood34'),
  (dopstr: x_smoke; dopxstr: 'icqmood35'),
  (dopstr: x_sex; dopxstr: 'icqmood36'));

const
  // Message Capability IDs
  MCAP_TLV2711_FMT = 0;
  MCAP_REVERSE_REQ = 1;
  MCAP_OSCAR_FT = 2;

  // Plugin Type GUIDs
  PSIG_MESSAGE = 0;
  PSIG_INFO_PLUGIN = 1;
  PSIG_STATUS_PLUGIN = 2;
  
  // Message Plugin Type GUIDs
  MGTYPE_MESSAGE = 0;
  MGTYPE_STATUSMSGEXT  = 1;
  MGTYPE_FILE = 2;
  MGTYPE_WEBURL = 3;
  MGTYPE_CONTACTS = 4;
  MGTYPE_GREETING_CARD = 5;
  MGTYPE_CHAT = 6;
  MGTYPE_XTRAZ_SCRIPT = 7;  



type
  //File request record
  TFTRequestRec = record
    ReqType: Byte;
    ITime, IRandomID: LongWord;
    UIN: LongWord;
    Description: String;
    FileName: String;
    FileSize: LongWord;
    Seq: Word;
    Port: Word;
  end;

  //File start record
  TFTStartRec = record
    UIN: LongWord;
    FilesCount: LongWord;
    Current: Integer;
    Speed: LongWord;
  end;
  // File Send DC Record
  TSendFileRec = Record
    UIN:LongWord;
    Nick:String;
    Seq:Word;
    Files:TStrings;
    FilesCurrent:Integer;
    FilesCount:LongWord;
    FilePath:String;
    FileName:String;
    FileDescription:String;
    FileSize:LongWord;
    TotalSize:LongWord;
    Port:Word;
    Speed:Byte;
  End;
  pSendFileRec = ^TSendFileRec;

  // Port Range to support firewalls
  TPortRange = Record
    First:Word;
    Last: Word;
  End;

  //Avatar formats
  TAvatarFormat = (afPng, afGif, afJpeg, afBmp, afXml, afUnknown);

  //Error types
  TErrorType = (ERR_SOCKET, ERR_INTERNAL, ERR_WARNING, ERR_PROXY, ERR_PROTOCOL, ERR_CONNTIMEOUT, ERR_LOGIN, ERR_LOGIN_BAD_UINORPASSW);

  //Proxy types
  TProxyType = (P_NONE, P_SOCKS4, P_SOCKS5, P_HTTPS, P_HTTP);

  //Info types
  TInfoType = (INFO_GENERAL, INFO_MORE, INFO_ABOUT, INFO_PASSWORD);

  //Database types, used in ICQDb.pas
  TDbType = (DB_ICQ, DB_MIRANDA);

  //Flap header
  PFlapHdr = ^TFlapHdr;
  TFlapHdr = record
    Ident: Byte;
    ChID: Byte;
    Seq: Word;
    DataLen: Word;
  end;

  //Snac header
  PSnacHdr = ^TSnacHdr;
  TSnacHdr = record
    Family: Word;
    SubType: Word;
    Flags: Word;
    ReqID: LongWord;
  end;

  //Raw packet
  PRawPkt = ^TRawPkt;
  TRawPkt = record
    Data: array[0..MAX_DATA_LEN - 1] of Byte;
    Len: Word;
  end;

  TThreadTimerTrigger = class(TThread)
  protected
    fInterval:LongInt;
    fOnTrigger:TNotifyEvent;
    Procedure Trigger;
    procedure Execute; override;
  Public
    Owner:TObject;

    Property Interval:LongInt read fInterval write fInterval;
    Property OnTrigger:TNotifyEvent read fOnTrigger write fOnTrigger;
  end;

  TTHreadTimer = Class(TObject)
  private
    fActive:Boolean;
    fInterval:LongInt;
    fOnTimer:TNotifyEvent;
    fTrigger:TThreadTimerTrigger;

    Procedure SetActive(Const IsActive:Boolean);
    Procedure SetInterval(Const aInterval:LongInt);
    Procedure SetOnTimer(Const aOnTimer:TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;

    Property Enabled:Boolean read fActive write SetActive;
    Property Interval:LongInt read fInterval write SetInterval;
    Property OnTimer:TNotifyEvent read fOnTimer write SetOnTimer;
  end;

  //Filetransfer init call back function
  TOnFTInit = procedure(Sender: TObject; UIN: LongWord; FileCount, TotalBytes, Speed: LongWord; NickName: String) of object;
  TOnFTStart = procedure(Sender: TObject; StartRec: TFTStartRec; FileName: String; FileSize, Speed: LongWord) of object;
  TOnFTFileData = procedure(Sender: TObject; UIN: LongWord; Data: Pointer; DataLen: LongWord; LastPacket: Boolean) of object;

  TOnSendFileStart = Procedure(Sender: TObject; UIN: LongWord; SendFileRec: TSendFileRec) of Object;
  TOnSendFileData  = Procedure (Sender: TObject; UIN:LongWord; Data: Pointer;Var DataLen: LongWord; Var IsLastPacket: Boolean) of Object;
  TOnSendFileFinish= Procedure (Sender: TObject; UIN: LongWord; SendFileRec: TSendFileRec; Aborted:Boolean) of Object;

function IntToStr(Value: Int64): String;
function StrToInt(const Value: String): LongWord;
function IntToHex(Int: Int64; IntSize: Byte): String;
function HexToInt(Value: String): LongWord;
procedure ICQEncryptPass(SrcBuf: Pointer; BufLen: LongWord);
procedure ICQEncryptPassStr(var Pass: String);
function Swap16(Value: Word): Word;
function Swap32(Value: LongWord): LongWord;
function iif(ACondition: Boolean; AIfTrue, AIfFalse: String): String;

{Low-level functions}
{-- Adding data --}
procedure PktAddData(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
procedure PktAddArrBuf(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
procedure PktInit(Pkt: PRawPkt; Channel: Byte; var Seq: Word);
procedure PktInitRaw(Pkt: PRawPkt);
procedure PktFinal(Pkt: PRawPkt);
procedure PktSnac(Pkt: PRawPkt; Family, SubType: Word; ID: LongWord; Flags: Word);
procedure PktInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
procedure PktLInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
procedure PktStr(Pkt: PRawPkt; const S: String);
procedure PktLStr(Pkt: PRawPkt; const S: String); overload;
procedure PktLStr(Pkt: PRawPkt; S: LongWord); overload;
procedure PktWStr(Pkt: PRawPkt; const S: String);
procedure PktDWStr(Pkt: PRawPkt; const S: String);
procedure PktLNTS(Pkt: PRawPkt; const S: String);
procedure PktLLNTS(Pkt: PRawPkt; const S: String);
procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: LongWord); overload;
procedure PktTLV(Pkt: PRawPkt; T: Word; const V: String); overload;
procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: Pointer); overload;
procedure PktLTLV(Pkt: PRawPkt; T: Word; const V: String); overload;
procedure PktLTLV(Pkt: PRawPkt; T,L: Word; V: LongWord); overload;
procedure PktLBTLV(Pkt: PRawPkt; T: Word; V: String; B: Byte); overload;

{-- Extracting data --}
function GetInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
function GetLInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
function GetStr(Pkt: PRawPkt; StrLen: Integer): String;
function GetTLVStr(Pkt: PRawPkt; var T: Word): String;
function GetTLVInt(Pkt: PRawPkt; var T: Word): LongWord;
procedure GetSnac(Pkt: PRawPkt; var Snac: TSnacHdr);
function GetLStr(Pkt: PRawPkt): String;
function GetWStr(Pkt: PRawPkt): String;
function GetDWStr(Pkt: PRawPkt): String;
function GetLNTS(Pkt: PRawPkt): String;


{High-level functions}
function StrToLanguageI(const Value: String): Word;
function StrToCountryI(const Value: String): Word;
function StrToInterestI(const Value: String): Word;
function StrToOccupationI(const Value: String): Word;
function StrToPastI(const Value: String): Word;
function StrToOrganizationI(const Value: String): Word;
procedure ParseContacts(Value: String; var List: TStringList);
function MakeContactsStr(Contacts: TStringList): String;

{Packet creation functions}

procedure CreateCLI_SSL_ACTIVATE(Pkt: PRawPkt; var Seq, Seq2: Word); {CLI_ROSTERACK}

procedure CreateCLI_IDENT(Pkt: PRawPkt; UIN: LongWord; Password: String; Secure: Boolean; var Seq: Word);
procedure CreateCLI_COOKIE(Pkt: PRawPkt; const Cookie: String; var Seq: Word);
procedure CreateCLI_FAMILIES(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_RATESREQUEST(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_ACKRATES(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQINFO(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQLISTS(Pkt: PRawPkt; var Seq: Word);  //added by Volkov Ioann
procedure CreateCLI_REQROSTER(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_CHECKROSTER(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQLOCATION(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQBUDDY(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQICBM(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SETSTATUS(Pkt: PRawPkt; Status: LongWord; IP: LongInt; Port: Word; Cookie: LongWord; ProxyType: TProxyType; var Seq: Word);
procedure CreateCLI_SETSTATUS_SHORT(Pkt: PRawPkt; Status: LongWord; var Seq: Word);
procedure CreateCLI_REQBOS(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SETUSERINFO(Pkt: PRawPkt; XStatNum : Byte; var Seq: Word);

procedure CreateCLI_SETNEWXSTATUS(Pkt: PRawPkt; XStat: T_NewXStatus; XStatStr: String; var Seq: Word);
procedure CreateCLI_SETNEWXSTATUS_icqmood(Pkt: PRawPkt; MoodNum: Byte; XStatStr: String; var Seq: Word);

procedure CreateCLI_SETICBM(Pkt: PRawPkt;IsMTNEnabled: Boolean; AChannel: Byte; var Seq: Word);
procedure CreateCLI_SETIDLETIME(Pkt: PRawPkt; const IsIdle: Boolean; var Seq: Word);
procedure CreateCLI_READY(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_EXT_SERV_READY(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SERVICExREQ(Pkt: PRawPkt; var Seq: Word); //ok3y
procedure CreateCLI_REQ_AVATAR(Pkt: PRawPkt; UIN: String; var FSeq: Word; AvatarId: Word; AvatarFlags: Byte; AvatarHash: String);
procedure CreateCLI_TOICQSRV(Pkt: PRawPkt; UIN: LongWord; Command: Word; Data: Pointer; DataLen: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_ADDCONTACT(Pkt: PRawPkt; UIN: String; var Seq: Word);
procedure CreateCLI_ADDCONTACT_MULTI(Pkt: PRawPkt; UINs: array of LongWord; var Seq: Word);
procedure CreateCLI_REMOVECONTACT(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
procedure CreateCLI_ADDVISIBLE(Pkt: PRawPkt; UINs: TStrings; var Seq: Word);
procedure CreateCLI_REMVISIBLE(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
procedure CreateCLI_ADDINVISIBLE(Pkt: PRawPkt; UINs: TStrings; var Seq: Word);
procedure CreateCLI_REMINVISIBLE(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
procedure CreateCLI_ACKOFFLINEMSGS(Pkt: PRawPkt; UIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_SENDMSG(Pkt: PRawPkt; ITime, IRandom, UIN: LongWord; const Msg: String; var Seq: Word);
procedure CreateCLI_SENDURL(Pkt: PRawPkt; ITime, IRandom, MyUIN, UIN: LongWord; const URL, Description: String; var Seq: Word);
procedure CreateCLI_AUTHORIZE(Pkt: PRawPkt; UIN: LongWord; Auth: Byte; Reason: String; var Seq: Word);
procedure CreateCLI_METAREQINFO(Pkt: PRawPkt; UIN, dUIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHBYMAIL(Pkt: PRawPkt; UIN: LongWord; const Email: String; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHBYUIN(Pkt: PRawPkt; UIN: LongWord; DUIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHBYNAME(Pkt: PRawPkt; UIN: LongWord; const FirstName, LastName, NickName, Email: String; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHRANDOM(Pkt: PRawPkt; UIN: LongWord; Group: Word; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHWP(Pkt: PRawPkt; UIN: LongWord; First, Last, Nick, Email: String; MinAge, MaxAge: Word; Gender: Byte; Language: Byte; City: String; Country: Word; Company, Department, Position: String; Occupation: Byte; Organization: Word; OrganKeyWords: String; Affiliation: Word; AffiKeyWords, KeyWord: String; Online: Byte; var Seq, Seq2: Word);

procedure CreateCLI_SETSELFINFO(Pkt: PRawPkt; UIN: LongWord;
  NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip: String;
  Country: Word; TimeZone: Byte; PublishEmail: Boolean;
  Age: Word; Gender: Byte; const HomePage: String; BirthYear: Word; BirthMonth, BirthDay: Byte;
  Language1, Language2, Language3: String; const About: String;
  AuthorizationRequired, WebAware: Boolean;
  var Seq, Seq2: Word);

procedure CreateCLI_METASETMORE(Pkt: PRawPkt; UIN: LongWord; Age: Word; Gender: Byte; HomePage: String; BirthYear: Word; BirthMonth, BirthDay, Lang1, Lang2, Lang3: Byte; var Seq, Seq2: Word);

procedure CreateCLI_METASETGENERAL(Pkt: PRawPkt; UIN: LongWord; const NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip: String; Country: Word; TimeZone: Byte; PublishEmail: Boolean; var Seq, Seq2: Word);

procedure CreateCLI_METASETABOUT(Pkt: PRawPkt; UIN: LongWord; const About: String; var Seq, Seq2: Word);
procedure CreateCLI_SENDSMS(Pkt: PRawPkt; UIN: LongWord; const Destination, Text: String; CodePage: Word; const Time: String; var Seq, Seq2: Word);
procedure CreateCLI_SENDMSG_ADVANCED(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; const Msg: String; RTFFormat: Boolean; var Seq: Word);
procedure CreateCLI_SENDMSG_FILEACK(Pkt: PRawPkt; Status: LongWord; FFSeq: Word; ITime, IRandom, UIN, FileSize: LongWord; const FileDesc, FileName: String; Port: Word; var Seq: Word);
procedure CreateCLI_SENDMSG_FILEDECLINE(Pkt: PRawPkt; FFSeq: Word; ITime, IRandom, UIN, FileSize: LongWord; const FileDesc, FileName, Reason: String; Port: Word; var Seq: Word);
procedure CreateCLI_HELLO(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_GOODBYE(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REGISTERUSER(Pkt: PRawPkt; const Password: String; var Seq: Word);
procedure CreateCLI_REQAWAYMSG(Pkt: PRawPkt; FStatus: LongWord; ITime, IRandom, UIN: LongWord; Status: Byte; var Seq: Word);
procedure CreateCLI_SENDCONTACTS(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; Contacts: TStringList; var Seq: Word);
procedure CreateCLI_SENDCONTACTS_REQ(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; Reason: String; var Seq: Word);
procedure CreateCLI_UNREGUIN(Pkt: PRawPkt; UIN: LongWord; const Password: String; var Seq, Seq2: Word);
procedure CreateCLI_METASETPASS(Pkt: PRawPkt; UIN: LongWord; const Password: String; Buffer: Pointer; BufLen: Word; var Seq, Seq2: Word);
procedure CreateCLI_METASETPERMISSIONS(Pkt: PRawPkt; UIN: LongWord; AuthorizationRequired, WebAware: Boolean; var Seq, Seq2: Word);
procedure CreateCLI_METAREQINFO_SHORT(Pkt: PRawPkt; UIN, DestUIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_REQAUTH(Pkt: PRawPkt; UIN: LongWord; Msg: String; var Seq: Word);
procedure CreateCLI_KEEPALIVE(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_ADDSTART(Pkt: PRawPkt; FirstUpload: Boolean; var Seq: Word);
procedure CreateCLI_ADDEND(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_UPDATEGROUP(Pkt: PRawPkt; Name: String; Tag: Word; IDs: TStringList; var Seq: Word);
procedure CreateCLI_UPDATEBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized: Boolean; var Seq: Word);
procedure CreateCLI_ADDBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized: Boolean; var Seq: Word);
procedure CreateCLI_DELETEBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized, IsGroup: Boolean; var Seq: Word);
procedure CreateCLI_SEND_YOU_WERE_ADDED(Pkt: PRawPkt; ITime, IRandom, UIN, FromUIN: LongWord; var Seq: Word);
procedure CreateCLI_TYPING_NOTIFICATION(Pkt: PRawPkt; UIN: String;
  NotificationType: Word; var Seq: Word); //ok3y
procedure CreateCLI_MD5AUTHREQ(Pkt: PRawPkt; UIN: String; var Seq: Word); //ok3y

{Misc functions}
function SnacToStr(Family, SubType: Word): String;
function SrvMetaToStr(V1, V2: Word): String;
function PeerCmdToStr(Cmd: Byte): String;
function DumpPacket(Buffer: Pointer; BufLen: Word): String;
function UnicodeCharCode2ANSIString (aCode: Word): String; // yegor
function RTF2Plain (const aSource: string): string; // yegor
function StatusToInt(Value: LongWord): LongWord;   // added by eraser 30.6.03
function StatusToStr(Value: LongWord): String;     // changed by eraser 30.6.03
function StatusToStr_rus(Value: LongWord): String; //added by Volkov Ioann
function CountryToStr(Value: Word): String;
function LanguageToStr(Value: Byte): String;
function OccupationToStr(Value: Word): String;
function InterestToStr(Value: Word): String;
function PastToStr(Value: Word): String;
function AffiliationToStr(Value: Word): String;
function LoadPacketRaw(Pkt: PRawPkt; const FName: String): Boolean;
function LoadPacket(Pkt: PRawPkt; const FName: String; var Flap: TFlapHdr; var Snac: TSnacHdr): Boolean;
function FileExists(const FileName: String): Boolean;
function FileSize(const FName: String): LongWord;
procedure LogText(const FName, Text: String);
procedure ShowMessage(const Value: String); overload;
procedure ShowMessage(Value: LongInt); overload;
function ExtractName(const Value: String): String;
function ExtractValue(const Value: String): String;
function UTF8ToStr(Value: String): String;
function UTF8ToStrSmart(Value: String): String;
function MyUTF8Decode(Value : String) : String;
function UCS2BEToStr (Value: string): string; // yegor
function IsStringProbablyUCS2BE (aString: string): Boolean; // yegor
function StrToHexStr(AStr: String): String;
function HexStrToStr(AStr: String): String;

function GetXMLEntry(const Tag, Msg: String): String;

{SMS functions}
function StrToUTF8(Value: String): String;
function STime: String;
function GetSMSTime: String;

function DecryptPak(Pak: Pointer; Size: LongWord; Ver: Byte): Boolean;
procedure EncryptPak(Pak: Pointer; Size: LongWord; Ver: Byte);

{Peer packet functions}
procedure CreatePEER_INIT(Pkt: PRawPkt; Cookie, DestUIN, SrcUIN, SrcPort, SrcIPExt, SrcIPInt: LongWord; ProxyType: TProxyType);
procedure CreatePEER_INIT2(Pkt: PRawPkt; Ack: Boolean);
procedure CreatePEER_ACK(Pkt: PRawPkt);
function CreatePEER_MSG(Pkt: PRawPkt; const Msg: String; RTFFormat: Boolean; var Seq: Word): Word;
procedure CreatePEER_MSGACK(Pkt: PRawPkt; Seq: Word);
function CreatePEER_MSG_FILE(Pkt: PRawPkt; FileSendRec:TSendFileRec; var Seq: Word): Word;
procedure CreatePEER_AUTOMSG_ACK(Pkt: PRawPkt; Answer: String; Status, Seq: Word);
function CreatePEER_CONTACTS(Pkt: PRawPkt; Contacts: TStringList; var Seq: Word): Word;
function CreatePEER_CONTACTREQ(Pkt: PRawPkt; const Reason: String; var Seq: Word): Word;
function CreatePEER_FILEINIT(Pkt: PRawPkt; Response: Boolean; FileDescription, FileName: String; Port: Word; FileLength: LongWord; var Seq: Word; Reason: String; Accept: Boolean): Word;
procedure CreatePEER_FILEINITACK(Pkt: PRawPkt; Speed: LongWord; Nick: String);
procedure CreatePEER_FILE_INIT(Pkt: PRawPkt; Count, Bytes, Speed: LongWord; Nick:String);
procedure CreatePEER_FILE_INIT2(Pkt: PRawPkt; Count, Bytes, Speed: LongWord);
procedure CreatePEER_FILE_START(Pkt: PRawPkt; FileName:String; FileSize, Speed:LongWord);
Procedure CreatePEER_FILE_DATA(Pkt: PRawPkt; Buffer:Pointer; BufLen:Integer);


function EncodeBase64(Value: String): String;
function DecodeBase64(Value: String): String;
function CreateHTTP_Header(const Method, URL, Host: String; DataLen: LongWord; Auth: Boolean; User, Password: String): String;
procedure CreateHTTP_DATA_HDR(Pkt: PRawPkt; PType: Word; DataLen: LongWord);
procedure CreateHTTP_DATA(Pkt: PRawPkt; PType: Word; Data: Pointer; DataLen: LongWord);
function CreateHTTP_INIT(Auth: Boolean; User, Password: String): String;
function CreateHTTP_RECV(Host, SID: String; Auth: Boolean; User, Password: String): String;
procedure CreateHTTP_LOGIN(Pkt: PRawPkt; Host: String; Port: Word);


function Decrypt99bPassword(UIN, CryptIV: LongWord; const HexPass: String): String;
function DecryptMirandaPassword(const Value: String): String;

function UnixDateTimeToDateTime (aUnixTime: LongInt): TDateTime;

//Text constants
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//------------------------------------------------------------------------------------------------------------\
const
  //Avatar exts
  AvatarExts : array [TAvatarFormat] of string = ('.png', '.gif' , '.jpg', '.bmp', '.xml', '');

  Countries: array[0..242] of record Ident: Word; Value: String end =
    ((Ident: 1; Value: 'USA'),
    (Ident: 7; Value: 'Russia'),
    (Ident: 20; Value: 'Egypt'),
    (Ident: 27; Value: 'South Africa'),
    (Ident: 30; Value: 'Greece'),
    (Ident: 31; Value: 'Netherlands'),
    (Ident: 32; Value: 'Belgium'),
    (Ident: 33; Value: 'France'),
    (Ident: 34; Value: 'Spain'),
    (Ident: 36; Value: 'Hungary'),
    (Ident: 39; Value: 'Italy'),
    (Ident: 40; Value: 'Romania'),
    (Ident: 41; Value: 'Switzerland'),
    (Ident: 42; Value: 'Czech Republic'),
    (Ident: 43; Value: 'Austria'),
    (Ident: 44; Value: 'United Kingdom'),
    (Ident: 45; Value: 'Denmark'),
    (Ident: 46; Value: 'Sweden'),
    (Ident: 47; Value: 'Norway'),
    (Ident: 48; Value: 'Poland'),
    (Ident: 49; Value: 'Germany'),
    (Ident: 51; Value: 'Peru'),
    (Ident: 52; Value: 'Mexico'),
    (Ident: 53; Value: 'Cuba'),
    (Ident: 54; Value: 'Argentina'),
    (Ident: 55; Value: 'Brazil'),
    (Ident: 56; Value: 'Chile'),
    (Ident: 57; Value: 'Colombia'),
    (Ident: 58; Value: 'Venezuela'),
    (Ident: 60; Value: 'Malaysia'),
    (Ident: 61; Value: 'Australia'),
    (Ident: 62; Value: 'Indonesia'),
    (Ident: 63; Value: 'Philippines'),
    (Ident: 64; Value: 'New Zealand'),
    (Ident: 65; Value: 'Singapore'),
    (Ident: 66; Value: 'Thailand'),
    (Ident: 81; Value: 'Japan'),
    (Ident: 82; Value: 'Korea (Republic of)'),
    (Ident: 84; Value: 'Vietnam'),
    (Ident: 86; Value: 'China'),
    (Ident: 90; Value: 'Turkey'),
    (Ident: 91; Value: 'India'),
    (Ident: 92; Value: 'Pakistan'),
    (Ident: 93; Value: 'Afghanistan'),
    (Ident: 94; Value: 'Sri Lanka'),
    (Ident: 95; Value: 'Myanmar'),
    (Ident: 98; Value: 'Iran'),
    (Ident: 101; Value: 'Anguilla'),
    (Ident: 102; Value: 'Antigua'),
    (Ident: 103; Value: 'Bahamas'),
    (Ident: 104; Value: 'Barbados'),
    (Ident: 105; Value: 'Bermuda'),
    (Ident: 106; Value: 'British Virgin Islands'),
    (Ident: 107; Value: 'Canada'),
    (Ident: 108; Value: 'Cayman Islands'),
    (Ident: 109; Value: 'Dominica'),
    (Ident: 110; Value: 'Dominican Republic'),
    (Ident: 111; Value: 'Grenada'),
    (Ident: 112; Value: 'Jamaica'),
    (Ident: 113; Value: 'Montserrat'),
    (Ident: 114; Value: 'Nevis'),
    (Ident: 115; Value: 'St. Kitts'),
    (Ident: 116; Value: 'St. Vincent and the Grenadines'),
    (Ident: 117; Value: 'Trinidad and Tobago'),
    (Ident: 118; Value: 'Turks and Caicos Islands'),
    (Ident: 120; Value: 'Barbuda'),
    (Ident: 121; Value: 'Puerto Rico'),
    (Ident: 122; Value: 'Saint Lucia'),
    (Ident: 123; Value: 'United States Virgin Islands'),
    (Ident: 212; Value: 'Morocco'),
    (Ident: 213; Value: 'Algeria'),
    (Ident: 216; Value: 'Tunisia'),
    (Ident: 218; Value: 'Libya'),
    (Ident: 220; Value: 'Gambia'),
    (Ident: 221; Value: 'Senegal Republic'),
    (Ident: 222; Value: 'Mauritania'),
    (Ident: 223; Value: 'Mali'),
    (Ident: 224; Value: 'Guinea'),
    (Ident: 225; Value: 'Ivory Coast'),
    (Ident: 226; Value: 'Burkina Faso'),
    (Ident: 227; Value: 'Niger'),
    (Ident: 228; Value: 'Togo'),
    (Ident: 229; Value: 'Benin'),
    (Ident: 230; Value: 'Mauritius'),
    (Ident: 231; Value: 'Liberia'),
    (Ident: 232; Value: 'Sierra Leone'),
    (Ident: 233; Value: 'Ghana'),
    (Ident: 234; Value: 'Nigeria'),
    (Ident: 235; Value: 'Chad'),
    (Ident: 236; Value: 'Central African Republic'),
    (Ident: 237; Value: 'Cameroon'),
    (Ident: 238; Value: 'Cape Verde Islands'),
    (Ident: 239; Value: 'Sao Tome and Principe'),
    (Ident: 240; Value: 'Equatorial Guinea'),
    (Ident: 241; Value: 'Gabon'),
    (Ident: 242; Value: 'Congo'),
    (Ident: 243; Value: 'Dem. Rep. of the Congo'),
    (Ident: 244; Value: 'Angola'),
    (Ident: 245; Value: 'Guinea-Bissau'),
    (Ident: 246; Value: 'Diego Garcia'),
    (Ident: 247; Value: 'Ascension Island'),
    (Ident: 248; Value: 'Seychelle Islands'),
    (Ident: 249; Value: 'Sudan'),
    (Ident: 250; Value: 'Rwanda'),
    (Ident: 251; Value: 'Ethiopia'),
    (Ident: 252; Value: 'Somalia'),
    (Ident: 253; Value: 'Djibouti'),
    (Ident: 254; Value: 'Kenya'),
    (Ident: 255; Value: 'Tanzania'),
    (Ident: 256; Value: 'Uganda'),
    (Ident: 257; Value: 'Burundi'),
    (Ident: 258; Value: 'Mozambique'),
    (Ident: 260; Value: 'Zambia'),
    (Ident: 261; Value: 'Madagascar'),
    (Ident: 262; Value: 'Reunion Island'),
    (Ident: 263; Value: 'Zimbabwe'),
    (Ident: 264; Value: 'Namibia'),
    (Ident: 265; Value: 'Malawi'),
    (Ident: 266; Value: 'Lesotho'),
    (Ident: 267; Value: 'Botswana'),
    (Ident: 268; Value: 'Swaziland'),
    (Ident: 269; Value: 'Mayotte Island'),
    (Ident: 290; Value: 'St. Helena'),
    (Ident: 291; Value: 'Eritrea'),
    (Ident: 297; Value: 'Aruba'),
    (Ident: 298; Value: 'Faeroe Islands'),
    (Ident: 299; Value: 'Greenland'),
    (Ident: 350; Value: 'Gibraltar'),
    (Ident: 351; Value: 'Portugal'),
    (Ident: 352; Value: 'Luxembourg'),
    (Ident: 353; Value: 'Ireland'),
    (Ident: 354; Value: 'Iceland'),
    (Ident: 355; Value: 'Albania'),
    (Ident: 356; Value: 'Malta'),
    (Ident: 357; Value: 'Cyprus'),
    (Ident: 358; Value: 'Finland'),
    (Ident: 359; Value: 'Bulgaria'),
    (Ident: 370; Value: 'Lithuania'),
    (Ident: 371; Value: 'Latvia'),
    (Ident: 372; Value: 'Estonia'),
    (Ident: 373; Value: 'Moldova'),
    (Ident: 374; Value: 'Armenia'),
    (Ident: 375; Value: 'Belarus'),
    (Ident: 376; Value: 'Andorra'),
    (Ident: 377; Value: 'Monaco'),
    (Ident: 378; Value: 'San Marino'),
    (Ident: 379; Value: 'Vatican City'),
    (Ident: 380; Value: 'Ukraine'),
    (Ident: 381; Value: 'Yugoslavia'),
    (Ident: 385; Value: 'Croatia'),
    (Ident: 386; Value: 'Slovenia'),
    (Ident: 387; Value: 'Bosnia and Herzegovina'),
    (Ident: 389; Value: 'F.Y.R.O.M. (Former Yugoslav Republic of Macedonia)'),
    (Ident: 500; Value: 'Falkland Islands'),
    (Ident: 501; Value: 'Belize'),
    (Ident: 502; Value: 'Guatemala'),
    (Ident: 503; Value: 'El Salvador'),
    (Ident: 504; Value: 'Honduras'),
    (Ident: 505; Value: 'Nicaragua'),
    (Ident: 506; Value: 'Costa Rica'),
    (Ident: 507; Value: 'Panama'),
    (Ident: 508; Value: 'St. Pierre and Miquelon'),
    (Ident: 509; Value: 'Haiti'),
    (Ident: 590; Value: 'Guadeloupe'),
    (Ident: 591; Value: 'Bolivia'),
    (Ident: 592; Value: 'Guyana'),
    (Ident: 593; Value: 'Ecuador'),
    (Ident: 594; Value: 'French Guiana'),
    (Ident: 595; Value: 'Paraguay'),
    (Ident: 596; Value: 'Martinique'),
    (Ident: 597; Value: 'Suriname'),
    (Ident: 598; Value: 'Uruguay'),
    (Ident: 599; Value: 'Netherlands Antilles'),
    (Ident: 670; Value: 'Saipan Island'),
    (Ident: 671; Value: 'Guam'),
    (Ident: 672; Value: 'Christmas Island'),
    (Ident: 673; Value: 'Brunei'),
    (Ident: 674; Value: 'Nauru'),
    (Ident: 675; Value: 'Papua New Guinea'),
    (Ident: 676; Value: 'Tonga'),
    (Ident: 677; Value: 'Solomon Islands'),
    (Ident: 678; Value: 'Vanuatu'),
    (Ident: 679; Value: 'Fiji Islands'),
    (Ident: 680; Value: 'Palau'),
    (Ident: 681; Value: 'Wallis and Futuna Islands'),
    (Ident: 682; Value: 'Cook Islands'),
    (Ident: 683; Value: 'Niue'),
    (Ident: 684; Value: 'American Samoa'),
    (Ident: 685; Value: 'Western Samoa'),
    (Ident: 686; Value: 'Kiribati Republic'),
    (Ident: 687; Value: 'New Caledonia'),
    (Ident: 688; Value: 'Tuvalu'),
    (Ident: 689; Value: 'French Polynesia'),
    (Ident: 690; Value: 'Tokelau'),
    (Ident: 691; Value: 'Micronesia, Federated States of'),
    (Ident: 692; Value: 'Marshall Islands'),
    (Ident: 705; Value: 'Kazakhstan'),
    (Ident: 706; Value: 'Kyrgyz Republic'),
    (Ident: 708; Value: 'Tajikistan'),
    (Ident: 709; Value: 'Turkmenistan'),
    (Ident: 711; Value: 'Uzbekistan'),
    (Ident: 800; Value: 'International Freephone Service'),
    (Ident: 850; Value: 'Korea (North)'),
    (Ident: 852; Value: 'Hong Kong'),
    (Ident: 853; Value: 'Macau'),
    (Ident: 855; Value: 'Cambodia'),
    (Ident: 856; Value: 'Laos'),
    (Ident: 870; Value: 'INMARSAT'),
    (Ident: 871; Value: 'INMARSAT (Atlantic-East)'),
    (Ident: 872; Value: 'INMARSAT (Pacific)'),
    (Ident: 873; Value: 'INMARSAT (Indian)'),
    (Ident: 874; Value: 'INMARSAT (Atlantic-West)'),
    (Ident: 880; Value: 'Bangladesh'),
    (Ident: 886; Value: 'Taiwan, Republic of China'),
    (Ident: 960; Value: 'Maldives'),
    (Ident: 961; Value: 'Lebanon'),
    (Ident: 962; Value: 'Jordan'),
    (Ident: 963; Value: 'Syria'),
    (Ident: 964; Value: 'Iraq'),
    (Ident: 965; Value: 'Kuwait'),
    (Ident: 966; Value: 'Saudi Arabia'),
    (Ident: 967; Value: 'Yemen'),
    (Ident: 968; Value: 'Oman'),
    (Ident: 971; Value: 'United Arab Emirates'),
    (Ident: 972; Value: 'Israel'),
    (Ident: 973; Value: 'Bahrain'),
    (Ident: 974; Value: 'Qatar'),
    (Ident: 975; Value: 'Bhutan'),
    (Ident: 976; Value: 'Mongolia'),
    (Ident: 977; Value: 'Nepal'),
    (Ident: 994; Value: 'Azerbaijan'),
    (Ident: 995; Value: 'Georgia'),
    (Ident: 2691; Value: 'Comoros'),
    (Ident: 4101; Value: 'Liechtenstein'),
    (Ident: 4201; Value: 'Slovak Republic'),
    (Ident: 5399; Value: 'Guantanamo Bay'),
    (Ident: 5901; Value: 'French Antilles'),
    (Ident: 6101; Value: 'Cocos-Keeling Islands'),
    (Ident: 6701; Value: 'Rota Island'),
    (Ident: 6702; Value: 'Tinian Island'),
    (Ident: 6721; Value: 'Australian Antarctic Territory'),
    (Ident: 6722; Value: 'Norfolk Island'),
    (Ident: 9999; Value: 'Unknown'));

  Languages: array[0..72] of record Ident: Byte; Value: String end =
    ((Ident: 1; Value: 'Arabic'),
    (Ident: 2; Value: 'Bhojpuri'),
    (Ident: 3; Value: 'Bulgarian'),
    (Ident: 4; Value: 'Burmese'),
    (Ident: 5; Value: 'Cantonese'),
    (Ident: 6; Value: 'Catalan'),
    (Ident: 7; Value: 'Chinese'),
    (Ident: 8; Value: 'Croatian'),
    (Ident: 9; Value: 'Czech'),
    (Ident: 10; Value: 'Danish'),
    (Ident: 11; Value: 'Dutch'),
    (Ident: 12; Value: 'English'),
    (Ident: 13; Value: 'Esperanto'),
    (Ident: 14; Value: 'Estonian'),
    (Ident: 15; Value: 'Farci'),
    (Ident: 16; Value: 'Finnish'),
    (Ident: 17; Value: 'French'),
    (Ident: 18; Value: 'Gaelic'),
    (Ident: 19; Value: 'German'),
    (Ident: 20; Value: 'Greek'),
    (Ident: 21; Value: 'Hebrew'),
    (Ident: 22; Value: 'Hindi'),
    (Ident: 23; Value: 'Hungarian'),
    (Ident: 24; Value: 'Icelandic'),
    (Ident: 25; Value: 'Indonesian'),
    (Ident: 26; Value: 'Italian'),
    (Ident: 27; Value: 'Japanese'),
    (Ident: 28; Value: 'Khmer'),
    (Ident: 29; Value: 'Korean'),
    (Ident: 30; Value: 'Lao'),
    (Ident: 31; Value: 'Latvian'),
    (Ident: 32; Value: 'Lithuanian'),
    (Ident: 33; Value: 'Malay'),
    (Ident: 34; Value: 'Norwegian'),
    (Ident: 35; Value: 'Polish'),
    (Ident: 36; Value: 'Portuguese'),
    (Ident: 37; Value: 'Romanian'),
    (Ident: 38; Value: 'Russian'),
    (Ident: 39; Value: 'Serbo-Croatian'),
    (Ident: 40; Value: 'Slovak'),
    (Ident: 41; Value: 'Slovenian'),
    (Ident: 42; Value: 'Somali'),
    (Ident: 43; Value: 'Spanish'),
    (Ident: 44; Value: 'Swahili'),
    (Ident: 45; Value: 'Swedish'),
    (Ident: 46; Value: 'Tagalog'),
    (Ident: 47; Value: 'Tatar'),
    (Ident: 48; Value: 'Thai'),
    (Ident: 49; Value: 'Turkish'),
    (Ident: 50; Value: 'Ukrainian'),
    (Ident: 51; Value: 'Urdu'),
    (Ident: 52; Value: 'Vietnamese'),
    (Ident: 53; Value: 'Yiddish'),
    (Ident: 54; Value: 'Yoruba'),
    (Ident: 55; Value: 'Afrikaans'),
    (Ident: 56; Value: 'Bosnian'),
    (Ident: 57; Value: 'Persian'),
    (Ident: 58; Value: 'Albanian'),
    (Ident: 59; Value: 'Armenian'),
    (Ident: 60; Value: 'Punjabi'),
    (Ident: 61; Value: 'Chamorro'),
    (Ident: 62; Value: 'Mongolian'),
    (Ident: 63; Value: 'Mandarin'),
    (Ident: 64; Value: 'Taiwanese'),
    (Ident: 65; Value: 'Macedonian'),
    (Ident: 66; Value: 'Sindhi'),
    (Ident: 67; Value: 'Welsh'),
    (Ident: 68; Value: 'Azerbaijani'),
    (Ident: 69; Value: 'Kurdish'),
    (Ident: 70; Value: 'Gujarati'),
    (Ident: 71; Value: 'Tamil'),
    (Ident: 72; Value: 'Belorussian'),
    (Ident: 255; Value: 'Unknown'));

  Occupations: array[1..17] of record Ident: Byte; Value: String end =
    ((Ident: 1; Value: 'Academic'),
    (Ident: 2; Value: 'Administrative'),
    (Ident: 3; Value: 'Art/Entertainment'),
    (Ident: 4; Value: 'College Student'),
    (Ident: 5; Value: 'Computers'),
    (Ident: 6; Value: 'Community & Social'),
    (Ident: 7; Value: 'Education'),
    (Ident: 8; Value: 'Engineering'),
    (Ident: 9; Value: 'Financial Services'),
    (Ident: 10; Value: 'Government'),
    (Ident: 11; Value: 'High School Student'),
    (Ident: 12; Value: 'Home'),
    (Ident: 13; Value: 'ICQ - Providing Help'),
    (Ident: 14; Value: 'Law'),
    (Ident: 15; Value: 'Managerial'),
    (Ident: 16; Value: 'Manufacturing'),
    (Ident: 17; Value: 'Medical/Health'));

  Interests: array[100..150] of record Ident: Byte; Value: String end =
    ((Ident: 100; Value: 'Art'),
    (Ident: 101; Value: 'Cars'),
    (Ident: 102; Value: 'Celebrity Fans'),
    (Ident: 103; Value: 'Collections'),
    (Ident: 104; Value: 'Computers'),
    (Ident: 105; Value: 'Culture & Literature'),
    (Ident: 106; Value: 'Fitness'),
    (Ident: 107; Value: 'Games'),
    (Ident: 108; Value: 'Hobbies'),
    (Ident: 109; Value: 'ICQ - Providing Help'),
    (Ident: 110; Value: 'Internet'),
    (Ident: 111; Value: 'Lifestyle'),
    (Ident: 112; Value: 'Movies/TV'),
    (Ident: 113; Value: 'Music'),
    (Ident: 114; Value: 'Outdoor Activities'),
    (Ident: 115; Value: 'Parenting'),
    (Ident: 116; Value: 'Pets/Animals'),
    (Ident: 117; Value: 'Religion'),
    (Ident: 118; Value: 'Science/Technology'),
    (Ident: 119; Value: 'Skills'),
    (Ident: 120; Value: 'Sports'),
    (Ident: 121; Value: 'Web Design'),
    (Ident: 122; Value: 'Nature and Environment'),
    (Ident: 123; Value: 'News & Media'),
    (Ident: 124; Value: 'Government'),
    (Ident: 125; Value: 'Business & Economy'),
    (Ident: 126; Value: 'Mystics'),
    (Ident: 127; Value: 'Travel'),
    (Ident: 128; Value: 'Astronomy'),
    (Ident: 129; Value: 'Space'),
    (Ident: 130; Value: 'Clothing'),
    (Ident: 131; Value: 'Parties'),
    (Ident: 132; Value: 'Women'),
    (Ident: 133; Value: 'Social science'),
    (Ident: 134; Value: '60''s'),
    (Ident: 135; Value: '70''s'),
    (Ident: 136; Value: '80''s'),
    (Ident: 137; Value: '50''s'),
    (Ident: 138; Value: 'Finance and corporate'),
    (Ident: 139; Value: 'Entertainment'),
    (Ident: 140; Value: 'Consumer electronics'),
    (Ident: 141; Value: 'Retail stores'),
    (Ident: 142; Value: 'Health and beauty'),
    (Ident: 143; Value: 'Media'),
    (Ident: 144; Value: 'Household products'),
    (Ident: 145; Value: 'Mail order catalog'),
    (Ident: 146; Value: 'Business services'),
    (Ident: 147; Value: 'Audio and visual'),
    (Ident: 148; Value: 'Sporting and athletic'),
    (Ident: 149; Value: 'Publishing'),
    (Ident: 150; Value: 'Home automation'));

  RandGroups: array[1..11] of record Ident: Byte; Value: String end =
    ((Ident: 1; Value: 'General'),
    (Ident: 2; Value: 'Romance'),
    (Ident: 3; Value: 'Games'),
    (Ident: 4; Value: 'Students'),
    (Ident: 5; Value: '20 something'),
    (Ident: 6; Value: '30 something'),
    (Ident: 7; Value: '40 something'),
    (Ident: 8; Value: '50+'),
    (Ident: 9; Value: 'Romance'),
    (Ident: 10; Value: 'Man requesting woman'),
    (Ident: 11; Value: 'Woman requesting man'));

  Organizations: array[0..19] of record Ident: Word; Value: String end =
    ((Ident: 200; Value: 'Alumni Org.'),
    (Ident: 201; Value: 'Charity Org.'),
    (Ident: 202; Value: 'Club/Social Org.'),
    (Ident: 203; Value: 'Community Org.'),
    (Ident: 204; Value: 'Cultural Org.'),
    (Ident: 205; Value: 'Fan Clubs'),
    (Ident: 206; Value: 'Fraternity/Sorority'),
    (Ident: 207; Value: 'Hobbyists Org.'),
    (Ident: 208; Value: 'International Org.'),
    (Ident: 209; Value: 'Nature and Environment Org.'),
    (Ident: 210; Value: 'Professional Org.'),
    (Ident: 211; Value: 'Scientific/Technical Org.'),
    (Ident: 212; Value: 'Self Improvement Group'),
    (Ident: 213; Value: 'Spiritual/Religious Org.'),
    (Ident: 214; Value: 'Sports Org.'),
    (Ident: 215; Value: 'Support Org.'),
    (Ident: 216; Value: 'Trade and Business Org.'),
    (Ident: 217; Value: 'Union'),
    (Ident: 218; Value: 'Volunteer Org.'),
    (Ident: 299; Value: 'Other'));

  Pasts: array[0..7] of record Ident: Word; Value: String end =
    ((Ident: 300; Value: 'Elementary School'),
    (Ident: 301; Value: 'High School'),
    (Ident: 302; Value: 'College'),
    (Ident: 303; Value: 'University'),
    (Ident: 304; Value: 'Military'),
    (Ident: 305; Value: 'Past Work Place'),
    (Ident: 306; Value: 'Past Organization'),
    (Ident: 399; Value: 'Other'));



//------------------------------------------------------------------------------------------------------------\
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Var
  gPortRange:TPortRange;
  InfoTime, ExtInfoTime, ExtStatusTime : LongWord;
  CAPS : String;

implementation
const
  FileSignature: array[0..15] of Byte = ($f0, $2d, $12, $d9, $30, $91, $d3, $11,
                                         $8d, $d7, $00, $10, $4b, $06, $46, $2e);

  ContactsSignature: array[0..15] of Byte = ($2a, $0e, $7d, $46, $76, $76, $d4, $11,
                                             $bc, $e6, $00, $04, $ac, $96, $1e, $a6);

  SecPerDay=86400;
  Offset1970=25569;


function iif(ACondition:Boolean; AIfTrue, AIfFalse: String): String;
begin
  if ACondition
    then Result := AIfTrue
    else Result := AIfFalse;
end;

function IntToStr(Value: Int64): String;
begin
  Str(Value, Result);
end;

function StrToInt(const Value: String): LongWord;
var
  nCode: Integer;
begin
  Val(Value, Result, nCode);
end;

const
  HexChars: array[0..15] of Char = ('0', '1', '2', '3', '4', '5',
                                    '6', '7', '8', '9', 'a', 'b',
                                    'c', 'd', 'e', 'f');

function IntToHex(Int: Int64; IntSize: Byte): String;
var
  n: Byte;
begin
  Result := '';
  for n := 0 to IntSize - 1 do
  begin
    Result := HexChars[Int and $F] + Result;
    Int := Int shr $4;
  end;
end;

function HexToInt(Value: String): LongWord;
const
  HexStr: String = '0123456789abcdef';
var
  i: Word;
begin
  Result := 0;
  if Value = '' then Exit;
  for i := 1 to Length(Value) do
    Inc(Result, (Pos(Value[i], HexStr) - 1) shl ((Length(Value) - i) shl 2));
end;

const
  TXorData: array[0..15] of Byte = (
    $F3, $26, $81, $C4, $39, $86, $DB, $92,
    $71, $A3, $B9, $E6, $53, $7A, $95, $7C
  );

procedure ICQEncryptPass(SrcBuf: Pointer; BufLen: LongWord); assembler;
asm
  or    edx,edx
  jz    @@end
@@loop:
  mov   cl,byte ptr[eax + edx - 1]
  xor   cl,byte ptr[TXorData + edx - 1]
  mov   byte ptr[eax + edx - 1],cl
  dec   edx
  jnz   @@loop
@@end:
end;

procedure ICQEncryptPassStr(var Pass: String);
var
  i: Word;
begin
  for i := 1 to Length(Pass) do
    Pass[i] := Chr(Ord(Pass[i]) xor TXorData[i - 1]);
end;

function Swap16(Value: Word): Word; assembler;
asm
  rol   ax,8
end;

function Swap32(Value: LongWord): LongWord; assembler;
asm
  bswap eax
end;

{----------------------------------------------}
//Adding data in reverse order
procedure PktAddData(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
var
  i: Word;
begin
  if DataLen = 0 then Exit;
  if Pkt^.Len + DataLen >= 8192 then
  begin
    DataLen := MAX_DATA_LEN - Pkt^.Len;
    if DataLen = 0 then Exit;
  end;
  for i := 0 to DataLen - 1 do
    PByte(LongWord(Pkt) + Pkt^.Len + i)^ := PByte(LongWord(Data) + DataLen - i - 1)^;
  Inc(Pkt^.Len, DataLen);
end;

//Adding data in direct order(local arrays, merging 2 or more packets)
procedure PktAddArrBuf(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
begin
  if (DataLen = 0) or (Pkt^.Len >= MAX_DATA_LEN) then
  begin
    Exit;
  end;
  if Pkt^.Len + DataLen >= 8192 then
  begin
    DataLen := MAX_DATA_LEN - Pkt^.Len;
    if DataLen = 0 then Exit;
  end;
  Move(Data^, Ptr(LongWord(Pkt) + Pkt^.Len)^, DataLen);
  Inc(Pkt^.Len, DataLen);
end;

procedure PktInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
begin
  PktAddData(Pkt, @Value, IntSize);
end;

procedure PktLInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
begin
  PktAddArrBuf(Pkt, @Value, IntSize);
end;

procedure PktStr(Pkt: PRawPkt; const S: String);
begin
  PktAddArrBuf(Pkt, @S[1], Length(S));
end;

procedure PktLStr(Pkt: PRawPkt; const S: String); overload;
begin
  PktInt(Pkt, Length(S), 1);
  PktStr(Pkt, S);
end;

procedure PktLStr(Pkt: PRawPkt; S: LongWord); overload;
begin
  PktLStr(Pkt, IntToStr(S));
end;

procedure PktWStr(Pkt: PRawPkt; const S: String);
begin
  if Length(S) = 0 then
  begin
    PktInt(Pkt, 0, 2);
    Exit;
  end;
  PktInt(Pkt, Length(S), 2);
  PktStr(Pkt, S);
end;

procedure PktDWStr(Pkt: PRawPkt; const S: String);
begin
  PktLInt(Pkt, Length(S), 4);
  PktStr(Pkt, S);
end;

procedure PktLNTS(Pkt: PRawPkt; const S: String);
begin
  if Length(S) = 0 then
  begin
    PktInt(Pkt, 0, 2);
    Exit;
  end;
  PktLInt(Pkt, Length(S) + 1, 2);
  PktStr(Pkt, S);
  PktInt(Pkt, 0, 1);
end;

procedure PktLLNTS(Pkt: PRawPkt; const S: String);
begin
  if Length(S) = 0 then
  begin
    PktInt(Pkt, 0, 2);
    Exit;
  end;
  PktLInt(Pkt, Length(S) + 3, 2);
  PktLNTS(Pkt, S);
end;



{--}
function GetInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
var
  i: Word;
begin
  Result := 0;
  if IntSize = 0 then Exit;
  if Pkt^.Len > 8100 then Exit;
  for i := Pkt^.Len to Pkt^.Len + IntSize - 1 do
    Inc(Result, PByte(LongWord(Pkt) + i)^ shl ((Pkt^.Len + IntSize - 1 - i) * 8));
  Inc(Pkt^.Len, IntSize);
end;

function GetLInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
var
  i, c: Word;
begin
  Result := 0; c := 0;
  if IntSize = 0 then Exit;
  if Pkt^.Len > 8100 then Exit;
  for i := Pkt^.Len to Pkt^.Len + IntSize - 1 do
  begin
    Inc(Result, PByte(LongWord(Pkt) + Pkt^.Len + IntSize - c - 1)^ shl ((Pkt^.Len + IntSize - 1 - i) * 8));
    Inc(c);
  end;
  Inc(Pkt^.Len, IntSize);
end;

function GetStr(Pkt: PRawPkt; StrLen: Integer): String;
begin
  Result := '';
  while StrLen > 0 do
  begin
    Result := Result + PChar(LongWord(Pkt) + Pkt^.Len)^;
    Inc(Pkt^.Len);
    Dec(StrLen);
    if Pkt^.Len > 8100 then Exit;
  end;
end;

function GetTLVStr(Pkt: PRawPkt; var T: Word): String;
var
  ISize: Word;
begin
  T := GetInt(Pkt, 2);          //Get type
  ISize := GetInt(Pkt, 2);      //Get data length
  Result := GetStr(Pkt, ISize); //Get data
end;

function GetTLVInt(Pkt: PRawPkt; var T: Word): LongWord;
var
  ISize: Word;
begin
  T := GetInt(Pkt, 2);          //Get type
  ISize := GetInt(Pkt, 2);      //Get data length
  Result := GetInt(Pkt, ISize); //Get data
end;

procedure GetSnac(Pkt: PRawPkt; var Snac: TSnacHdr);
begin
 {
  Snac := PSnacHdr(LongWord(Pkt) + Pkt^.Len)^;
  Snac.Family := Swap16(Snac.Family);
  Snac.SubType := Swap16(Snac.SubType);
  Snac.ReqID := Swap32(Snac.ReqID);
  Snac.Flags := Swap16(Snac.Flags);
  Inc(Pkt^.Len, TSNACSZ);
  }
  Snac.Family := GetInt(Pkt,2);
  Snac.SubType := GetInt(Pkt,2);
  Snac.Flags := GetInt(Pkt,2);
  Snac.ReqID := GetInt(Pkt,4);
end;

function GetLStr(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, GetInt(Pkt, 1));
end;

function GetWStr(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, GetInt(Pkt, 2));
end;

function GetDWStr(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, GetLInt(Pkt, 4));
end;

function GetLNTS(Pkt: PRawPkt): String;
begin
  Result:=GetStr(Pkt, LongInt (GetLInt(Pkt, 2)) - 1);
  Inc(Pkt^.Len, 1);
end;


{--------}
procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: LongWord); overload;
begin
  PktInt(Pkt, T, 2);  //Add type
  PktInt(Pkt, L, 2);  //Add length
  PktInt(Pkt, V, L);  //Add value
end;

procedure PktTLV(Pkt: PRawPkt; T: Word; const V: String); overload;
begin
  PktInt(Pkt, T, 2);            //Add type
  PktInt(Pkt, Length(V), 2);    //Add length
  PktStr(Pkt, V);               //Add value
end;

procedure PktLTLV(Pkt: PRawPkt; T: Word; const V: String); overload;
begin
  PktInt(Pkt, Swap16(T), 2);            //Add type
  PktInt(Pkt, Swap16(Length(V)+3), 2);    //Add length
  PktInt(Pkt, Swap16(Length(V)+1), 2);    //Add length
  PktStr(Pkt, V+#0);               //Add value
end;

procedure PktLBTLV(Pkt: PRawPkt; T: Word; V: String; B: Byte); overload;
begin
  PktInt(Pkt, Swap16(T), 2);            //Add type
  PktInt(Pkt, Swap16(Length(V)+4), 2);    //Add length
  PktInt(Pkt, Swap16(Length(V)+1), 2);    //Add length
  PktStr(Pkt, V+#0);               //Add value
  PktInt(Pkt, B,1);               //Add value
end;

procedure PktLTLV(Pkt: PRawPkt; T,L: Word; V: LongWord); overload;
begin
  PktInt(Pkt, Swap16(T), 2);            //Add type
  PktInt(Pkt, Swap16(L), 2);            //Add length
  if L = 1
    then PktInt(Pkt, V, L)
    else PktInt(Pkt, Swap16(V), L);    //Add value
end;

procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: Pointer); overload; //for arrays
begin
  PktInt(Pkt, T, 2);            //Add type
  PktInt(Pkt, L, 2);            //Add length
  PktAddArrBuf(Pkt, V, L);      //Add value
end;

procedure PktInit(Pkt: PRawPkt; Channel: Byte; var Seq: Word);
begin
  Pkt^.Len := 0;                //Starting size of packet to 0
  PktInt(Pkt, $2A, 1);          //Ident, always $2A
  PktInt(Pkt, Channel, 1);      //Channel
  PktInt(Pkt, SEQ, 2); Inc(SEQ);//Seq
  PktInt(Pkt, 0, 2);            //Reserved for size
end;


{$IFDEF USE_ASM}
procedure PktInitRaw(Pkt: PRawPkt); assembler;
asm
  mov   word ptr[eax + MAX_DATA_LEN],0          //Default size of the packet
end;
{$ELSE}
procedure PktInitRaw(Pkt: PRawPkt);
begin
  Pkt^.Len := 0;                                //Default size of the packet
end;
{$ENDIF}


//Used with PktInit only
{$IFDEF USE_ASM}
procedure PktFinal(Pkt: PRawPkt); assembler;
asm
  mov   cx,word ptr[eax + MAX_DATA_LEN]
  sub   cx,TFLAPSZ
  rol   cx,8
  mov   word ptr[eax + 4],cx   //Store the packet size (without flap header size)
end;
{$ELSE}
procedure PktFinal(Pkt: PRawPkt);
begin
  PWord(LongWord(Pkt) + 4)^ := Swap16(Pkt.Len - TFLAPSZ); //Store the packet size (without flap header size)
end;
{$ENDIF}

procedure PktSnac(Pkt: PRawPkt; Family, SubType: Word; ID: LongWord; Flags: Word);
begin
  PktInt(Pkt, Family, 2);       //Snac family
  PktInt(Pkt, SubType, 2);      //Snac subtype
  PktInt(Pkt, Flags, 2);        //Snac flags
  PktInt(Pkt, ID, 4);           //Snac reference
end;

{@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@}
function StrToLanguageI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Languages) to High(Languages) do
    if Languages[i].Value = Value then
    begin
      Result := Languages[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToCountryI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Countries) to High(Countries) do
    if Countries[i].Value = Value then
    begin
      Result := Countries[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToInterestI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Interests) to High(Interests) do
    if Interests[i].Value = Value then
    begin
      Result := Interests[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToOccupationI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Occupations) to High(Occu