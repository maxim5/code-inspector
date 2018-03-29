/*
 * Data.cpp
 *
 *  Created on: 2014年3月5日
 *      Author: acm
 */

#include "SystemData.h"

namespace Data
{

//DI
SystemData<unsigned> diAlcPrsULmt(0, "alcohol gas pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 0, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diAlcPrsLLmt(1, "alcohol gas pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 1, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diAlcTkPrsULmt(2, "alcohol tank gas pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 2, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diAlcTkPrsLLmt(3, "alcohol tank gas pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 3, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diHFSplPrsULmt(4, "HF supply pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 4, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diHFSplPrsLLmt(5, "HF supply pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 5, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgHFPrsULmt(6, "N2 purge HF tube pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 6, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgHFPrsLLmt(7, "N2 purge HF tube pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 7, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgAlcTkPrsULmt(8, "N2 purge alcohol tank pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 8, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgAlcTkPrsLLmt(9, "N2 purge alcohol tank pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 9, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgExpCbPrsULmt(10, "N2 purge expansion chamber pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 10, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgExpCbPrsLLmt(11, "N2 purge expansion chamber pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 11, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2SplVPumpPrsULmt(12, "N2 supply vacuum pump pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 12, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2SplVPumpPrsLLmt(13, "N2 supply vacuum pump pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 13, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgPrcCbPrsULmt(14, "N2 purge process chamber pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 14, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgPrcCbPrsLLmt(15, "N2 purge process chamber pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 15, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2FacSplPrsULmt(16, "N2 facility supply pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 16, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2FacSplPrsLLmt(17, "N2 facility supply pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 17, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgVapPrsULmt(18, "N2 purge vaporizer pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 18, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diN2PgVapPrsLLmt(19, "N2 purge vaporizer pressure lower limit sensor", "DI", 0, 1, 0, 0, 0, 19, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diFacVPrsSwtLLmt(21, "facility vacuum pressure switch lower limit sensor", "DI", 0, 1, 0, 0, 0, 21, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diExpCbVPrsSwtULmt(22, "expansion chamber vacuum pressure switch upper limit sensor", "DI", 0, 1, 0, 0, 0, 22, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diExpCbVPrsSwtLLmt(23, "expansion chamber vacuum pressure switch lower limit sensor", "DI", 0, 1, 0, 0, 0, 23, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diIODoorClose(24, "IO door closed", "DI", 0, 1, 0, 0, 0, 24, 0, 1, false);
SystemData<unsigned> diPCDoorClose(25, "PC door closed", "DI", 0, 1, 0, 0, 0, 25, 0, 1, false);
SystemData<unsigned> diPneuDoorClose(26, "pneumatic door closed", "DI", 0, 1, 0, 0, 0, 26, 0, 1, false);
SystemData<unsigned> diAcFrmDoorClose(27, "AC frame door closed", "DI", 0, 1, 0, 0, 0, 27, 0, 1, false);
SystemData<unsigned> diAcDoorOverride(28, "AC door orerride", "DI", 0, 1, 0, 0, 0, 28, 0, 1, false);
SystemData<unsigned> diVaporMHeaterAlarm(29, "vaporizer main heater alarm", "DI", 0, 1, 0, 0, 0, 29, 0, 1, false);
SystemData<unsigned> diVaporVHeaterAlarm(30, "vaporizer vapor heater alarm", "DI", 0, 1, 0, 0, 0, 30, 0, 1, false);
SystemData<unsigned> diHFReady(31, "HF ready", "DI", 0, 1, 0, 0, 0, 31, 0, 1, false);
SystemData<unsigned> diHeartbeatFail(32, "Heartbeat failed", "DI", 0, 1, 0, 0, 0, 32, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diExhaustAlarm(33, "gas box exhaust pressure alarm", "DI", 0, 1, 0, 0, 0, 33, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diFacInletVPrsULmt(34, "facility main inlet vacuum pressure upper limit sensor", "DI", 0, 1, 0, 0, 0, 34, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diAlcoholLeak(35, "alcohol leak", "DI", 0, 1, 0, 0, 0, 35, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPrcCbVacuumFail(36, "process chamber vacuum failed", "DI", 0, 1, 0, 0, 0, 36, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPrcCbDoorClose(37, "process chamber door closed", "DI", 0, 1, 0, 0, 0, 37, 0, 1, false);
SystemData<unsigned> diAlcoholGasLeak(38, "alcohol gas leak", "DI", 0, 1, 0, 0, 0, 38, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diExhaustPresAlarm(39, "main frame exhaust pressure alarm", "DI", 0, 1, 0, 0, 0, 39, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diGasboxDoorClose(40, "gas box door closed", "DI", 0, 1, 0, 0, 0, 40, 0, 1, false);
SystemData<unsigned> diDoorOverride(41, "door override", "DI", 0, 1, 0, 0, 0, 41, 0, 1, false);
SystemData<unsigned> diInterlockReset(42, "interlock reset", "DI", 0, 1, 0, 0, 0, 42, 0, 1, false);
SystemData<unsigned> diGasBoxHFLeak(43, "HF leak", "DI", 0, 1, 0, 0, 0, 43, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diThrottleValveOpen(44, "throttle valve open", "DI", 0, 1, 0, 0, 0, 44, 0, 1, false);
SystemData<unsigned> diHWInterlock(45, "hardware interlock", "DI", 0, 1, 0, 0, 0, 45, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diAlcTkLow(46, "alcohol tank low level sensor", "DI", 0, 1, 0, 0, 0, 46, 0, 1, false);
SystemData<unsigned> diAlcTkHigh(47, "alcohol tank high level sensor", "DI", 0, 1, 0, 0, 0, 47, 0, 1, false);
SystemData<unsigned> diAlcTkHighHigh(48, "alcohol tank high-high level sensor", "DI", 0, 1, 0, 0, 0, 48, 0, 1, false);
SystemData<unsigned> diAlcTkOverfill(49, "alcohol tank over fill", "DI", 0, 1, 0, 0, 0, 49, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diVPumpWarning(50, "vacuum pump warning", "DI", 0, 1, 0, 0, 0, 50, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diVPumpAlarm(51, "vacuum pump alarm", "DI", 0, 1, 0, 0, 0, 51, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPlumbing1Alarm(52, "plumbing heater#1 alarm", "DI", 0, 1, 0, 0, 0, 52, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPlumbing2Alarm(53, "plumbing heater#2 alarm", "DI", 0, 1, 0, 0, 0, 53, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPlumbing3Alarm(54, "plumbing heater#3 alarm", "DI", 0, 1, 0, 0, 0, 54, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPlumbing4Alarm(55, "plumbing heater#4 alarm", "DI", 0, 1, 0, 0, 0, 55, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPlumbing5Alarm(56, "plumbing heater#5 alarm", "DI", 0, 1, 0, 0, 0, 56, 0, 1, false,"",ConvertNot<unsigned>);
SystemData<unsigned> diPrcCbDoorOpen(60, "process chamber door open", "DI", 0, 1, 0, 0, 0, 60, 0, 1, false);
SystemData<unsigned> diPinUp(61, "pin up position sensor", "DI", 0, 1, 0, 0, 0, 61, 0, 1, false);
SystemData<unsigned> diPinDown(62, "pin down position sensor", "DI", 0, 1, 0, 0, 0, 62, 0, 1, false);
SystemData<unsigned> diForkVertical(63, "fork vertical position sensor", "DI", 0, 1, 0, 0, 0, 63, 0, 1, false);
SystemData<unsigned> diForkHorizontal(64, "fork horizontal position sensor", "DI", 0, 1, 0, 0, 0, 64, 0, 1, false);
SystemData<unsigned> diArmIn(65, "arm in position sensor", "DI", 0, 1, 0, 0, 0, 65, 0, 1, false);
SystemData<unsigned> diArmOut(66, "arm out position sensor", "DI", 0, 1, 0, 0, 0, 66, 0, 1, false);
//SystemData<unsigned> diPrcCbLidLeftOpen(67, "diPrcCbLidLeftOpen", "DI", 0, 1, 0, 0, 0, 67, 0, 1, false);
//SystemData<unsigned> diPrcCbLidLeftClose(68, "diPrcCbLidLeftClose", "DI", 0, 1, 0, 0, 0, 68, 0, 1, false);
//SystemData<unsigned> diPrcCbLidRightOpen(69, "diPrcCbLidRightOpen", "DI", 0, 1, 0, 0, 0, 69, 0, 1, false);
//SystemData<unsigned> diPrcCbLidRightClose(70, "diPrcCbLidRightClose", "DI", 0, 1, 0, 0, 0, 70, 0, 1, false);
SystemData<unsigned> diCbLeftDoorClose(76, "chamber left door closed", "DI", 0, 1, 0, 0, 0, 76, 0, 1, false);
SystemData<unsigned> diCbRightDoorClose(77, "chamber right door closed", "DI", 0, 1, 0, 0, 0, 77, 0, 1, false);
SystemData<unsigned> diProcCbHFLeak(78, "process chamber HF leak","DI",0,1,0,0,0,78,0,1,false,"",ConvertNot<unsigned>);
SystemData<unsigned> diCDAInletAlarm(79, "Main CDA inlet pressure alarm", "DI", 0, 1, 0, 0, 0, 79, 0, 1, false, "", ConvertNot<unsigned>);
SystemData<unsigned> diEStop(80, "EStop", "DI", 0, 1, 0, 0, 0, 80, 0, 1, false, "", ConvertNot<unsigned>);


//DO
SystemData<unsigned> aoRedLight(1000,"red light","DO",0,256,0,0,1,0,0,16,true);
SystemData<unsigned> aoYellowLight(1002,"yellow light","DO",0,256,0,0,1,2,0,16,true);
SystemData<unsigned> aoGreenLight(1004,"green light","DO",0,256,0,0,1,4,0,16,true);
SystemData<unsigned> aoBlueLight(1006,"blue light","DO",0,256,0,0,1,6,0,16,true);
SystemData<unsigned> aoBuzzer(1008,"buzzer","DO",0,256,0,0,1,8,0,16,true);
SystemData<unsigned> doHeartbeat(1010,"heartbeat","DO",0,1,0,0,1,10,0,1,true);
SystemData<unsigned> doEnableVPump(1011,"vacuum pump enable","DO",0,1,0,0,1,11,0,1,true);
SystemData<unsigned> doHFRequest(1012,"HF request","DO",0,1,0,0,1,12,0,1,true);
SystemData<unsigned> doN2PurgeGBValve(1016,"N2 purge gas box valve","DO",0,1,0,0,1,16,0,1,true);
SystemData<unsigned> doExpCbVacValve(1018,"expansion chamber vacuum valve","DO",0,1,0,0,1,18,0,1,true);
SystemData<unsigned> doExpCbVapVacValve(1019,"expansion chamber & vaporizer vacuum valve","DO",0,1,0,0,1,19,0,1,true);
SystemData<unsigned> doAlcMFCVal1(1021,"alcohol MFC valve1","DO",0,1,0,0,1,21,0,1,true);
SystemData<unsigned> doAlcMFCVal2(1022,"alcohol MFC valve2","DO",0,1,0,0,1,22,0,1,true);
SystemData<unsigned> doAlcMFCVal3(1023,"alcohol MFC bypass valve","DO",0,1,0,0,1,23,0,1,true,"",ConvertNot<unsigned>,ConvertNot<unsigned>);
SystemData<unsigned> doVapSupplyN2Valve(1024,"vaporizer supply N2 valve","DO",0,1,0,0,1,24,0,1,true);
SystemData<unsigned> doVapBypassValve(1025,"vaporizer bypass valve","DO",0,1,0,0,1,25,0,1,true);
SystemData<unsigned> doHFMFCVal1(1026,"HF MFC valve1","DO",0,1,0,0,1,26,0,1,true);
SystemData<unsigned> doHFMFCVal2(1027,"HF MFC valve2","DO",0,1,0,0,1,27,0,1,true);
SystemData<unsigned> doHFMFCVal3(1028,"HF MFC bypass valve","DO",0,1,0,0,1,28,0,1,true,"",ConvertNot<unsigned>,ConvertNot<unsigned>);
SystemData<unsigned> doPurgeN2MFCVal1(1029,"purge N2 MFC valve1","DO",0,1,0,0,1,29,0,1,true);
SystemData<unsigned> doPurgeN2MFCVal2(1030,"purge N2 MFC valve2","DO",0,1,0,0,1,30,0,1,true);
SystemData<unsigned> doVaVapValve(1032,"vaporizer tube vacuum valve","DO",0,1,0,0,1,32,0,1,true);
SystemData<unsigned> doVaHFValve(1033,"HF tube vacuum valve","DO",0,1,0,0,1,33,0,1,true);
SystemData<unsigned> doVaSupplyIPAValve(1034,"vaporizer supply IPA valve","DO",0,1,0,0,1,34,0,1,true);
SystemData<unsigned> doExpCbVacIPASupply(1035,"IPA supply expansion chamber vacuum valve","DO",0,1,0,0,1,35,0,1,true);
SystemData<unsigned> doAlcTankOpen(1036,"alcohol tank opening valve","DO",0,1,0,0,1,36,0,1,true);
SystemData<unsigned> doPurgeAlcTank(1037,"alcohol tank purge valve","DO",0,1,0,0,1,37,0,1,true);
SystemData<unsigned> doExpCbHFInletVal(1038,"expansion chamber HF inlet valve","DO",0,1,0,0,1,38,0,1,true);
SystemData<unsigned> doHFFacSupplyVal(1039,"HF facility supply valve","DO",0,1,0,0,1,39,0,1,true);
SystemData<unsigned> doN2PurgeHFVal(1040,"HF tube purge valve","DO",0,1,0,0,1,40,0,1,true);
SystemData<unsigned> doExpCbSupplyCbVal(1041,"expansion chamber supply process chamber valve","DO",0,1,0,0,1,41,0,1,true);
SystemData<unsigned> doN2SupplyVacVal(1048,"N2 supply vacuum valve","DO",0,1,0,0,1,48,0,1,true);
SystemData<unsigned> doN2SupplyProcVal(1049,"N2 supply process chamber valve","DO",0,1,0,0,1,49,0,1,true);
SystemData<unsigned> doN2MFCVal1(1050,"N2 MFC valve1","DO",0,1,0,0,1,50,0,1,true);
SystemData<unsigned> doN2MFCVal2(1051,"N2 MFC valve2","DO",0,1,0,0,1,51,0,1,true);
SystemData<unsigned> doPinUp(1052,"pin up","DO",0,1,0,0,1,52,0,1,true);
SystemData<unsigned> doArmIn(1053,"arm in","DO",0,1,0,0,1,53,0,1,true);
SystemData<unsigned> doForkVertical(1054,"fork vertical","DO",0,1,0,0,1,54,0,1,true);
SystemData<unsigned> doVacSlowProcCbVal(1056,"slow pump valve","DO",0,1,0,0,1,56,0,1,true);
SystemData<unsigned> doVacFastProcCbVal(1057,"fast pump valve","DO",0,1,0,0,1,57,0,1,true);
SystemData<unsigned> doPinDown(1063,"pin down","DO",0,1,0,0,1,63,0,1,true);
SystemData<unsigned> doArmOut(1064,"arm out","DO",0,1,0,0,1,64,0,1,true);
SystemData<unsigned> doForkHorizontal(1065,"fork horizontal","DO",0,1,0,0,1,65,0,1,true);
SystemData<unsigned> doVapInletVal(1067,"vaporizer inlet valve","DO",0,1,0,0,1,67,0,1,true);
SystemData<unsigned> doCbGateOpen(1071,"open process chamber door","DO",0,1,0,0,1,71,0,1,true);
SystemData<unsigned> doCbGateClose(1072,"close process chamber door","DO",0,1,0,0,1,72,0,1,true);



//AI
SystemData<float> aiFacPressure(2000,"facility vacuum pressure","AI",0,0,2,0,2,0,0,16,false,"kPa", ConvertLinear<float,unsigned>(3276, 16383, -100.0f, 1000.0f));
SystemData<float> aiExpPressure(2002,"expansion chamber vacuum pressure","AI",0,0,2,0,2,2,0,16,false,"kPa", ConvertLinear<float,unsigned>(3276, 16383, -100.0f, 1000.0f));
SystemData<float> aiAlcPressure(2004,"alcohol gas pressure","AI",0,0,2,0,2,4,0,16,false,"kPa", ConvertLinear<float,unsigned>(3276, 16383, -100.0f, 100.0f));
SystemData<float> aiAlcTankPressure(2006,"alcohol tank gas pressure","AI",0,0,2,0,2,6,0,16,false,"kPa", ConvertLinear<float,unsigned>(3276, 16383, -100.0f, 1000.0f));
SystemData<float> aiHFSupplyPressure(2008,"HF supply pressure","AI",0,0,2,0,2,8,0,16,false,"kPa", ConvertLinear<float,unsigned>(3276, 16383, -100.0f, 1000.0f));
SystemData<float> aiAlcGasLeak(2010,"alcohol gas leak signal","AI",0,0,1,0,2,10,0,16,false,"%lel", ConvertLinear<float,unsigned>(6540, 32701, 0.0f, 100.0f));
SystemData<float> aiHFGasboxPPM(2012,"gas box HF concentration","AI",0,0,2,0,2,12,0,16,false,"ppm", ConvertLinear<float,unsigned>(6540, 32701, 0.0f, 10.0f));
SystemData<float> aiHFChamberPPM(2014,"chamber HF concentration","AI",0,0,2,0,2,14,0,16,false,"ppm", ConvertLinear<float,unsigned>(6540, 32701, 0.0f, 10.0f));
SystemData<float> aiGasboxTC(2022,"gas box temperature","AI",0,0,1,0,2,22,0,16,false,"Cels", ConvertRatio<float,short>(10.0f));
SystemData<float> aiPipelineTC(2024,"pipeline temperature","AI",0,0,1,0,2,24,0,16,false,"Cels", ConvertRatio<float,short>(10.0f));


//AO


//Body Heater
SystemData<float> aiBodyHTTemp(4000, "body heater temperature","Heater",0,0,1,0,4,0,0,32,false,"Cels",ConvertMemery<float, unsigned>);
SystemData<unsigned> aiBodyHTErrCode(4008, "body heater error code","Heater",0,0,0,0,4,8,0,16,false);
SystemData<unsigned> diBodyHTPowRdy(4010, "body heater power status","Heater",0,1,0,0,4,10,0,1,false);
SystemData<unsigned> diBodyHTErr(4011, "body heater alarm","Heater",0,1,0,0,4,11,0,1,false);
SystemData<unsigned> doBodyHTEnable(4018, "enable body heater","Heater",0,1,0,0,4,18,0,1,true);
SystemData<float> aoBodyHTTempSet(4026, "body heater temperature setpoint","Heater",0,1000,1,0,4,26,0,32,true,"Cels",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);

//Lid Heater
SystemData<float> aiLidHTTemp(5000, "lid heater temperature","Heater",0,0,1,0,5,0,0,32,false,"Cels",ConvertMemery<float, unsigned>);
SystemData<unsigned> aiLidHTErrCode(5008, "lid heater error code","Heater",0,0,0,0,5,8,0,16,false);
SystemData<unsigned> diLidHTPowRdy(5010, "lid heater power status","Heater",0,1,0,0,5,10,0,1,false);
SystemData<unsigned> diLidHTErr(5011, "lid heater alarm","Heater",0,1,0,0,5,11,0,1,false);
SystemData<unsigned> doLidHTEnable(5018, "enable lid heater","Heater",0,1,0,0,5,18,0,1,true);
SystemData<float> aoLidHTTempSet(5026, "lid heater temperature setpoint","Heater",0,1000,1,0,5,26,0,32,true,"Cels",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);


//Chuck Heater
SystemData<float> aiChuckHTTemp(6000, "chuck heater temperature","Heater",0,0,1,0,6,0,0,32,false,"Cels",ConvertMemery<float, unsigned>);
SystemData<unsigned> aiChuckHTErrCode(6008, "chuck heater error code","Heater",0,0,0,0,6,8,0,16,false);
SystemData<unsigned> diChuckHTPowRdy(6010, "chuck heater power status","Heater",0,1,0,0,6,10,0,1,false);
SystemData<unsigned> diChuckHTErr(6011, "chuck heater alarm","Heater",0,1,0,0,6,11,0,1,false);
SystemData<unsigned> doChuckHTEnable(6018, "enable chuck heater","Heater",0,1,0,0,6,18,0,1,true);
SystemData<float> aoChuckHTTempSet(6026, "chuck heater temperature setpoint","Heater",0,1000,1,0,6,26,0,32,true,"Cels",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);


//Motion
SystemData<unsigned> diHomeSensor(7000,"home sensor","Motion",0,1,0,0,7,0,0,1,false);
SystemData<unsigned> diCWLimitStatus(7001,"CW limit sensor","Motion",0,1,0,0,7,1,0,1,false,"",ConvertNot<unsigned>);
SystemData<unsigned> diCCWLimitStatus(7002,"CCW limit sensor","Motion",0,1,0,0,7,2,0,1,false,"",ConvertNot<unsigned>);
SystemData<unsigned> diReachPosition(7003,"position reached","Motion",0,1,0,0,7,3,0,1,false);
SystemData<unsigned> diDriverAlarm(7004,"driver alarm","Motion",0,1,0,0,7,4,0,1,false,"",ConvertNot<unsigned>);
SystemData<unsigned> diAxisError(7008,"axis error","Motion",0,1,0,0,7,8,0,1,false);
SystemData<unsigned> diAxisErrStop(7009,"axis error stop","Motion",0,1,0,0,7,9,0,1,false);
SystemData<unsigned> diAxisDisabled(7010,"axis disabled","Motion",0,1,0,0,7,10,0,1,false);
SystemData<unsigned> diAxisStandStill(7011,"axis stand still","Motion",0,1,0,0,7,11,0,1,false);
SystemData<unsigned> diAxisMoving(7012,"axis moving","Motion",0,1,0,0,7,12,0,1,false);
SystemData<unsigned> diAxisNotMoving(7013,"axis not moving","Motion",0,1,0,0,7,13,0,1,false);
//SystemData<unsigned> diAxisStopCmdBusy(7018,"diAxisStopCmdBusy","Motion",0,1,0,0,7,18,0,1,false);
//SystemData<unsigned> diAxisStopDone(7019,"diAxisStopDone","Motion",0,1,0,0,7,19,0,1,false);
//SystemData<unsigned> diAxisRstCmdBusy(7020,"diAxisRstCmdBusy","Motion",0,1,0,0,7,20,0,1,false);
//SystemData<unsigned> diAxisRstDone(7021,"diAxisRstDone","Motion",0,1,0,0,7,21,0,1,false);
//SystemData<unsigned> diAxisServoCmdBusy(7022,"diAxisServoCmdBusy","Motion",0,1,0,0,7,22,0,1,false);
SystemData<unsigned> diAxisServoDone(7023,"axis servo done","Motion",0,1,0,0,7,23,0,1,false);
SystemData<unsigned> diAxisHomeDone(7025,"axis home done","Motion",0,1,0,0,7,25,0,1,false);
SystemData<unsigned> aiAxisErrCode(7040, "axis error code","Motion",0,0,0,0,7,40,0,32,false);
SystemData<float> aiActualPosition(7044, "axis actual position","Motion",0,0,1,0,7,44,0,32,false,"deg",ConvertMemery<float, unsigned>);
SystemData<float> aiActualVelocity(7048, "axis actual velocity","Motion",0,0,1,0,7,48,0,32,false,"deg/s",ConvertMemery<float, unsigned>);

SystemData<unsigned> aoAxisControl(7052, "axis control","Motion",0,255,0,0,7,52,0,8,true);
SystemData<unsigned> doAxisExecute(7053, "axis execute","Motion",0,1,0,0,7,53,0,1,true);
SystemData<unsigned> doAxisServoOn(7054, "axis servo on","Motion",0,1,0,0,7,54,0,1,true);
SystemData<unsigned> doAxisReset(7055, "reset axis","Motion",0,1,0,0,7,55,0,1,true);
SystemData<float> aoAxisStopDec(7061, "axis stop deceleration setpoint","Motion",0,100,1,0,7,61,0,32,true,"deg/s2",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisStopJerk(7065, "axis stop jerk setpoint","Motion",0,100000,1,0,7,65,0,32,true,"deg/s3",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisAcc(7069, "axis acceleration setpoint","Motion",0,100,1,0,7,69,0,32,true,"deg/s2",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisDec(7073, "axis deceleration setpoint","Motion",0,100,1,0,7,73,0,32,true,"deg/s2",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisJerk(7077, "axis jerk setpoint","Motion",0,100000,1,0,7,77,0,32,true,"deg/s3",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisVelocity(7081, "axis velocity setpoint","Motion",-100,100,1,0,7,81,0,32,true,"deg/s",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisPosition(7085, "axis position setpoint","Motion",-160,160,1,0,7,85,0,32,true,"deg",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisDistance(7089, "axis distance setpoint","Motion",-320,320,1,0,7,89,0,32,true,"deg",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisVelOverride(7093, "axis velocity override setpoint","Motion",0,100,1,0,7,93,0,32,true,"%",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisPatrolPos1(7097, "axis patrol position1 setpoint","Motion",-160,160,1,0,7,97,0,32,true,"deg",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);
SystemData<float> aoAxisPatrolPos2(7101, "axis patrol position2 setpoint","Motion",-160,160,1,0,7,101,0,32,true,"deg",ConvertMemery<float, unsigned>,ConvertMemery<unsigned, float>);


//DeviceNet
SystemData<unsigned> aiAPCStatus(8000, "APC status","DeviceNet",0,0,0,0,8,0,0,8,false);
SystemData<float> aiAPCPressure(8001, "APC pressure","DeviceNet",0,0,2,0,8,1,0,32,false,"torr",ConvertLinear<float,unsigned>(0, 23405, 0.0f, 1000.0f));
SystemData<float> aiAPCPosition(8005, "APC position","DeviceNet",0,0,1,0,8,5,0,16,false,"%",ConvertLinear<float,unsigned>(0, 32767, 0.0f, 100.0f));
SystemData<unsigned> aiAPCOverrideStatus(8007, "APC override status","DeviceNet",0,0,0,0,8,7,0,8,false);
SystemData<unsigned> aiVapMFCStatus(8008, "alcohol MFC status","DeviceNet",0,0,0,0,8,8,0,8,false);
SystemData<float> aiEtOHFlowrate(8009, "alcohol flowrate","DeviceNet",0,0,0,0,8,9,0,16,false,"sccm",ConvertLinear2<float,unsigned>(0, 32767, 0.0f, 972.4f));
SystemData<unsigned> aiHFMFCStatus(8011, "HF MFC status","DeviceNet",0,0,0,0,8,11,0,8,false);
SystemData<float> aiHFFlowrate(8012, "HF flowrate","DeviceNet",0,0,0,0,8,12,0,16,false,"sccm",ConvertLinear2<float,unsigned>(0, 32767, 0.0f, 2670.0f));
SystemData<unsigned> aiPurgeN2MFCStatus(8014, "purge N2 MFC status","DeviceNet",0,0,0,0,8,14,0,8,false);
SystemData<float> aiPurgeN2Flowrate(8015, "purge N2 flowrate","DeviceNet",0,0,0,0,8,15,0,16,false,"sccm",ConvertLinear2<float,unsigned>(0, 32767, 0.0f, 6670.0f));
SystemData<unsigned> aiN2MFCStatus(8017, "N2 MFC status","DeviceNet",0,0,0,0,8,17,0,8,false);
SystemData<float> aiN2Flowrate(8018, "N2 flowrate","DeviceNet",0,0,0,0,8,18,0,16,false,"sccm",ConvertLinear2<float,unsigned>(0, 32767, 0.0f, 6670.0f));
SystemData<float> aiProcChamPressure(8020, "process chamber pressure","DeviceNet",0,0,2,0,8,20,0,16,false,"torr",ConvertLinear2<float,unsigned>(0, 23405, 0.0f, 1000.0f));
SystemData<unsigned> aiProcChamManoStatus(8022, "process chamber manometer status","DeviceNet",0,0,0,0,8,22,0,8,false);
SystemData<float> aiExpChamPressure(8023, "expansion chamber pressure","DeviceNet",0,0,2,0,8,23,0,16,false,"torr",ConvertLinear2<float,unsigned>(0, 23405, 0.0f, 1000.0f));
SystemData<unsigned> aiExpChamManoStatus(8025, "expansion chamber manometer status","DeviceNet",0,0,0,0,8,25,0,8,false);

SystemData<float> aoAPCPressure(8026, "APC pressure setpoint","DeviceNet",0,1000,1,0,8,26,0,16,true,"torr",ConvertLinear<float,unsigned>(0, 32767, 0.0f, 1000.0f),ConvertLinear<unsigned,float>(0.0f, 1000.0f, 0, 32767));
SystemData<unsigned> aoAPCOverride(8028, "APC override setpoint","DeviceNet",0,255,0,0,8,28,0,8,true);
SystemData<unsigned> aoAPCControlMode(8029, "APC control mode","DeviceNet",0,255,0,0,8,29,0,8,true);
SystemData<float> aoEtOHFlowSetpoint(8030, "alcohol flowrate setpoint","DeviceNet",0,700,0,0,8,30,0,16,true,"sccm",ConvertLinear<float,unsigned>(0, 24576, 0.0f, 729.3f),ConvertLinear<unsigned,float>(0.0f, 729.3f, 0, 24576));
SystemData<float> aoHFFlowSetpoint(8032, "HF flowrate setpoint","DeviceNet",0,2000,0,0,8,32,0,16,true,"sccm",ConvertLinear<float,unsigned>(0, 24576, 0.0f, 2000.0f),ConvertLinear<unsigned,float>(0.0f, 2000.0f, 0, 24576));
SystemData<float> aoPurgeN2FlowSetpoint(8034, "purge N2 flowrate setpoint","DeviceNet",0,5000,0,0,8,34,0,16,true,"sccm",ConvertLinear<float,unsigned>(0, 24576, 0.0f, 5000.0f),ConvertLinear<unsigned,float>(0.0f, 5000.0f, 0, 24576));
SystemData<float> aoN2FlowSetpoint(8036, "N2 flowrate setpoint","DeviceNet",0,5000,0,0,8,36,0,16,true,"sccm",ConvertLinear<float,unsigned>(0, 24576, 0.0f, 5000.0f),ConvertLinear<unsigned,float>(0.0f, 5000.0f, 0, 24576));
SystemData<float> aoAPCPosition(8038, "APC position setpoint","DeviceNet",0,7000,1,0,8,38,0,16,true,"%",ConvertLinear<float,unsigned>(0, 32767, 0.0f, 100.0f),ConvertLinear<unsigned,float>(0.0f, 100.0f, 0, 32767));




//system defined
SystemData<unsigned> TotalSteps(100011, "total recipe steps", "System", 0, 1000);
SystemData<unsigned> CurrentStep(100012, "current recipe step", "System", 0, 1000);
SystemData<std::string> RecipeName(100013, "recipe name", "System");
SystemData<unsigned> RecipeTotalTime(100014, "recipe total time", "System", 0, 10000, 0, "s");
SystemData<unsigned> RecipeElapseTime(100015, "recipe elapsed time", "System", 0, 10000, 0, "s");
SystemData<unsigned> CurrentStepTime(100016, "current step time", "System", 0, 3000, 0, "s");
SystemData<unsigned> StepElapseTime(100017, "step elapsed time", "System", 0, 3000, 0, "s");

SystemData<unsigned> LoadUnloadState(100018, "load/unload state", "System", 0, 1);
SystemData<unsigned> LoadUnloadSkip(100019, "load/unload skip flag", "System", 0, 1);
SystemData<unsigned> LoadUnloadOK(100020, "load/unload ok", "System", 0, 1);
SystemData<unsigned> WaferProcessedCount(100021, "processed wafer count", "System", 0, 1000);
SystemData<std::string> WaferBatchID(100022, "wafer batch id", "System");
SystemData<unsigned> WaferBatchCount(100023, "wafer batch count", "System", 0, 100000);
SystemData<unsigned> WaferTotalCount(100024, "wafer total count", "System", 0, 100000);

SystemData<std::string> LeakCheckResult(100030, "leak check result", "System");
SystemData<float> LeakRate(100031, "leak rate", "System", 0, 1000, 3, "torr/min");

SystemData<std::string> Slot1WaferID(100040, "wafer id of slot1", "System");
SystemData<std::string> Slot2WaferID(100041, "wafer id of slot2", "System");
SystemData<std::string> Slot3WaferID(100042, "wafer id of slot3", "System");
SystemData<unsigned> Slot1WaferState(100043, "wafer state of slot1", "System", 0, 5);
SystemData<unsigned> Slot2WaferState(100044, "wafer state of slot2", "System", 0, 5);
SystemData<unsigned> Slot3WaferState(100045, "wafer state of slot3", "System", 0, 5);
SystemData<std::string> ForkWaferID(100046, "wafer id of fork", "System");
SystemData<unsigned> ForkWaferState(100047, "wafer state of fork", "System", 0, 5);

SystemData<unsigned> ProcUnitState(100050, "process unit state", "System", 0, 2);
SystemData<unsigned> ProcUnitRetryable(100051, "retryable flag of process unit", "System", 0, 1);
SystemData<unsigned> ProcUnitCommand(100052, "current command of process unit", "System", 0, 10000);
SystemData<unsigned> ProcUnitParam1(100053, "parameter 1 of process unit", "System", 0, 10000);
SystemData<unsigned> ProcUnitParam2(100054, "parameter 2 of process unit", "System", 0, 10000);
SystemData<std::string> ProcUnitStepName(100055, "current step name of process unit", "System");

SystemData<unsigned> ProcChamberDirty(100060, "dirty flag of process chamber", "System", 0, 1);
SystemData<unsigned> ExpChamberDirty(100061, "dirty flag of expansion chamber", "System", 0, 1);

}
