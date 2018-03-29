# Copyright (C) 2000-2010 The OpenRPG Project
#
#        owner@madmathlabs.com
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# --
#
# File: Alternity.py
# Maintainer (Traipse): Tyler Starke
# Version:
#   $Id: Alternity.py,v .1 JEC (cchriss@thecastle.com)
#
# Description: Alternity die roller based on Posterboy's D20 Dieroller
#
# Changelog:
#
# v.1 original release JEC
#
# Traipse Release: 
# The changes made in the Traipe release are intended to create a more direct connection
# between the source and the intrepretor. IF, ELIF statements have been replaced with dictionaries,
# unused objects have been replace with re-usable objects, and the code has been condensed.
#
# SEG: JAN 24 2010 - v.1.4 O'Flux Release:
# Edits & Additions: fixed a few minor bugs; Damage roll & Display Issues.
# Added Secondary Damage Calculation and Display. Fix all errors.
# Added proper results for Critcal Successes with failure ==> final Result Ordinary Success
# Removed reduntent Method to make parent class true with all others working as child.
# Made all special output same colour codes font size. Cleaned out old commented lines.
# Tested for Traipse on Win 7
#
#  Skill Check Example:
#  [1d20.sk(12,-2)]
#  OUTPUT Example:
#  => [6,-3] = (3) AMAZING Success
#
#  Pistol, Laser; 0 step -- Attack Example:
#  [1d20.at(12,0,(1d4+1,"w"),(1d6+1,"w"),(1d4,"m"))]
#  OUTPUT Example:
#  => [1,0] = (1) CRITICAL SUCCESS AMAZING HIT
#  ===> Damage [4] = (4) mortal ======> Secondary Damage (2) stun / (2) wound
#
#  Action Check Example:
#  [1d20.ac(14,-1)]
#  OUTPUT Example:
#   => ACTION CHECK : [18,-3] = (15) Marginal failure
#   -1 Step make up bonus next Action Check
#
#

import re

from std import std
from time import time, clock
from CoreEngine.dieroller.base import di, die_base, die_rollers

##from CoreEngine.tools.CoreEngine_log import debug


__version__ = "$Id: alternity.py,v 0.1 2003/01/02 12:00:00 cchriss Exp $"

# Alternity stands for "Alternity system" 20 sided die plus mods

class alternity(std):
    name = "alternity" # ADDED by SEG Nov 2009 ***
    
    def __init__(self,source=[]):
        std.__init__(self,source)

    # these methods return new die objects for specific options
    def sk(self,score,mod):
      return sk(self,score,mod)

    def at(self,score,mod,dmgo,dmgg,dmga):
      return at(self,score,mod,dmgo,dmgg,dmga)

    def ac(self,score,mod):
        return ac(self,score,mod)

die_rollers.register(alternity)

class sk(std):
    def __init__(self,source=[],sc="10/5/2",mod=0):
        std.__init__(self,source)
        m = re.match( r"\d+", str(sc) )
        self.score = int( m.group(0) )
        self.mod = mod

    def getMod(self,mod=0):
        m=0
        mods = { -4: -di(12), -3: -di(8), -2:  -di(6), -1: -di(4), 1: di(4),
                2: di(6), 3: di(8), 4: di(12), 5: di(20)} # SEG fix 1: di(4) #
        if mod in mods.keys(): m = mods[mod].value
        elif mod <= -5: m=-di(20).value
        elif mod == 6:  m=di(20).value + di(20).value
        elif mod >= 7:  m=di(20).value + di(20).value + di(20).value
        return m

    def getRolLStr(self):
        myStr = "[" + str(self.data[0])
        self.d20 = self.sum()
        self.amod = self.getMod(self.mod)

##        varN = "self.amod"
##        debug(varN)
##        debug(self.amod)  ## seg added debug output
        
        self.dieRoll = self.d20 + self.amod
        for a in self.data[1:]:
            myStr += ","
            myStr += str(a)
        myStr += "," + str(self.amod) + "] = (" + str(self.dieRoll) + ")"
        if ( self.dieRoll <= self.score / 4 ): self.success = 'A'
        elif ( self.dieRoll <= self.score / 2 ): self.success = 'G'
        elif ( self.dieRoll <= self.score ): self.success = 'O'
        else: self.success = 'F'
        if ( self.d20 == 20 ): self.success = 'CF'
        return myStr

    def __str__(self):
        myStr = self.getRolLStr()

##        varN = "myStr"
##        debug(varN)
##        debug(myStr)  ## seg added debug output

        successes = {'CS': " <b><font size=2 color='#8D38C9'>CRITICAL SUCCESS</font></b>",
                    'CF': " <b><font size=2 color='#151B54'>CRITICAL FAILURE</font></b>",
                    'A': " <b><font size=2 color='#E42217'>AMAZING Success</font></b>",
                    'G': " <b><font size=2 color='#306EFF'>Good Success</font></b>",
                    'O': " <b><font size=2 color='#52D017'>Ordinary Success</font></b>",
                    'F': " <b><font size=2 color='#41627E'>failure</font></b>"}

        if ( self.d20 == 1 ):  myStr += successes['CS'] # SEG Dec 19 2009
        myStr += successes[self.success]
        if ( self.d20 == 1 ) and (self.success == 'F') :
            myStr += "          final result ==> "
            myStr += successes['O'] # SEG JAN 23 2010
        return myStr

class at(sk):
    ## Traipse Usage: The source I received had the damage rolls like this 1d6s, with the damage type a
    ## letter that could be sliced from the roll. However, the roll is parsed before the letter can be
    ## sliced from it, and with the letter attached it created an error.
    ##
    ## The Traipse method puts the damage type and the damage roll into a Tuple, ie (1d6, 's').
    ## When using this method you must include single or double quoutes around the damage type or the
    ## software will treat it as an object.

    def __init__(self,source=[],sc=10, mod=0, dmgo="(1d6, 's')",dmgg="(1d6, 'w')",dmga="(1d6, 'm')"):
        sk.__init__(self,source,sc,mod)
        self.dmgo = dmgo
        self.dmgg = dmgg
        self.dmga = dmga

    def getdmg(self,dmgroll):
        astr = "<b>===></b> Damage "
        droll = str(dmgroll[0])
        xyz = droll.split('(')
        secD = (int(xyz[1][:-1])/2)   ## SEG* Calculate Secondary Damage

##        varN = "secD"
##        debug(varN)
##        debug(secD)  ## seg added debug output

        dtype = dmgroll[1]
        astr += droll
        if dtype=="s": astr += " <b><font size=2 color='#52D017'>stun</font></b><BR>"
        elif dtype=="w":
            astr += " <b><font size=2 color='#C11B17'>wound</font></b>"+" <b>======></b> Secondary Damage ("+str(secD) \
                    +") <b><font size=2 color='#52D017'>stun</font></b><BR>"  # SEG* Display Secondary Damage
        elif dtype=="m":
            astr += " <b><font size=2 color='#FF0000'>mortal</font></b>"+" <b>======></b> Secondary Damage ("+str(secD) \
                    +") <b><font size=2 color='#52D017'>stun</font></b>"+" <b>/</b> ("+str(secD)+") <b><font size=2 color='#C11B17'>wound</font></b><BR>"  # SEG* Display Secondary Damage
        return astr

    def __str__(self):
        myStr = self.getRolLStr()

##        varN = "myStr"
##        debug(varN)
##        debug(myStr)  ## seg added debug output
        
        successes = {'CS': " <b><font size=2 color='#8D38C9'>CRITICAL SUCCESS</font></b>",
                    'CF': " <b><font size=2 color='#151B54'>CRITICAL FAILURE</font></b>",
                    'A': " <b><font size=2 color='#E42217'>AMAZING HIT</font></b><BR> ",
                    'G': " <b><font size=2 color='#306EFF'>Good HIT</font></b><BR> ",
                    'O': " <b><font size=2 color='#52D017'>Ordinary HIT</font></b><BR> ",
                    'F': " <b><font size=2 color='#41627E'>miss</font></b>"}
        if ( self.d20 == 1 ):
            myStr += successes['CS'] # SEG Dec 19 2009

        if ( self.d20 == 1 ) and (self.success == 'F') :
            myStr += successes['F'] # SEG JAN 23 2010
            myStr += "          final result ==> "
            self.success = 'O'

        myStr += successes[self.success]
        if self.success == 'A': myStr += self.getdmg(self.dmga)
        elif self.success == 'G': myStr += self.getdmg(self.dmgg)
        elif self.success == 'O': myStr += self.getdmg(self.dmgo)
        return myStr

class ac(sk):
    def __init__(self,source=[],sc=10,mod=0):
        sk.__init__(self,source,sc,mod)

    def __str__(self):
        myStr = self.getRolLStr()

##        varN = "myStr"
##        debug(varN)
##        debug(myStr)  ## seg added debug output
        
        myStr = " <b><font color='#E42217'>ACTION CHECK : </font></b>"+myStr
        successes = {'CS': " <b><font size=2 color='#8D38C9'>CRITICAL SUCCESS</font></b>",
                    'CF': " <b><font size=2 color='#151B54'>CRITICAL FAILURE</font></b><BR> -2 Step make up bonus next Action Check",
                    'A': " <b><font size=2 color='#E42217'>AMAZING Success</font></b>",
                    'G': " <b><font size=2 color='#306EFF'>Good Success</font></b>",
                    'O': " <b><font size=2 color='#52D017'>Ordinary Success</font></b>",
                    'F': " <b><font size=2 color='#41627E'>Marginal failure</font></b>"}
        if ( self.d20 == 1 ):  myStr += successes['CS'] # SEG Dec 19 2009
        myStr += successes[self.success]
        if ( self.d20 == 1 ) and (self.success == 'F') :
            myStr += " final result ==> "
            myStr += successes['O'] # SEG JAN 23 2010
        if ( self.d20 != 1 ) and (self.success == 'F') :
            myStr += "<BR> -1 Step make up bonus next Action Check"
            
        return myStr












        

