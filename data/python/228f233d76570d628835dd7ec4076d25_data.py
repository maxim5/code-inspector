"""
Data used by ROPCheck
"""

"""
    DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
                    Version 2, December 2004

 Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>

 Everyone is permitted to copy and distribute verbatim or modified
 copies of this license document, and changing it is allowed as long
 as the name is changed.

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

  0. You just DO WHAT THE FUCK YOU WANT TO. 

  1. This program is free software. It comes without any warranty, to
     the extent permitted by applicable law. You can redistribute it
     and/or modify it under the terms of the Do What The Fuck You Want
     To Public License, Version 2, as published by Sam Hocevar. See
     http://sam.zoy.org/wtfpl/COPYING for more details. 
"""

# (c) 2010 Bjoern Doebel <doebel@tudos.org>
#     economic rights: Technische Universitaet Dresden (Germany)

from bdutil import Colors

class Section:
    """
    Represents a binary section
    """
    def __init__(self, name, start, size):
        self.__name = name
        self.__start = int(start, 16)
        self.__size = int(size, 16)

    def dump(self):
        """
        Dump section
        """
        print "Section %s%s%s @ %08x - %08x" % (Colors.Green, self.__name,
                                                Colors.Reset, self.__start,
                                                self.__start + self.__size)

    @property
    def name(self):
        """Section name"""
        return self.__name

    @property
    def start(self):
        """Section start address"""
        return self.__start

    @property
    def size(self):
        """Section size"""
        return self.__size


    @property
    def end(self):
        """Last address within section"""
        return self.__start + self.__size -1
