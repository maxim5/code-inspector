# stream.py
#
# Copyright (C) 2010 Alexey Naydenov <alexey.naydenovREMOVETHIS@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""@package tracexplorer.stream
Utility objects for working with trace streams.
"""

import struct

## message alignment in a trace binary stream
DEFAULT_ALIGNMENT = 4

class InputStreamError(Exception):
    """Base exception for all input stream errors."""
    pass

class EndOfStreamError(InputStreamError):
    """Out of data exception, used to terminate with block."""
    pass

class InputStream:
    """Utility object for working with input stream.

    It wraps binary stream and allows extracting values from it.
    """

    def __init__(self, stream):
        """Contstruct a wrapper object for a binary stream.

        @param stream banary io stream.
        """

        ## reference to the binary stream
        self.stream = stream
        ## message alignment in the stream
        self.alignment = DEFAULT_ALIGNMENT
        ## format string for struct module that defines endianness
        self.endian_format = '<'

    def __enter__(self):
        return self

    def __exit__(self, type_, value, traceback):
        self.stream.close()
        return True
        

    def get(self, varobj, count=1, is_aligned=False):
        """Extract a list of values from the binary stream.

        @param varobj object that describes data type, 
        see traceviewer.parser.variable.VarType;
        @param count number of values to read;
        @param is_aligned should stream position be aligned after
        retrieving values.
        @returns list of values.
        """
        read_bytes = self.stream.read(varobj.size * count)
        if read_bytes == b'':
            raise EndOfStreamError()
        result = list(struct.unpack(
                self.endian_format + str(count) + varobj.format, read_bytes))
        if is_aligned:
            reminder = (varobj.size * count) % self.alignment
            if reminder > 0:
                self.skip(self.alignment - reminder)
        return result

    def skip(self, count):
        """Advance stream read position.

        @param count advance stream position by count bytes.
        @returns nothing.
        """
        if count > 0:
            self.stream.read(count)


    @property
    def position(self):
        """Return current stream position."""
        return self.stream.tell()
