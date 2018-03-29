{*
===============================================================================
The original notice of the softfloat package is shown below. The conversion
to pascal was done by Carl Eric Codere in 2002 (ccodere@ieee.org).
===============================================================================

This C source file is part of the SoftFloat IEC/IEEE Floating-Point
Arithmetic Package, Release 2a.

Written by John R. Hauser.  This work was made possible in part by the
International Computer Science Institute, located at Suite 600, 1947 Center
Street, Berkeley, California 94704.  Funding was partially provided by the
National Science Foundation under grant MIP-9311980.  The original version
of this code was written as part of a project to build a fixed-point vector
processor in collaboration with the University of California at Berkeley,
overseen by Profs. Nelson Morgan and John Wawrzynek.  More information
is available through the Web page
`http://HTTP.CS.Berkeley.EDU/~jhauser/arithmetic/SoftFloat.html'.

THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort
has been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT
TIMES RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO
PERSONS AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ANY
AND ALL LOSSES, COSTS, OR OTHER PROBLEMS ARISING FROM ITS USE.

Derivative works are acceptable, even for commercial purposes, so long as
(1) they include prominent notice that the work is derivative, and (2) they
include prominent notice akin to these four paragraphs for those parts of
this code that are retained.

===============================================================================

The float80 and float128 part is translated from the softfloat package
by Florian Klaempfl and contained the following copyright notice

The code might contain some duplicate stuff because the floatx80/float128 port was
done based on the 64 bit enabled softfloat code.

===============================================================================

This C source file is part of the SoftFloat IEC/IEEE Floating-point Arithmetic
Package, Release 2b.

Written by John R. Hauser.  This work was made possible in part by the
International Computer Science Institute, located at Suite 600, 1947 Center
Street, Berkeley, California 94704.  Funding was partially provided by the
National Science Foundation under grant MIP-9311980.  The original version
of this code was written as part of a project to build a fixed-point vector
processor in collaboration with the University of California at Berkeley,
overseen by Profs. Nelson Morgan and John Wawrzynek.  More information
is available through the Web page `http://www.cs.berkeley.edu/~jhauser/
arithmetic/SoftFloat.html'.

THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort has
been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT TIMES
RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO PERSONS
AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ALL LOSSES,
COSTS, OR OTHER PROBLEMS THEY INCUR DUE TO THE SOFTWARE, AND WHO FURTHERMORE
EFFECTIVELY INDEMNIFY JOHN HAUSER AND THE INTERNATIONAL COMPUTER SCIENCE
INSTITUTE (possibly via similar legal warning) AGAINST ALL LOSSES, COSTS, OR
OTHER PROBLEMS INCURRED BY THEIR CUSTOMERS AND CLIENTS DUE TO THE SOFTWARE.

Derivative works are acceptable, even for commercial purposes, so long as
(1) the source code for the derivative work includes prominent notice that
the work is derivative, and (2) the source code includes prominent notice with
these four paragraphs for those parts of this code that are retained.


===============================================================================
*}

{ $define FPC_SOFTFLOAT_FLOATX80}
{ $define FPC_SOFTFLOAT_FLOAT128}

{ the softfpu unit can be also embedded directly into the system unit }

{$if not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}

{$mode objfpc}
unit softfpu;

{ Overflow checking must be disabled,
  since some operations expect overflow!
}
{$Q-}
{$goto on}

interface
{$endif not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}

{$if not(defined(fpc_softfpu_implementation))}
{
-------------------------------------------------------------------------------
Software IEC/IEEE floating-point types.
-------------------------------------------------------------------------------
}
TYPE
  float32 = longword;
  { we use here a record in the function header because
    the record allows bitwise conversion to single }
  float32rec = record
    float32 : float32;
  end;

  flag = byte;
  uint8 = byte;
  int8 = shortint;
  uint16 = word;
  int16 = smallint;
  uint32 = longword;
  int32 = longint;

  bits8 = byte;
  sbits8 = shortint;
  bits16 = word;
  sbits16 = smallint;
  sbits32 = longint;
  bits32 = longword;
{$ifndef fpc}
  qword = int64;
{$endif}
  { now part of the system unit
  uint64 = qword;
  }
  bits64 = qword;
  sbits64 = int64;

{$ifdef ENDIAN_LITTLE}
  float64 = packed record
    low: bits32;
    high: bits32;
  end;

  int64rec = packed record
    low: bits32;
    high: bits32;
  end;

  floatx80 = packed record
    low : qword;
    high : word;
  end;

  float128 = packed record
    low : qword;
    high : qword;
  end;
{$else}
  float64 = packed record
    high,low : bits32;
  end;

  int64rec = packed record
    high,low : bits32;
  end;

  floatx80 = packed record
    high : word;
    low : qword;
  end;

  float128 = packed record
    high : qword;
    low : qword;
  end;
{$endif}

{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_lt(a: float64;b: float64): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is less than
or equal to the corresponding value `b', and 0 otherwise.  The comparison
is performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_le(a: float64;b: float64): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the double-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_eq(a: float64;b: float64): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the square root of the double-precision floating-point value `a'.
The operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Procedure float64_sqrt( a: float64; var out: float64 ); compilerproc;
{*
-------------------------------------------------------------------------------
Returns the remainder of the double-precision floating-point value `a'
with respect to the corresponding value `b'.  The operation is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_rem(a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of dividing the double-precision floating-point value `a'
by the corresponding value `b'.  The operation is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_div(a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of multiplying the double-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_mul( a: float64; b:float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of subtracting the double-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_sub(a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of adding the double-precision floating-point values `a'
and `b'.  The operation is performed according to the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_add( a: float64; b : float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Rounds the double-precision floating-point value `a' to an integer,
and returns the result as a double-precision floating-point value.  The
operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_round_to_int(a: float64) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the single-precision floating-point format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float64_to_float32(a: float64) : float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic, except that the conversion is always rounded toward zero.
If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
the conversion overflows, the largest integer with the same sign as `a' is
returned.
-------------------------------------------------------------------------------
*}
Function float64_to_int32_round_to_zero(a: float64 ): int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the double-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic---which means in particular that the conversion is rounded
according to the current rounding mode.  If `a' is a NaN, the largest
positive integer is returned.  Otherwise, if the conversion overflows, the
largest integer with the same sign as `a' is returned.
-------------------------------------------------------------------------------
*}
Function float64_to_int32(a: float64): int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_lt( a:float32rec ; b : float32rec): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is less than
or equal to the corresponding value `b', and 0 otherwise.  The comparison
is performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_le( a: float32rec; b : float32rec ):flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is equal to
the corresponding value `b', and 0 otherwise.  The comparison is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_eq( a:float32rec; b:float32rec): flag; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the square root of the single-precision floating-point value `a'.
The operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_sqrt(a: float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the remainder of the single-precision floating-point value `a'
with respect to the corresponding value `b'.  The operation is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_rem(a: float32rec; b: float32rec ):float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of dividing the single-precision floating-point value `a'
by the corresponding value `b'.  The operation is performed according to the
IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_div(a: float32rec;b: float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of multiplying the single-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_mul(a: float32rec; b: float32rec ) : float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of subtracting the single-precision floating-point values
`a' and `b'.  The operation is performed according to the IEC/IEEE Standard
for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_sub( a: float32rec ; b:float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of adding the single-precision floating-point values `a'
and `b'.  The operation is performed according to the IEC/IEEE Standard for
Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_add( a: float32rec; b:float32rec ): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Rounds the single-precision floating-point value `a' to an integer,
and returns the result as a single-precision floating-point value.  The
operation is performed according to the IEC/IEEE Standard for Binary
Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_round_to_int( a: float32rec): float32rec; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the double-precision floating-point format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic.
-------------------------------------------------------------------------------
*}
Function float32_to_float64( a : float32rec) : Float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic, except that the conversion is always rounded toward zero.
If `a' is a NaN, the largest positive integer is returned.  Otherwise, if
the conversion overflows, the largest integer with the same sign as `a' is
returned.
-------------------------------------------------------------------------------
*}
Function float32_to_int32_round_to_zero( a: Float32rec ): int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point value
`a' to the 32-bit two's complement integer format.  The conversion is
performed according to the IEC/IEEE Standard for Binary Floating-Point
Arithmetic---which means in particular that the conversion is rounded
according to the current rounding mode.  If `a' is a NaN, the largest
positive integer is returned.  Otherwise, if the conversion overflows, the
largest integer with the same sign as `a' is returned.
-------------------------------------------------------------------------------
*}
Function float32_to_int32( a : float32rec) : int32; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the 32-bit two's complement integer `a' to
the double-precision floating-point format.  The conversion is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function int32_to_float64( a: int32) : float64; compilerproc;
{*
-------------------------------------------------------------------------------
Returns the result of converting the 32-bit two's complement integer `a' to
the single-precision floating-point format.  The conversion is performed
according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
-------------------------------------------------------------------------------
*}
Function int32_to_float32( a: int32): float32rec; compilerproc;

{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the double-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
Function int64_to_float64( a: int64 ): float64; compilerproc;

{*----------------------------------------------------------------------------
| Returns the result of converting the 64-bit two's complement integer `a'
| to the single-precision floating-point format.  The conversion is performed
| according to the IEC/IEEE Standard for Binary Floating-Point Arithmetic.
*----------------------------------------------------------------------------*}
Function int64_to_float32( a: int64 ): float32rec; compilerproc;


{$ifdef FPC_SOFTFLOAT_FLOAT128}
function float128_is_nan( a : float128): flag;
function float128_is_signaling_nan( a : float128): flag;
function float128_to_int32(a: float128): int32;
function float128_to_int32_round_to_zero(a: float128): int32;
function float128_to_int64(a: float128): int64;
function float128_to_int64_round_to_zero(a: float128): int64;
function float128_to_float32(a: float128): float32;
function float128_to_float64(a: float128): float64;
function float64_to_float128( a : float64) : float128;
{$ifdef FPC_SOFTFLOAT_FLOAT80}
function float128_to_floatx80(a: float128): floatx80;
{$endif FPC_SOFTFLOAT_FLOAT80}
function float128_round_to_int(a: float128): float128;
function float128_add(a: float128; b: float128): float128;
function float128_sub(a: float128; b: float128): float128;
function float128_mul(a: float128; b: float128): float128;
function float128_div(a: float128; b: float128): float128;
function float128_rem(a: float128; b: float128): float128;
function float128_sqrt(a: float128): float128;
function float128_eq(a: float128; b: float128): flag;
function float128_le(a: float128; b: float128): flag;
function float128_lt(a: float128; b: float128): flag;
function float128_eq_signaling(a: float128; b: float128): flag;
function float128_le_quiet(a: float128; b: float128): flag;
function float128_lt_quiet(a: float128; b: float128): flag;
{$endif FPC_SOFTFLOAT_FLOAT128}

CONST
{-------------------------------------------------------------------------------
Software IEC/IEEE floating-point underflow tininess-detection mode.
-------------------------------------------------------------------------------
*}
    float_tininess_after_rounding  = 0;
    float_tininess_before_rounding = 1;

{*
-------------------------------------------------------------------------------
Underflow tininess-detection mode, statically initialized to default value.
(The declaration in `softfloat.h' must match the `int8' type here.)
-------------------------------------------------------------------------------
*}

const float_detect_tininess: int8 = float_tininess_after_rounding;

{$endif  not(defined(fpc_softfpu_implementation))}

{$if not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}
implementation
{$endif not(defined(fpc_softfpu_interface)) and not(defined(fpc_softfpu_implementation))}


{$if not(defined(fpc_softfpu_interface))}
(*****************************************************************************)
(*----------------------------------------------------------------------------*)
(* Primitive arithmetic functions, including multi-word arithmetic, and       *)
(* division and square root approximations.  (Can be specialized to target if *)
(* desired.)                                                                  *)
(* ---------------------------------------------------------------------------*)
(*****************************************************************************)


{*----------------------------------------------------------------------------
| Takes a 64-bit fixed-point value `absZ' with binary point between bits 6
| and 7, and returns the properly rounded 32-bit integer corresponding to the
| input.  If `zSign' is 1, the input is negated before being converted to an
| integer.  Bit 63 of `absZ' must be zero.  Ordinarily, the fixed-point input
| is simply rounded to an integer, with the inexact exception raised if the
| input cannot be represented exactly as an integer.  However, if the fixed-
| point input is too large, the invalid exception is raised and the largest
| positive or negative integer is returned.
*----------------------------------------------------------------------------*}

function roundAndPackInt32( zSign: flag; absZ : bits64): int32;
var
    roundingMode: int8;
    roundNearestEven: flag;
    roundIncrement, roundBits: int8;
    z: int32;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := ord( roundingMode = float_round_nearest_even );
    roundIncrement := $40;
    if ( roundNearestEven=0 ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            roundIncrement := 0;
        end
        else begin
            roundIncrement := $7F;
            if ( zSign<>0 ) then
            begin
                if ( roundingMode = float_round_up ) then
                  roundIncrement := 0;
            end
            else begin
                if ( roundingMode = float_round_down ) then
                  roundIncrement := 0;
            end;
        end;
    end;
    roundBits := absZ and $7F;
    absZ := ( absZ + roundIncrement ) shr 7;
    absZ := absZ and not( ord( ( roundBits xor  $40 ) = 0 ) and roundNearestEven );
    z := absZ;
    if ( zSign<>0 ) then
      z := - z;
    if ( ( absZ shr 32 ) or ( z and ( ord( z < 0 ) xor  zSign ) ) )<>0 then
    begin
        float_raise( float_flag_invalid );
        if zSign<>0 then
          result:=sbits32($80000000)
        else
          result:=$7FFFFFFF;
        exit;
    end;
    if ( roundBits<>0 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    result:=z;
end;

{*----------------------------------------------------------------------------
| Takes the 128-bit fixed-point value formed by concatenating `absZ0' and
| `absZ1', with binary point between bits 63 and 64 (between the input words),
| and returns the properly rounded 64-bit integer corresponding to the input.
| If `zSign' is 1, the input is negated before being converted to an integer.
| Ordinarily, the fixed-point input is simply rounded to an integer, with
| the inexact exception raised if the input cannot be represented exactly as
| an integer.  However, if the fixed-point input is too large, the invalid
| exception is raised and the largest positive or negative integer is
| returned.
*----------------------------------------------------------------------------*}

function roundAndPackInt64( zSign: flag; absZ0: bits64; absZ1 : bits64): int64;
var
    roundingMode: int8;
    roundNearestEven, increment: flag;
    z: int64;
label
    overflow;
begin
    roundingMode := softfloat_rounding_mode;
    roundNearestEven := ord( roundingMode = float_round_nearest_even );
    increment := ord( sbits64(absZ1) < 0 );
    if ( roundNearestEven=0 ) then
    begin
        if ( roundingMode = float_round_to_zero ) then
        begin
            increment := 0;
        end
        else begin
            if ( zSign<>0 ) then
            begin
                increment := ord(( roundingMode = float_round_down ) and (absZ1<>0));
            end
            else begin
                increment := ord(( roundingMode = float_round_up ) and (absZ1<>0));
            end;
        end;
    end;
    if ( increment<>0 ) then
    begin
        inc(absZ0);
        if ( absZ0 = 0 ) then
          goto overflow;
        absZ0 := absZ0 and not( ord( bits64( absZ1 shl 1 ) = 0 ) and roundNearestEven );
    end;
    z := absZ0;
    if ( zSign<>0 ) then
      z := - z;
    if ( (z<>0) and (( ord( z < 0 ) xor  zSign )<>0) ) then
    begin
 overflow:
        float_raise( float_flag_invalid );
        if zSign<>0 then
          result:=int64($8000000000000000)
        else
          result:=int64($7FFFFFFFFFFFFFFF);
    end;
    if ( absZ1<>0 ) then
      softfloat_exception_flags := softfloat_exception_flags or float_flag_inexact;
    result:=z;
end;

{*
-------------------------------------------------------------------------------
Shifts `a' right by the number of bits given in `count'.  If any nonzero
bits are shifted off, they are ``jammed'' into the least significant bit of
the result by setting the least significant bit to 1.  The value of `count'
can be arbitrarily large; in particular, if `count' is greater than 32, the
result will be either 0 or 1, depending on whether `a' is zero or nonzero.
The result is stored in the location pointed to by `zPtr'.
-------------------------------------------------------------------------------
*}
Procedure shift32RightJamming( a: bits32 ; count: int16 ; VAR zPtr :bits32);
var
  z: Bits32;
Begin
    if ( count = 0 ) then
        z := a
   else
    if ( count < 32 ) then
    Begin
        z := ( a shr count ) or bits32( (( a shl ( ( - count ) AND 31 )) ) <> 0);
    End
   else
    Begin
        z := bits32( a <> 0 );
    End;
    zPtr := z;
End;

{*----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' right by the
| number of bits given in `count'.  Any bits shifted off are lost.  The value
| of `count' can be arbitrarily large; in particular, if `count' is greater
| than 128, the result will be 0.  The result is broken into two 64-bit pieces
| which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure shift128Right(a0: bits64; a1: bits64; count: int16; var z0Ptr: bits64; z1Ptr : bits64);
var
    z0, z1: bits64;
    negCount: int8;
begin
    negCount := ( - count ) and 63;

    if ( count = 0 ) then
    begin
        z1 := a1;
        z0 := a0;
    end
    else if ( count < 64 ) then
    begin
        z1 := ( a0 shl negCount ) or ( a1 shr count );
        z0 := a0 shr count;
    end
    else
    begin
    	  if ( count shl 64 )<>0 then
          z1 := a0 shr ( count and 63 )
        else
          z1 := 0;
        z0 := 0;
    end;
    z1Ptr := z1;
    z0Ptr := z0;
end;


{*----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' right by the
| number of bits given in `count'.  If any nonzero bits are shifted off, they
| are ``jammed'' into the least significant bit of the result by setting the
| least significant bit to 1.  The value of `count' can be arbitrarily large;
| in particular, if `count' is greater than 128, the result will be either
| 0 or 1, depending on whether the concatenation of `a0' and `a1' is zero or
| nonzero.  The result is broken into two 64-bit pieces which are stored at
| the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure shift128RightJamming(a0,a1 : bits64; count : int16; var z0Ptr, z1Ptr : bits64);
var
    z0,z1 : bits64;
    negCount : int8;
begin
    negCount := ( - count ) and 63;

    if ( count = 0 ) then begin
        z1 := a1;
        z0 := a0;
    end
    else if ( count < 64 ) then begin
        z1 := ( a0 shl negCount ) or ( a1 shr count ) or ord( ( a1 shl negCount ) <> 0 );
        z0 := a0>>count;
    end
    else begin
        if ( count = 64 ) then begin
            z1 := a0 or ord( a1 <> 0 );
        end
        else if ( count < 128 ) then begin
            z1 := ( a0 shr ( count and 63 ) ) or ord( ( ( a0 shl negCount ) or a1 ) <> 0 );
        end
        else begin
            z1 := ord( ( a0 or a1 ) <> 0 );
        end;
        z0 := 0;
    end;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' right by the
number of bits given in `count'.  Any bits shifted off are lost.  The value
of `count' can be arbitrarily large; in particular, if `count' is greater
than 64, the result will be 0.  The result is broken into two 32-bit pieces
which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shift64Right(
     a0 :bits32; a1: bits32; count:int16; VAR z0Ptr:bits32; VAR z1Ptr:bits32);
Var
  z0, z1: bits32;
  negCount : int8;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z1 := a1;
        z0 := a0;
    End
    else if ( count < 32 ) then
    Begin
        z1 := ( a0 shl negCount ) OR ( a1 shr count );
        z0 := a0 shr count;
    End
   else
    Begin
        if (count < 64) then
          z1 := ( a0 shr ( count AND 31 ) )
        else
          z1 := 0;
        z0 := 0;
    End;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' right by the
number of bits given in `count'.  If any nonzero bits are shifted off, they
are ``jammed'' into the least significant bit of the result by setting the
least significant bit to 1.  The value of `count' can be arbitrarily large;
in particular, if `count' is greater than 64, the result will be either 0
or 1, depending on whether the concatenation of `a0' and `a1' is zero or
nonzero.  The result is broken into two 32-bit pieces which are stored at
the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shift64RightJamming(
     a0:bits32; a1: bits32; count:int16; VAR Z0Ptr :bits32;VAR z1Ptr: bits32 );
VAR
    z0, z1 : bits32;
    negCount : int8;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z1 := a1;
        z0 := a0;
    End
   else
    if ( count < 32 ) then
    Begin
        z1 := ( a0 shl negCount ) OR ( a1 shr count ) OR bits32( ( a1 shl negCount ) <> 0 );
        z0 := a0 shr count;
    End
   else
    Begin
        if ( count = 32 ) then
        Begin
            z1 := a0 OR bits32( a1 <> 0 );
        End
       else
        if ( count < 64 ) Then
        Begin
            z1 := ( a0 shr ( count AND 31 ) ) OR bits32( ( ( a0 shl negCount ) OR a1 ) <> 0 );
        End
       else
        Begin
            z1 := bits32( ( a0 OR a1 ) <> 0 );
        End;
        z0 := 0;
    End;
    z1Ptr := z1;
    z0Ptr := z0;
End;


{*----------------------------------------------------------------------------
| Shifts `a' right by the number of bits given in `count'.  If any nonzero
| bits are shifted off, they are ``jammed'' into the least significant bit of
| the result by setting the least significant bit to 1.  The value of `count'
| can be arbitrarily large; in particular, if `count' is greater than 64, the
| result will be either 0 or 1, depending on whether `a' is zero or nonzero.
| The result is stored in the location pointed to by `zPtr'.
*----------------------------------------------------------------------------*}

procedure shift64RightJamming(a: bits64; count: int16; var zPtr : bits64);
var
    z: bits64;
begin
    if ( count = 0 ) then
    begin
        z := a;
    end
    else if ( count < 64 ) then
    begin
        z := ( a shr count ) or ord( ( a  shl ( ( - count ) and 63 ) ) <> 0 );
    end
    else
    begin
        z := ord( a <> 0 );
    end;
    zPtr := z;
end;



{*
-------------------------------------------------------------------------------
Shifts the 96-bit value formed by concatenating `a0', `a1', and `a2' right
by 32 _plus_ the number of bits given in `count'.  The shifted result is
at most 64 nonzero bits; these are broken into two 32-bit pieces which are
stored at the locations pointed to by `z0Ptr' and `z1Ptr'.  The bits shifted
off form a third 32-bit result as follows:  The _last_ bit shifted off is
the most-significant bit of the extra result, and the other 31 bits of the
extra result are all zero if and only if _all_but_the_last_ bits shifted off
were all zero.  This extra result is stored in the location pointed to by
`z2Ptr'.  The value of `count' can be arbitrarily large.
    (This routine makes more sense if `a0', `a1', and `a2' are considered
to form a fixed-point value with binary point between `a1' and `a2'.  This
fixed-point value is shifted right by the number of bits given in `count',
and the integer part of the result is returned at the locations pointed to
by `z0Ptr' and `z1Ptr'.  The fractional part of the result may be slightly
corrupted as described above, and is returned at the location pointed to by
`z2Ptr'.)
-------------------------------------------------------------------------------
}
Procedure
 shift64ExtraRightJamming(
     a0: bits32;
     a1: bits32;
     a2: bits32;
     count: int16;
     VAR z0Ptr: bits32;
     VAR z1Ptr: bits32;
     VAR z2Ptr: bits32
 );
Var
    z0, z1, z2: bits32;
    negCount : int8;
Begin
    negCount := ( - count ) AND 31;

    if ( count = 0 ) then
    Begin
        z2 := a2;
        z1 := a1;
        z0 := a0;
    End
   else
    Begin
        if ( count < 32 ) Then
        Begin
            z2 := a1 shl negCount;
            z1 := ( a0 shl negCount ) OR ( a1 shr count );
            z0 := a0 shr count;
        End
       else
        Begin
            if ( count = 32 ) then
            Begin
                z2 := a1;
                z1 := a0;
            End
           else
            Begin
                a2 := a2 or a1;
                if ( count < 64 ) then
                Begin
                    z2 := a0 shl negCount;
                    z1 := a0 shr ( count AND 31 );
                End
               else
                Begin
                    if count = 64 then
                       z2 := a0
                    else
                       z2 := bits32(a0 <> 0);
                    z1 := 0;
                End;
            End;
            z0 := 0;
        End;
        z2 := z2 or bits32( a2 <> 0 );
    End;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Shifts the 64-bit value formed by concatenating `a0' and `a1' left by the
number of bits given in `count'.  Any bits shifted off are lost.  The value
of `count' must be less than 32.  The result is broken into two 32-bit
pieces which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shortShift64Left(
     a0:bits32; a1:bits32; count:int16; VAR z0Ptr:bits32; VAR z1Ptr:bits32 );
Begin

    z1Ptr := a1 shl count;
    if count = 0 then
      z0Ptr := a0
    else
      z0Ptr := ( a0 shl count ) OR ( a1 shr ( ( - count ) AND 31 ) );
End;

{*
-------------------------------------------------------------------------------
Shifts the 96-bit value formed by concatenating `a0', `a1', and `a2' left
by the number of bits given in `count'.  Any bits shifted off are lost.
The value of `count' must be less than 32.  The result is broken into three
32-bit pieces which are stored at the locations pointed to by `z0Ptr',
`z1Ptr', and `z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 shortShift96Left(
     a0: bits32;
     a1: bits32;
     a2: bits32;
     count: int16;
     VAR z0Ptr: bits32;
     VAR z1Ptr: bits32;
     VAR z2Ptr: bits32
 );
Var
    z0, z1, z2: bits32;
    negCount: int8;
Begin
    z2 := a2 shl count;
    z1 := a1 shl count;
    z0 := a0 shl count;
    if ( 0 < count ) then
    Begin
        negCount := ( ( - count ) AND 31 );
        z1 := z1 or (a2 shr negCount);
        z0 := z0 or (a1 shr negCount);
    End;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Shifts the 128-bit value formed by concatenating `a0' and `a1' left by the
| number of bits given in `count'.  Any bits shifted off are lost.  The value
| of `count' must be less than 64.  The result is broken into two 64-bit
| pieces which are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure shortShift128Left(a0: bits64; a1: bits64; count: int16; var z0Ptr: bits64; z1Ptr : bits64);
begin
    z1Ptr := a1 shl count;
    if count=0 then
      z0Ptr:=a0
    else
      z0Ptr:=( a0 shl count ) or ( a1 shr ( ( - count ) and 63 ) );
end;

{*
-------------------------------------------------------------------------------
Adds the 64-bit value formed by concatenating `a0' and `a1' to the 64-bit
value formed by concatenating `b0' and `b1'.  Addition is modulo 2^64, so
any carry out is lost.  The result is broken into two 32-bit pieces which
are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 add64(
     a0:bits32; a1:bits32; b0:bits32; b1:bits32; VAR z0Ptr:bits32; VAR z1Ptr:bits32 );
Var
    z1: bits32;
Begin
    z1 := a1 + b1;
    z1Ptr := z1;
    z0Ptr := a0 + b0 + bits32( z1 < a1 );
End;

{*
-------------------------------------------------------------------------------
Adds the 96-bit value formed by concatenating `a0', `a1', and `a2' to the
96-bit value formed by concatenating `b0', `b1', and `b2'.  Addition is
modulo 2^96, so any carry out is lost.  The result is broken into three
32-bit pieces which are stored at the locations pointed to by `z0Ptr',
`z1Ptr', and `z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 add96(
     a0: bits32;
     a1: bits32;
     a2: bits32;
     b0: bits32;
     b1: bits32;
     b2: bits32;
     VAR z0Ptr: bits32;
     VAR z1Ptr: bits32;
     VAR z2Ptr: bits32
 );
var
    z0, z1, z2: bits32;
    carry0, carry1: int8;
Begin
    z2 := a2 + b2;
    carry1 := int8( z2 < a2 );
    z1 := a1 + b1;
    carry0 := int8( z1 < a1 );
    z0 := a0 + b0;
    z1 := z1 + carry1;
    z0 := z0 + bits32( z1 < carry1 );
    z0 := z0 + carry0;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Shifts the 192-bit value formed by concatenating `a0', `a1', and `a2' left
| by the number of bits given in `count'.  Any bits shifted off are lost.
| The value of `count' must be less than 64.  The result is broken into three
| 64-bit pieces which are stored at the locations pointed to by `z0Ptr',
| `z1Ptr', and `z2Ptr'.
*----------------------------------------------------------------------------*}

procedure shortShift192Left(a0,a1,a2 : bits64;count : int16;var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2 : bits64;
    negCount : int8;
begin
    z2 := a2 shl count;
    z1 := a1 shl count;
    z0 := a0 shl count;
    if ( 0 < count ) then
    begin
        negCount := ( ( - count ) and 63 );
        z1 := z1 or (a2 shr negCount);
        z0 := z0 or (a1 shr negCount);
    end;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*----------------------------------------------------------------------------
| Adds the 128-bit value formed by concatenating `a0' and `a1' to the 128-bit
| value formed by concatenating `b0' and `b1'.  Addition is modulo 2^128, so
| any carry out is lost.  The result is broken into two 64-bit pieces which
| are stored at the locations pointed to by `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure add128( a0, a1, b0, b1 : bits64; var z0Ptr, z1Ptr : bits64);inline;
var
    z1 : bits64;
begin
    z1 := a1 + b1;
    z1Ptr := z1;
    z0Ptr := a0 + b0 + ord( z1 < a1 );
end;

{*----------------------------------------------------------------------------
| Adds the 192-bit value formed by concatenating `a0', `a1', and `a2' to the
| 192-bit value formed by concatenating `b0', `b1', and `b2'.  Addition is
| modulo 2^192, so any carry out is lost.  The result is broken into three
| 64-bit pieces which are stored at the locations pointed to by `z0Ptr',
| `z1Ptr', and `z2Ptr'.
*----------------------------------------------------------------------------*}

procedure add192(a0,a1,a2,b0,b1,b2: bits64; var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2 : bits64;
    carry0, carry1 : int8;
begin
    z2 := a2 + b2;
    carry1 := ord( z2 < a2 );
    z1 := a1 + b1;
    carry0 := ord( z1 < a1 );
    z0 := a0 + b0;
    inc(z1, carry1);
    inc(z0, ord( z1 < carry1 ));
    inc(z0, carry0);
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*
-------------------------------------------------------------------------------
Subtracts the 64-bit value formed by concatenating `b0' and `b1' from the
64-bit value formed by concatenating `a0' and `a1'.  Subtraction is modulo
2^64, so any borrow out (carry out) is lost.  The result is broken into two
32-bit pieces which are stored at the locations pointed to by `z0Ptr' and
`z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 sub64(
     a0: bits32; a1 : bits32; b0 :bits32; b1: bits32; VAR z0Ptr:bits32; VAR z1Ptr: bits32 );
Begin
    z1Ptr := a1 - b1;
    z0Ptr := a0 - b0 - bits32( a1 < b1 );
End;

{*
-------------------------------------------------------------------------------
Subtracts the 96-bit value formed by concatenating `b0', `b1', and `b2' from
the 96-bit value formed by concatenating `a0', `a1', and `a2'.  Subtraction
is modulo 2^96, so any borrow out (carry out) is lost.  The result is broken
into three 32-bit pieces which are stored at the locations pointed to by
`z0Ptr', `z1Ptr', and `z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 sub96(
     a0:bits32;
     a1:bits32;
     a2:bits32;
     b0:bits32;
     b1:bits32;
     b2:bits32;
     VAR z0Ptr:bits32;
     VAR z1Ptr:bits32;
     VAR z2Ptr:bits32
 );
Var
    z0, z1, z2: bits32;
    borrow0, borrow1: int8;
Begin
    z2 := a2 - b2;
    borrow1 := int8( a2 < b2 );
    z1 := a1 - b1;
    borrow0 := int8( a1 < b1 );
    z0 := a0 - b0;
    z0 := z0 - bits32( z1 < borrow1 );
    z1 := z1 - borrow1;
    z0 := z0 -borrow0;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Subtracts the 128-bit value formed by concatenating `b0' and `b1' from the
| 128-bit value formed by concatenating `a0' and `a1'.  Subtraction is modulo
| 2^128, so any borrow out (carry out) is lost.  The result is broken into two
| 64-bit pieces which are stored at the locations pointed to by `z0Ptr' and
| `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure sub128( a0, a1, b0, b1 : bits64; var z0Ptr, z1Ptr : bits64);
begin
    z1Ptr := a1 - b1;
    z0Ptr := a0 - b0 - ord( a1 < b1 );
end;


{*----------------------------------------------------------------------------
| Subtracts the 192-bit value formed by concatenating `b0', `b1', and `b2'
| from the 192-bit value formed by concatenating `a0', `a1', and `a2'.
| Subtraction is modulo 2^192, so any borrow out (carry out) is lost.  The
| result is broken into three 64-bit pieces which are stored at the locations
| pointed to by `z0Ptr', `z1Ptr', and `z2Ptr'.
*----------------------------------------------------------------------------*}

procedure sub192(a0,a1,a2,b0,b1,b2: bits64; var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2 : bits64;
    borrow0, borrow1 : int8;
begin
    z2 := a2 - b2;
    borrow1 := ord( a2 < b2 );
    z1 := a1 - b1;
    borrow0 := ord( a1 < b1 );
    z0 := a0 - b0;
    dec(z0, ord( z1 < borrow1 ));
    dec(z1, borrow1);
    dec(z0, borrow0);
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*
-------------------------------------------------------------------------------
Multiplies `a' by `b' to obtain a 64-bit product.  The product is broken
into two 32-bit pieces which are stored at the locations pointed to by
`z0Ptr' and `z1Ptr'.
-------------------------------------------------------------------------------
*}
Procedure mul32To64( a:bits32; b:bits32; VAR z0Ptr: bits32; VAR z1Ptr
:bits32 );
Var
    aHigh, aLow, bHigh, bLow: bits16;
    z0, zMiddleA, zMiddleB, z1: bits32;
Begin
    aLow := a and $ffff;
    aHigh := a shr 16;
    bLow := b and $ffff;
    bHigh := b shr 16;
    z1 := ( bits32( aLow) ) * bLow;
    zMiddleA := ( bits32 (aLow) ) * bHigh;
    zMiddleB := ( bits32 (aHigh) ) * bLow;
    z0 := ( bits32 (aHigh) ) * bHigh;
    zMiddleA := zMiddleA + zMiddleB;
    z0 := z0 + ( ( bits32 ( zMiddleA < zMiddleB ) ) shl 16 ) + ( zMiddleA shr 16 );
    zMiddleA := zmiddleA shl 16;
    z1 := z1 + zMiddleA;
    z0 := z0 + bits32( z1 < zMiddleA );
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Multiplies the 64-bit value formed by concatenating `a0' and `a1' by `b'
to obtain a 96-bit product.  The product is broken into three 32-bit pieces
which are stored at the locations pointed to by `z0Ptr', `z1Ptr', and
`z2Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 mul64By32To96(
     a0:bits32;
     a1:bits32;
     b:bits32;
     VAR z0Ptr:bits32;
     VAR z1Ptr:bits32;
     VAR z2Ptr:bits32
 );
Var
    z0, z1, z2, more1: bits32;
Begin
    mul32To64( a1, b, z1, z2 );
    mul32To64( a0, b, z0, more1 );
    add64( z0, more1, 0, z1, z0, z1 );
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*
-------------------------------------------------------------------------------
Multiplies the 64-bit value formed by concatenating `a0' and `a1' to the
64-bit value formed by concatenating `b0' and `b1' to obtain a 128-bit
product.  The product is broken into four 32-bit pieces which are stored at
the locations pointed to by `z0Ptr', `z1Ptr', `z2Ptr', and `z3Ptr'.
-------------------------------------------------------------------------------
*}
Procedure
 mul64To128(
     a0:bits32;
     a1:bits32;
     b0:bits32;
     b1:bits32;
     VAR z0Ptr:bits32;
     VAR z1Ptr:bits32;
     VAR z2Ptr:bits32;
     VAR z3Ptr:bits32
 );
Var
    z0, z1, z2, z3: bits32;
    more1, more2: bits32;
Begin

    mul32To64( a1, b1, z2, z3 );
    mul32To64( a1, b0, z1, more2 );
    add64( z1, more2, 0, z2, z1, z2 );
    mul32To64( a0, b0, z0, more1 );
    add64( z0, more1, 0, z1, z0, z1 );
    mul32To64( a0, b1, more1, more2 );
    add64( more1, more2, 0, z2, more1, z2 );
    add64( z0, z1, 0, more1, z0, z1 );
    z3Ptr := z3;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
End;

{*----------------------------------------------------------------------------
| Multiplies `a' by `b' to obtain a 128-bit product.  The product is broken
| into two 64-bit pieces which are stored at the locations pointed to by
| `z0Ptr' and `z1Ptr'.
*----------------------------------------------------------------------------*}

procedure mul64To128( a, b : bits64; var z0Ptr, z1Ptr : bits64);
var
    aHigh, aLow, bHigh, bLow : bits32;
    z0, zMiddleA, zMiddleB, z1 : bits64;
begin
    aLow := a;
    aHigh := a shr 32;
    bLow := b;
    bHigh := b shr 32;
    z1 := ( bits64(aLow) ) * bLow;
    zMiddleA := ( bits64( aLow )) * bHigh;
    zMiddleB := ( bits64( aHigh )) * bLow;
    z0 := ( bits64(aHigh) ) * bHigh;
    inc(zMiddleA, zMiddleB);
    inc(z0 ,( ( bits64( zMiddleA < zMiddleB ) ) shl 32 ) + ( zMiddleA shr 32 ));
    zMiddleA := zMiddleA shl 32;
    inc(z1, zMiddleA);
    inc(z0, ord( z1 < zMiddleA ));
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*----------------------------------------------------------------------------
| Multiplies the 128-bit value formed by concatenating `a0' and `a1' to the
| 128-bit value formed by concatenating `b0' and `b1' to obtain a 256-bit
| product.  The product is broken into four 64-bit pieces which are stored at
| the locations pointed to by `z0Ptr', `z1Ptr', `z2Ptr', and `z3Ptr'.
*----------------------------------------------------------------------------*}

procedure mul128To256(a0,a1,b0,b1 : bits64;var z0Ptr,z1Ptr,z2Ptr,z3Ptr : bits64);
var
    z0,z1,z2,z3,more1,more2 : bits64;
begin
    mul64To128( a1, b1, z2, z3 );
    mul64To128( a1, b0, z1, more2 );
    add128( z1, more2, 0, z2, z1, z2 );
    mul64To128( a0, b0, z0, more1 );
    add128( z0, more1, 0, z1, z0, z1 );
    mul64To128( a0, b1, more1, more2 );
    add128( more1, more2, 0, z2, more1, z2 );
    add128( z0, z1, 0, more1, z0, z1 );
    z3Ptr := z3;
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;


{*----------------------------------------------------------------------------
| Multiplies the 128-bit value formed by concatenating `a0' and `a1' by
| `b' to obtain a 192-bit product.  The product is broken into three 64-bit
| pieces which are stored at the locations pointed to by `z0Ptr', `z1Ptr', and
| `z2Ptr'.
*----------------------------------------------------------------------------*}
procedure mul128By64To192(a0,a1,b : bits64;var z0Ptr,z1Ptr,z2Ptr : bits64);
var
    z0, z1, z2, more1 : bits64;
begin
    mul64To128( a1, b, z1, z2 );
    mul64To128( a0, b, z0, more1 );
    add128( z0, more1, 0, z1, z0, z1 );
    z2Ptr := z2;
    z1Ptr := z1;
    z0Ptr := z0;
end;

{*----------------------------------------------------------------------------
| Returns an approximation to the 64-bit integer quotient obtained by dividing
| `b' into the 128-bit value formed by concatenating `a0' and `a1'.  The
| divisor `b' must be at least 2^63.  If q is the exact quotient truncated
| toward zero, the approximation returned lies between q and q + 2 inclusive.
| If the exact quotient q is larger than 64 bits, the maximum positive 64-bit
| unsigned integer is returned.
*----------------------------------------------------------------------------*}

Function estimateDiv128To64( a0:bits64; a1: bits64; b:bits64): bits64;
var
    b0, b1, rem0, rem1, term0, term1, z : bits64;
begin
    if ( b <= a0 ) then
      begin
        result:=qword( $FFFFFFFFFFFFFFFF );
        exit;
      end;
    b0 := b shr 32;
    if ( b0 shl 32 <= a0 ) then
      z:=qword( $FFFFFFFF00000000 )
    else
      z:=( a0 div b0 ) shl 32;
    mul64To128( b, z, term0, term1 );
    sub128( a0, a1, term0, term1, rem0, rem1 );
    while ( ( sbits64(rem0) ) < 0 ) do begin
        dec(z,qword( $100000000 ));
        b1 := b shl 32;
        add128( rem0, rem1, b0, b1, rem0, rem1 );
    end;
    rem0 := ( rem0 shl 32 ) or ( rem1 shr 32 );
    if ( b0 shl 32 <= rem0 ) then
      z:=z or $FFFFFFFF
    else
      z:=z or rem0 div b0;
    result:=z;
end;


{*
-------------------------------------------------------------------------------
Returns an approximation to the 32-bit integer quotient obtained by dividing
`b' into the 64-bit value formed by concatenating `a0' and `a1'.  The
divisor `b' must be at least 2^31.  If q is the exact quotient truncated
toward zero, the approximation returned lies between q and q + 2 inclusive.
If the exact quotient q is larger than 32 bits, the maximum positive 32-bit
unsigned integer is returned.
-------------------------------------------------------------------------------
*}
Function estimateDiv64To32( a0:bits32; a1: bits32; b:bits32): bits32;
Var
    b0, b1: bits32;
    rem0, rem1, term0, term1: bits32;
    z: bits32;
Begin
    if ( b <= a0 ) then
    Begin
       estimateDiv64To32 := $FFFFFFFF;
       exit;
    End;
    b0 := b shr 16;
    if ( b0 shl 16 <= a0 ) then
       z:= $FFFF0000
     else
       z:= ( a0 div b0 ) shl 16;
    mul32To64( b, z, term0, term1 );
    sub64( a0, a1, term0, term1, rem0, rem1 );
    while ( ( sbits32 (rem0) ) < 0 ) do
    Begin
        z := z - $10000;
        b1 := b shl 16;
        add64( rem0, rem1, b0, b1, rem0, rem1 );
    End;
    rem0 := ( rem0 shl 16 ) OR ( rem1 shr 16 );
    if ( b0 shl 16 <= rem0 ) then
      z := z or $FFFF
    else
      z := z or (rem0 div b0);
    estimateDiv64To32 := z;

End;

{*
-------------------------------------------------------------------------------
Returns an approximation to the square root of the 32-bit significand given
by `a'.  Considered as an integer, `a' must be at least 2^31.  If bit 0 of
`aExp' (the least significant bit) is 1, the integer returned approximates
2^31*sqrt(`a'/2^31), where `a' is considered an integer.  If bit 0 of `aExp'
is 0, the integer returned approximates 2^31*sqrt(`a'/2^30).  In either
case, the approximation returned lies strictly within +/-2 of the exact
value.
-------------------------------------------------------------------------------
*}
Function estimateSqrt32( aExp: int16; a: bits32 ): bits32;
    const sqrtOddAdjustments: array[0..15] of bits16 = (
        $0004, $0022, $005D, $00B1, $011D, $019F, $0236, $02E0,
        $039C, $0468, $0545, $0631, $072B, $0832, $0946, $0A67
    );
    const sqrtEvenAdjustments: array[0..15] of bits16 = (
        $0A2D, $08AF, $075A, $0629, $051A, $0429, $0356, $029E,
        $0200, $0179, $0109, $00AF, $0068, $0034, $0012, $0002
    );
Var
    index: int8;
    z: bits32;
Begin

    index := ( a shr 27 ) AND 15;
    if ( aExp AND 1 ) <> 0  then
    Begin
        z := $4000 + ( a shr 17 ) - sqrtOddAdjustments[ index ];
        z := ( ( a div z ) shl 14 ) + ( z shl 15 );
        a := a shr 1;
    End
    else
    Begin
        z := $8000 + ( a shr 17 ) - sqrtEvenAdjustments[ index ];
        z := a div z + z;
        if ( $20000 <= z ) then
          z := $FFFF8000
        else
          z := ( z shl 15 );
        if ( z <= a ) then
        Begin
           estimateSqrt32 := bits32 ( ( sbits32 (a )) shr 1 );
           exit;
        End;
    End;
    estimateSqrt32 := ( ( estimateDiv64To32( a, 0, z ) ) shr 1 ) + ( z shr 1 );
End;

{*
-------------------------------------------------------------------------------
Returns the number of leading 0 bits before the most-significant 1 bit of
`a'.  If `a' is zero, 32 is returned.
-------------------------------------------------------------------------------
*}
Function countLeadingZeros32( a:bits32 ): int8;

    const countLeadingZerosHigh:array[0..255] of int8 = (
        8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    );
Var
    shiftCount: int8;
Begin

    shiftCount := 0;
    if ( a < $10000 ) then
    Begin
        shiftCount := shiftcount + 16;
        a := a shl 16;
    End;
    if ( a < $1000000 ) then
    Begin
        shiftCount := shiftcount + 8;
        a := a shl 8;
    end;
    shiftCount := shiftcount + countLeadingZerosHigh[ a shr 24 ];
    countLeadingZeros32:= shiftCount;
End;

{*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 64 is returned.
*----------------------------------------------------------------------------*}

function countLeadingZeros64( a : bits64): int8;
var
 shiftcount : int8;
Begin
    shiftCount := 0;
    if ( a <  (bits64(1)  shl 32 )) then
        shiftCount := shiftcount + 32
    else
        a := a shr 32;
    shiftCount := shiftCount + countLeadingZeros32( a );
    countLeadingZeros64:= shiftCount;
End;



{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is
equal to the 64-bit value formed by concatenating `b0' and `b1'.  Otherwise,
returns 0.
-------------------------------------------------------------------------------
*}
Function eq64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin
    eq64 :=  flag( a0 = b0 ) and flag( a1 = b1 );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is less
than or equal to the 64-bit value formed by concatenating `b0' and `b1'.
Otherwise, returns 0.
-------------------------------------------------------------------------------
*}
Function le64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin

    le64:= flag( a0 < b0 ) or flag( ( a0 = b0 ) and ( a1 <= b1 ) );

End;

{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is less
than the 64-bit value formed by concatenating `b0' and `b1'.  Otherwise,
returns 0.
-------------------------------------------------------------------------------
*}
Function lt64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin
    lt64 := flag( a0 < b0 ) or flag( ( a0 = b0 ) and ( a1 < b1 ) );
End;

{*
-------------------------------------------------------------------------------
Returns 1 if the 64-bit value formed by concatenating `a0' and `a1' is not
equal to the 64-bit value formed by concatenating `b0' and `b1'.  Otherwise,
returns 0.
-------------------------------------------------------------------------------
*}
Function ne64( a0: bits32; a1:bits32 ;b0:bits32; b1:bits32 ): flag;
Begin
    ne64:= flag( a0 <> b0 ) or flag( a1 <> b1 );
End;

const
  float128_default_nan_high = qword($FFFFFFFFFFFFFFFF);
  float128_default_nan_low = qword($FFFFFFFFFFFFFFFF);


(*****************************************************************************)
(*                      End Low-Level arithmetic                             *)
(*****************************************************************************)


{*
-------------------------------------------------------------------------------
Functions and definitions to determine:  (1) whether tininess for underflow
is detected before or after rounding by default, (2) what (if anything)
happens when exceptions are raised, (3) how signaling NaNs are distinguished
from quiet NaNs, (4) the default generated quiet NaNs, and (4) how NaNs
are propagated from function inputs to output.  These details are ENDIAN
specific
-------------------------------------------------------------------------------
*}
{$IFDEF ENDIAN_LITTLE}
{*
-------------------------------------------------------------------------------
Internal canonical NaN format.
-------------------------------------------------------------------------------
*}
TYPE
 commonNaNT = packed record
   sign: flag;
   high, low : bits32;
 end;
{*
-------------------------------------------------------------------------------
The pattern for a default generated single-precision NaN.
-------------------------------------------------------------------------------
*}
const float32_default_nan = $FFC00000;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is a NaN;
otherwise returns 0.
-------------------------------------------------------------------------------
*}
Function float32_is_nan( a : float32 ): flag;
Begin

    float32_is_nan:= flag( $FF000000 < bits32 ( a shl 1 ) );

End;

{*
-------------------------------------------------------------------------------
Returns 1 if the single-precision floating-point value `a' is a signaling
NaN; otherwise returns 0.
-------------------------------------------------------------------------------
*}
Function float32_is_signaling_nan( a : float32  ): flag;
Begin

    float32_is_signaling_nan := flag
      ( ( ( a shr 22 ) and $1FF ) = $1FE ) and( a and $003FFFFF );

End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the single-precision floating-point NaN
`a' to the canonical NaN format.  If `a' is a signaling NaN, the invalid
exception is raised.
-------------------------------------------------------------------------------
*}
Procedure float32ToCommonNaN( a: float32; VAR c:commonNaNT  );
var
    z : commonNaNT ;
Begin
    if ( float32_is_signaling_nan( a ) <> 0) then
       float_raise( float_flag_invalid );
    z.sign := a shr 31;
    z.low := 0;
    z.high := a shl 9;
    c := z;

End;

{*
-------------------------------------------------------------------------------
Returns the result of converting the canonical NaN `a' to the single-
precision floating-point format.
-------------------------------------------------------------------------------
*}
Function commonNaNToFloat32( a : commonNaNT ): float32;
Begin
    commonNaNToFloat32 := ( ( bits32 (a.sign) ) shl 31 ) or $7FC00000 or ( a.high shr 9 );
End;

{*
-------------------------------------------------------------------------------
Takes two single-precision floating-point values `a' and `b', one of which
is a NaN, and returns the appropriate NaN result.  If either `a' or `b' is a
signaling NaN, the invalid exception is raised.
-------------------------------------------------------------------------------
*}
Function propagateFloat32NaN( a : float32 ; b: float32 ): float32;
Var
    aIsNaN, aIsSignalingNaN, bIsNaN, bIsSignalingNaN: flag;
label returnLargerSignificand;
Begin
    aIsNaN := float32_is_nan( a );
    aIsSignalingNaN := float32_is_signaling_nan( a );
    bIsNaN := float32_is_nan( b );
    bIsSignalingNaN := float32_is_signaling_nan( b );
    a := a or $00400000;
    b := b or $00400000;
    if ( aIsSignalingNaN or bIsSignalingNaN ) <> 0 then
        float_raise( float_flag_invalid );
    if ( aIsSignalingNaN )<> 