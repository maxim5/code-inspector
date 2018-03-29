{!! The funny stuff between the !! lines is explained below.
cat >mainvars.pas <<'//GO.SYSIN DD mainvars.pas'
 !!}
(* Note: the statements "input: text;", "assign(input,'con:');",
 *    and "reset(input);" appear below as comments; some version of
 *    Pascal require you to activate one or more of these statements.
 *
 *  Some versions of TURBO Pascal (e.g. PC versions >= 4) require
 *  splitting the following source into several "units".  The goo
 *  between pairs of !! lines gives a way to do this.  If you have
 *  this file on a UNIX system, you can simply pipe it through
 *         sed /!!/d | /bin/sh
 *  to create files mainvars.pas, unit1.pas, unit2.pas, and par.pas;
 *  the first 3 are "units" needed in the fourth.  If using a UNIX
 *  system is inconvenient, you can do the splitting by hand:
 *  omit the lines that contain !! (that's what "sed /!!/d" does)
 *  and put the lines between each "cat >..." and the following
 *  "//GO.SYSIN DD" line into the file named on these lines.
 *)

{$P1024,D+}  { to allow I/O redirection under TURBO Pascal versions < 3 !!}

program PARA (input, output);	{ !!
unit mainvars;
interface
 !!}
{ (C) Apr 19 1983 in BASIC version by:
      Professor W M Kahan,
      567 Evans Hall.
      Electrical Engineering & Computer Science Dept.
      University of California
      Berkeley, California 94720
      USA
 converted to Pascal by:
      B A Wichmann
      National Physical Laboratory
      Teddington Middx
      TW11 OLW
      UK
 further massaging by dmg =
      David M. Gay
      AT&T Bell Labs
      600 Mountain Avenue
      Murray Hill, NJ 07974
 and a couple of bug fixes from dgh = sun!dhough (29 May 1986)

 See the article by Richard Karpinski in the February 1985 issue
 of BYTE Magazine.

 You may copy this program freely if you acknowledge its source.
 Comments on the Pascal version to NPL or dmg, please.  }

   const
      {integer constants}
      NoTrials = 20;
      {Number of tests for commutativity. }

   type
      Guard = (Yes, No);
      Rounding = (Chopped, Rounded, Other);
      Message = packed array [1..40] of char;
      WhichOp = packed array [1..14] of char;
      Class = (Flaw, Defect, SeriousDefect, Failure);

   var
      {input: text;}
      {Small floating point constants.}
      Zero, { 0.0; }
      Half, { 0.5; }
      One, { 1.0; }
      Two, { 2.0; }
      Three, { 3.0; }
      Four, { 4.0; }
      Five, { 5.0; }
      Eight, { 8.0; }
      Nine, { 9.0; }
      TwentySeven, { 27.0; }
      ThirtyTwo, { 32.0; }
      TwoForty, { 240.0; }
      MinusOne, { -1.0; }
      OneAndHalf: { 1.5; } real;
      MyZero: integer;
      NoTimes, Index: integer;
      ch: char;
      AInverse, A1: real;
      Radix, BInverse, RadixD2, BMinusU2: real;
      C, CInverse: real;
      D, FourD: real;
      E0, E1, Exp2, MinSqrtError: real;
      SqrtError, MaxSqrtError, E9: real;
      Third: real;
      F6, F9: real;
      H, HInverse: real;
      I: integer;
      StickyBit, J: real;
      M, N, N1: real;
      Precision: real;
      Q, Q9: real;
      R, R9: real;
      T, Underflow, S: real;
      OneUlp, UnderflowThreshold, U1, U2: real;
      V, V0, V9: real;
      W: real;
      X, X1, X2, X8, RandomNumber1: real;
      Y, Y1, Y2, RandomNumber2: real;
      Z, PseudoZero, Z1, Z2, Z9: real;
      NoErrors: array [Class] of integer;
      Milestone: integer;
      PageNo: integer;
      GMult, GDiv, GAddSub: Guard;
      RMult, RDiv, RAddSub, RSqrt: Rounding;
      Continue, Break, Done, NotMonot, Monot, AnomolousArithmetic, IEEE,
            SquareRootWrong, UnderflowNotGradual: Boolean;
      { Computed constants. }
      {U1  gap below 1.0, i.e, 1.0-U1 is next number below 1.0 }
      {U2  gap above 1.0, i.e, 1.0+U2 is next number above 1.0 }
{!!
   procedure Page;
   function Int (X: real): real;
   function Sign (X: real): real;
   procedure Pause;
   procedure Instructions;
   procedure Heading;
   procedure Characteristics;
   procedure History;
   procedure notify(T: WhichOp);
   procedure TestCondition (K: Class; Valid: Boolean; T: Message);
   function Random: real;
   procedure SqrtXMinX (ErrorKind: Class);
   procedure NewD;
   procedure SubRout3750;
   function Power (X, Y: real): real;
   procedure DoesYequalX;
   procedure SubRout3980;
   procedure PrintIfNPositive;
   procedure TestPartialUnderflow;

implementation
 !!}


   procedure Page;
     begin
       (* write(#$C) *) {FF in TURBO Pascal} writeln; writeln;
       end;

   function Int (X: real): real;
   {  simulates BASIC INT-function, which is defined as:
      INT(X) is the greatest integer value less than or
      equal to X. }


      function LargeTrunc (X: real): real;

         var
            start, acc, y, p: real;
            trunced: integer; (* dgh *)

         begin (* LargeTrunc *)
         if abs (X) < maxint then begin
            trunced := trunc(X);
            LargeTrunc := trunced;
            end
         else
            begin
            start := abs (X);
            acc := 0.0;
            repeat
               y := start;
               p := 1.0;
               while y > maxint - 1.0 do
                  begin
                  y := y / Radix;
                  p := p * Radix;
                  end;
               trunced := trunc(y);
               acc := acc + trunced * p;
               start := start - trunced * p;
            until start < 1.0;
            if X < 0.0 then
               LargeTrunc := - acc
            else
               LargeTrunc := acc
            end;
         end (* LargeTrunc *);


      begin (* Int *)
      if X > 0.0 then
         Int := LargeTrunc (X)
      else if LargeTrunc (X - 0.5) = X then
         Int := X
      else
         Int := LargeTrunc (X) - 1;
      end (* Int *);


   function Sign (X: real): real;

      begin (* Sign *)
      if X < 0.0 then
         Sign := - 1.0
      else
         Sign := + 1.0;
      end (* Sign *);


   procedure Pause;

      var
         ch: char;

      begin (* Pause *)
      writeln ('To continue, press any key and newline:');
      readln (input);
      while not eoln (input) do
         read (input, ch);
      Page;
      write ('Diagnosis resumes after milestone no ', Milestone);
      writeln ('               Page: ', PageNo);
      writeln;
      Milestone := Milestone + 1;
      PageNo := PageNo + 1;
      end (* Pause *);


   procedure Instructions;

      begin (* Instructions *)
      writeln ('Lest this program stop prematurely, ',
            'i.e. before displaying');
      writeln ('         "END OF TEST",');
      writeln ('try to persuade the computer NOT to',
            ' terminate execution whenever an');
      writeln ('error like Over/Underflow or Division by Zero occurs,',
            ' but rather');
      writeln ('to persevere with a surrogate value after, ',
            ' perhaps, displaying some');
      writeln ('warning.  If persuasion avails naught, don''t despair'
            , ' but run this');
      writeln ('program anyway to see how many milestones it passes,',
            ' and then');
      writeln ('amend it to make further progress.');
      writeln ('Answer questions with Y, y, N or n',
            ' (unless otherwise indicated).');
      writeln;
      end (* Instructions *);


   procedure Heading;

      begin (* Heading *)
      writeln ('Users are invited to help debug and augment',
            ' this program so it will');
      writeln ('cope with unanticipated and newly uncovered',
            ' arithmetic pathologies.');
      writeln ('Please send suggestions and interesting results to');
      writeln('        Richard Karpinski');
      writeln('        Computer Center U-76');
      writeln('        University of California');
      writeln('        San Francisco, CA 94143-0704, USA');
      writeln;
      writeln('In doing so, please include the following information:');
      writeln('        Version:  10 February 1989');
      writeln('        Computer:'); writeln;
      writeln('        Compiler:'); writeln;
      writeln('        Optimization level:'); writeln;
      writeln('        Other relevant compiler options:'); writeln;
      end (* Heading *);


   procedure Characteristics;

      begin (* Characteristics *)
      writeln (
            'Running this program should reveal these characteristics');
      writeln ('  Radix = 1, 2, 4, 8, 10, 16, 100, 256, or ...');
      writeln ('  Precision = number of significant digits carried.');
      writeln ('  U2 = Radix/Radix^Precision = One Ulp (OneUlpnit in the');
      writeln ('    Last Place) of 1.000xxx .');
      writeln ('  U1 = 1/Radix^Precision = One Ulp of numbers',
            ' a little less than 1.0 .');
      writeln ('  Adequacy of guard digits for Mult., Div., and Subt.');
      writeln ('  Whether arithmetic is chopped, correctly rounded, ',
            'or something else');
      writeln ('    for Mult., Div., Add/Subt. and Sqrt.');
      writeln ('  Whether a Sticky Bit is used correctly for rounding.');
      writeln ('  UnderflowThreshold = an Underflow Threshold.');
      writeln ('  E0 and PseudoZero tell whether underflow is abrupt,',
            ' gradual, or fuzzy.');
      writeln ('  V = an overflow threshold, roughly.');
      writeln ('  V0  tells, roughly, whether Infinity is represented.');
      writeln ('  Comparisions are checked for consistency with',
            ' subtraction');
      writeln ('    and for contamination with pseudo-zeros.');
      writeln ('  Sqrt is tested.  Y^X is not tested.');
      writeln ('  Extra-precise subexpressions are revealed',
            ' but NOT YET tested.');
      writeln ('  Decimal-Binary conversion is NOT YET tested',
            ' for accuracy.');
      end (* Characteristics *);


   procedure History;

      begin (* History *)
      writeln ('The program attempts to discriminate among');
      writeln ('   FLAWs, like lack of a sticky bit,');
      writeln ('   SERIOUS DEFECTs, like lack of a guard digit, and');
      writeln ('   FAILUREs, like 2+2 = 5 .');
      writeln ('Failures may confound subsequent diagnoses.');
      writeln;
      writeln ('The diagnostic capabilities of this program go beyond',
            ' an earlier');
      writeln ('program called "MACHAR", which can be found at the',
            ' end of the');
      writeln ('book  "Software Manual for the Elementary Functions',
            '" (1980) by');
      writeln ('W. J. Cody and W. Waite.  Although both programs',
            ' try to discover');
      writeln ('the Radix, Precision and range (over/underflow',
            ' thresholds)');
      writeln ('of the arithmetic, this program tries to cope',
            ' with a wider variety');
      writeln ('of pathologies, and to say how well the',
            ' arithmetic is implemented.');
      writeln ('BASIC version of this program (C) 1983 by Professor',
            ' W. M. Kahan;');
      writeln ('see source comments for more history.');
      writeln;
      writeln ('The program is based upon a conventional',
            ' radix representation for');
      writeln ('floating-point numbers, but also allows',
            ' logarithmic encoding');
      writeln ('as used by certain early WANG machines.');
      writeln;
      end (* History *);


   procedure notify(T: WhichOp);
      begin
      writeln('The ', T, ' test is inconsistent;');
      writeln('   PLEASE NOTIFY KARPINSKI !');
      end;

   procedure TestCondition (K: Class; Valid: Boolean; T: Message);

      begin (* TestCondition *)
      if not Valid then
         begin
         NoErrors [K] := NoErrors [K] + 1;
         case K of
            Flaw:
               write ('FLAW');
            Defect:
               write ('DEFECT');
            SeriousDefect:
               write ('SERIOUS DEFECT');
            Failure:
               write ('FAILURE');
            end;
         writeln (':  ', T);
         end;
      end (* TestCondition *);


   function Random: real;

      var
         X, Y: real;

      begin (* Random *)
      X := RandomNumber1 + R9;
      Y := X * X;
      Y := Y * Y;
      X := X * Y;
      Y := X - Int (X);
      RandomNumber1 := Y + X * 0.000005;
      Random := RandomNumber1;
      end (* Random *);


   procedure SqrtXMinX (ErrorKind: Class);

      begin (* SqrtXMinX *)
      SqrtError := ((sqrt (X * X) - X * BInverse) - (X - X * BInverse))
            / OneUlp;
      if SqrtError <> 0 then
         begin
         if SqrtError < MinSqrtError then
            MinSqrtError := SqrtError;
         if SqrtError > MaxSqrtError then
            MaxSqrtError := SqrtError;
         J := J + 1;
         writeln;
         if ErrorKind = SeriousDefect then
            write ('SERIOUS ');
         writeln ('DEFECT:  sqrt( ', X * X, ' - ', X, ') = ');
         writeln (OneUlp * SqrtError, ' instead of correct value 0 .');
         end;
      end (* SqrtXMinX *);


   procedure NewD;

      begin (* NewD *)
      X := Z1 * Q;
      X := Int (Half - X / Radix) * Radix + X;
      Q := (Q - X * Z) / Radix + X * X * (D / Radix);
      Z := Z - Two * X * D;
      if Z <= Zero then
         begin
         Z := - Z;
         Z1 := - Z1;
         end;
      D := Radix * D;
      end (* NewD *);


   procedure SubRout3750;

      begin (* SubRout3750 *)
      if not ((X - Radix < Z2 - Radix) or (X - Z2 > W - Z2)) then
         begin
         I := I + 1;
         X2 := sqrt (X * D);
         Y2 := (X2 - Z2) - (Y - Z2);
         X2 := X8 / (Y - Half);
         X2 := X2 - Half * X2 * X2;
         SqrtError := (Y2 + Half) + (Half - X2);
         if SqrtError < MinSqrtError then
            MinSqrtError := SqrtError;
         SqrtError := Y2 - X2;
         if SqrtError > MaxSqrtError then
            MaxSqrtError := SqrtError;
         end;
      end (* SubRout3750 *);

   { Sun version of Power (from dgh)...
   function pow(x, y : real ): real; external c;
   function Power(X, Y: real): real;
      var xx, yy: real;
      begin
         xx := X;
         yy := Y;
         Power := pow(xx,yy);
         end;
   }

   function Power (X, Y: real): real;

      (* Crude power function:  see Cody & Waite for a better one. *)

      begin (* Power *)
      if Y = 0.0 then
         Power := 1.0
      else if (X = 0.0) and (Y > 0.0) then
         Power := 0.0
      else if (X < 0.0) and (Y = Int(Y)) then
         if odd(trunc(Y)) then Power := -exp (Y * ln (-X))
         else Power := exp (Y * ln (-X))
      else
         Power := exp (Y * ln (X));
      end (* Power *);


   procedure DoesYequalX;

      begin (* DoesYequalX *)
      if Y <> X then
         begin
         if N <= 0 then
            begin
            if (Z = Zero) and (Q <= Zero) then write('WARNING') (* dgh: Y --> Z *)
            else begin
               NoErrors [Defect] := NoErrors [Defect] + 1;
               write('DEFECT');
               end;
            writeln (':  computed (', Z, ') ^ (', Q, ') = ');
            writeln (Y, ', which compares unequal to correct ', X, ';'); (* dgh: V --> Y *)
            writeln (' they differ by ', Y - X);
            end;
         N := N + 1;
      { ... count discrepancies. }
         end;
      end (* DoesYequalX *);


   procedure SubRout3980;

      begin (* SubRout3980 *)
      repeat
         Q := I;
         Y := Power (Z, Q);
         DoesYequalX;
         I := I + 1;
         if I <= M then
            X := Z * X;
      until (X >= W) or (I > M);
      end (* SubRout3980 *);


   procedure PrintIfNPositive;

      begin (* PrintIfNPositive *)
      if N > 0 then
         writeln ('Similar discrepancies have occurred ', N, ' times.');
      end (* PrintIfNPositive *);


   procedure TestPartialUnderflow;

      begin (* TestPartialUnderflow *)
      N := 0;
      if Z <> 0 then
         begin
         writeln ('Since comparison denies Z = 0, evaluating');
         writeln ('(Z + Z) / Z should be safe.');
         write ('What the machine gets for (Z + Z) / Z is: ');
         Q9 := (Z + Z) / Z;
         writeln (Q9);
         if (abs (Q9 - Two) < Radix * U2) then
            begin
            write ('This is O.K., provided Over/Underflow');
            writeln (' has NOT just been signaled.');
            end
         else if (Q9 < One) or (Q9 > Two) then
            begin
            N := 1;
            NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
            writeln ('This is a VERY SERIOUS DEFECT!');
            end
         else
            begin
            N := 1;
            NoErrors [Defect] := NoErrors [Defect] + 1;
            writeln ('This is a DEFECT.');
            end;
         V9 := Z * One;
         RandomNumber1 := V9;
         V9 := One * Z;
         RandomNumber2 := V9;
         V9 := Z / One;
         if (Z = RandomNumber1) and (Z = RandomNumber2)
               and (Z = V9) then
            begin
            if N > 0 then
               Pause
            end
         else
            begin
            N := 1;
            NoErrors [Defect] := NoErrors [Defect] + 1;
            writeln ('DEFECT: What prints as Z = ', Z, 'compares');
            write ('different from        ');
            if not (Z = RandomNumber1) then
               writeln ('Z * 1 = ', RandomNumber1);
            if not ((Z = RandomNumber2)
                  or (RandomNumber2 = RandomNumber1)) then
               writeln ('1 * Z = ', RandomNumber2);
            if not (Z = V9) then
               writeln ('Z / 1 = ', V9);
            if RandomNumber2 <> RandomNumber1 then
               begin
               NoErrors [Defect] := NoErrors [Defect] + 1;
               writeln ('DEFECT   Multiplication does not commute;');
               writeln ('comparrison allegs that 1 * Z = ',
                     RandomNumber2);
               writeln ('differs from Z * 1 = ', RandomNumber1);
               end;
            if N > 0 then
               Pause;
            end;
         end;
      end (* TestPartialUnderflow *);
{!!
end.
//GO.SYSIN DD mainvars.pas
cat >unit1.pas <<'//GO.SYSIN DD unit1.pas'
unit Unit1;
interface
   uses mainvars;
   procedure start;
   procedure mile2060;

implementation
   procedure start;
 !!}


   begin (* PARA *)

   {First two assignments use integer right-hand sides.}
   Zero := 0;
   One := 1;
   Two := One + One;
   Three := Two + One;
   Four := Three + One;
   Five := Four + One;
   Eight := Four + Four;
   Nine := Three * Three;
   TwentySeven := Nine * Three;
   ThirtyTwo := Four * Eight;
   TwoForty := Four * Five * Three * Four;
   MinusOne := -One;
   Half := One / Two;
   OneAndHalf := One + Half;

   NoErrors [Failure] := 0;
   NoErrors [SeriousDefect] := 0;
   NoErrors [Defect] := 0;
   NoErrors [Flaw] := 0;
   PageNo := 0;
{=============================================}
   Milestone := 0;
{=============================================}
   writeln ('Type any character to start the program.');
   { assign(input,'con:');} { for TURBO Pascal version 2 }
   { reset (input); }       { for old Cray Pascal }
   while not eoln (input) do
      read (input, ch);
   Instructions;
   Pause;
   Heading;
   Pause;
   Characteristics;
   Pause;
   History;
{=============================================}
   Milestone := 7;
{=============================================}
   Pause;
   writeln ('Program is now RUNNING tests on small integers:');
   TestCondition (Failure, (Zero + Zero = Zero) and (One - One = Zero)
         and (One > Zero)
         and (One + One = Two), ' 0+0<>0  or 1-1<>0  or  1<=0  or 1+1<>2 '
         );
   Z := - Zero;
   if Z <> 0.0 then
      begin
      NoErrors [Failure] := NoErrors [Failure] + 1;
      writeln ('Comparison alleges that -0.0 is Non-zero!');
      U2 := 0.001;
      Radix := 1;
      TestPartialUnderflow;
      end;
   TestCondition (Failure, (Three = Two + One) and (Four = Three + One)
         and (Four + Two * (- Two) = Zero)
         and (Four - Three - One = Zero),
         ' 3<>2+1, 4<>3+1, 4+2*(-2)<>0 or 4-3-1<>0');
   TestCondition (Failure, (MinusOne = - One)
         and (MinusOne + One = Zero ) and (One + MinusOne = Zero)
         and (MinusOne + abs (One) = Zero)
         and (MinusOne + MinusOne * MinusOne = Zero),
         '-1+1<>0, -1+abs(1)<>0 or -1+(-1)*(-1)<>0');
   TestCondition (Failure, Half + MinusOne + Half = Zero,
         '   1/2  + (-1) + 1/2 <> 0               ');
{=============================================}
   Milestone := 10;
{=============================================}
   TestCondition (Failure, (Nine = Three * Three)
         and (TwentySeven = Nine * Three) and (Eight = Four + Four)
         and (ThirtyTwo = Eight * Four)
         and (ThirtyTwo - TwentySeven - Four - One = Zero),
         '9<>3*3, 27<>9*3, 32<>8*4 or 32-27-4-1<>0');
   TestCondition (Failure, (Five = Four + One) 
         and (TwoForty = Four * Five * Three * Four)
         and (TwoForty / Three - Four * Four * Five = Zero)
         and ( TwoForty / Four - Five * Three * Four = Zero)
         and ( TwoForty / Five - Four * Three * Four = Zero),
         '5<>4+1,240/3<>80,240/4<>60, or 240/5<>48');
   if NoErrors [Failure] = 0 then
      begin
      writeln (' -1, 0, 1/2, 1, 2, 3, 4, 5, 9, 27, 32 & 240 are O.K.');
      writeln
      end;
   writeln ('Searching for Radix and Precision.');
   W := One;
   repeat
      W := W + W;
      Y := W + One;
      Z := Y - W;
      Y := Z - One;
   until (MinusOne + abs (Y) >= Zero);
{.. now W is just big enough that |((W+1)-W)-1| >= 1 ...}
   Precision := 0;
   Y := One;
   repeat
      Radix := W + Y;
      Y := Y + Y;
      Radix := Radix - W;
   until (Radix <> Zero);
   if Radix < Two then
      Radix := One;
   writeln ('Radix = ', Radix);
   if Radix <> 1 then
      begin
      W := One;
      repeat
         Precision := Precision + One;
         W := W * Radix;
         Y := W + One;
      until (Y - W) <> One;
   {... now W = Radix^Precision is barely too big to satisfy (W+1)-W = 1
                                          ...}
      end;
   U1 := One / W;
   U2 := Radix * U1;
   writeln ('Closest relative separation found is U1 = ', U1);
   writeln;
   writeln ('Recalculating radix and precision');
   E0 := Radix;
   E1 := U1;
   E9 := U2;
{save old values}
   X := Four / Three;
   Third := X - One;
   F6 := Half - Third;
   X := F6 + F6;
   X := abs (X - Third);
   if X < U2 then
      X := U2;
{... now X = (unknown no.) ulps of 1+...}
   repeat
      U2 := X;
      Y := Half * U2 + ThirtyTwo * U2 * U2;
      Y := One + Y;
      X := Y - One;
   until (U2 <= X) or (X <= Zero);
{... now U2 = 1 ulp of 1 + ... }
   X := Two / Three;
   F6 := X - Half;
   Third := F6 + F6;
   X := Third - Half;
   X := abs (X + F6);
   if X < U1 then
      X := U1;
{... now  X = (unknown no.) ulps of 1 -... }
   repeat
      U1 := X;
      Y := Half * U1 + ThirtyTwo * U1 * U1;
      Y := Half - Y;
      X := Half + Y;
      Y := Half - X;
      X := Half + Y;
   until (U1 <= X) or (X <= Zero);
{... now U1 = 1 ulp of 1 - ... }
   if U1 = E1 then
      writeln (' confirms closest relative separation U1 .')
   else
      writeln (' gets better closest relative separation U1 = ', U1);
   W := One / U1;
   F9 := (Half - U1) + Half;
   Radix := Int (0.01 + U2 / U1);
   if Radix = E0 then
      writeln ('Radix confirmed.')
   else
      writeln ('MYSTERY: recalculated Radix = ', Radix);
   TestCondition (Defect, Radix <= Eight + Eight,
         'Radix is too big: roundoff problems     ');
   TestCondition (Flaw, (Radix = Two) or (Radix = 10)
         or (Radix = One), 'Radix is not as good as 2 or 10.        ');
{!!
   end (*start*);

   procedure mile2060;
   begin
 !!}

{=============================================}
   Milestone := 20;
{=============================================}
   TestCondition (Failure, F9 - Half < Half,
         ' (1-U1)-1/2 < 1/2 is FALSE, prog. fails?');
   X := F9;
   I := 1;
   Y := X - Half;
   Z := Y - Half;
   TestCondition (Failure, (X <> One)
         or (Z = Zero), 'Comparison is fuzzy,X=1 but X-1/2-1/2<>1');
   X := One + U2;
   I := 0;
{=============================================}
   Milestone := 25;
{=============================================}
   BMinusU2 := Radix - One;
   BMinusU2 := (BMinusU2 - U2) + One;
   if Radix <> One then
      begin {... BMinusU2 = nextafter(Radix, 0) }
      X := - TwoForty * ln (U1) / ln (Radix);
      Y := Int (Half + X);
      if abs (X - Y) * Four < One then
         X := Y;
      Precision := X / TwoForty;
      Y := Int (Half + Precision);
      if abs (Precision - Y) * TwoForty < Half then
         Precision := Y;
   { Purify integers }
      end;
   if (Precision <> Int (Precision)) or (Radix = One) then
      begin
      writeln ('Precision cannot be characterized by an integer',
            ' number of sig. digits,');
      writeln ('but, by itself, this is a minor flaw.');
      end;
   if Radix = One then
      writeln ('logarithmic encoding has precision characterized',
            'solely by U1.')
   else
      writeln ('The number of significant digits of the Radix is ',
            Precision);
   TestCondition (SeriousDefect, U2 * Nine * Nine * TwoForty < One,
         ' Precision worse than 5 decimal figures ');
{=============================================}
   Milestone := 30;
{=============================================}
{ Test for extra-precise subepressions }
   X := abs (((Four / Three - One) - One / Four) * Three - One / Four);
   repeat
      Z2 := X;
      X := (One + (Half * Z2 + ThirtyTwo * Z2 * Z2)) - One;
   until (Z2 <= X) or (X <= Zero);
   Y := abs ((Three / Four - Two / Three) * Three - One / Four);
   Z := Y;
   X := Y;
   repeat
      Z1 := Z;
      Z := (One / Two - ((One / Two - (Half * Z1 + ThirtyTwo * Z1 * Z1))
            + One / Two)) + One / Two;
   until (Z1 <= Z) or (Z <= Zero);
   repeat
      repeat
         Y1 := Y;
         Y := (Half - ((Half - (Half * Y1 + ThirtyTwo * Y1 * Y1)) + Half
               )) + Half;
      until (Y1 <= Y) or (Y <= Zero);
      X1 := X;
      X := ((Half * X1 + ThirtyTwo * X1 * X1) - F9) + F9;
   until (X1 <= X) or (X <= Zero);
   if (X1 <> Y1) or (X1 <> Z1) then
      begin
      NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
      writeln ('SERIOUS DEFECT:  Disagreements among the values X1, Y1, Z1');
      writeln ('resp. ', X1, Y1, Z1);
      writeln ('are symptoms of inconsistencies introduced');
      writeln ('by extra-precise evaluation of allegedly');
      writeln ('"optimized" arithmetic subexpressions.');
      writeln ('Possibly some part of this test is inconsistent.');
      if (X1 = U1) or (Y1 = U1) or (Z1 = U1) then
         writeln ('That feature is not tested further by this program.');
      end
   else if (Z1 <> U1) or (Z2 <> U2) then
      begin
      if (Z1 >= U1) or (Z2 >= U2) then
         begin
         NoErrors [Failure] := NoErrors [Failure] + 1;
         writeln ('FAILURE:  Precision ', Precision);
         writeln ('U1 = ', U1, ' Z1 - U1 = ', Z1 - U1);
         writeln ('U2 = ', U2, ' Z2 - U2 = ', Z2 - U2);
         end
      else begin
        if (Z1 <= Zero) or (Z2 <= Zero) then begin
         writeln ('Because of unusual Radix = ', Radix);
         writeln (' or exact rational arithmetic a result');
         writeln (' Z1 = ', Z1, ' or Z2 = ', Z2);
         writeln (' of an extra precision test is inconsistent.');
         if Z1 = Z2 then
         end;
        if (Z1 <> Z2) or (Z1 > Zero) then begin
         X := Z1 / U1;
         Y := Z2 / U2;
         if Y > X then X := Y;
         Q := - ln (X);
         writeln ('Some subexpressions appear to be calculated');
         writeln ('extra precisely with about ');
         writeln (Q / ln (Radix), 'extra B-digits i.e. ');
         writeln ('roughly ', Q / ln (10),
            ' extra significant decimals.');
         end;
        writeln ('That feature is not tested further by this program.')
        end
      end;
   Pause;
(* For TURBO Pascal versions < 3, put everything after
   the next line into file "paran2.pas" . *)
{$Iparan2.pas} {!!}
{=============================================}
   Milestone := 35;
{=============================================}
   if Radix >= Two then
      begin
      X := W / (Radix * Radix);
      Y := X + One;
      Z := Y - X;
      T := Z + U2;
      X := T - Z;
      TestCondition (Failure, X = U2,
            'Subtraction is not normlzd X=Y,X+Z<>Y+Z!');
      if X = U2 then
          writeln ('Subtraction appears to be normalized,',
            ' as it should be.');
      end;
   writeln;
   writeln ('Checking for guard digit on *, /, and -.');
   Y := F9 * One;
   Z := One * F9;
   X := F9 - Half;
   Y := (Y - Half) - X;
   Z := (Z - Half) - X;
   X := One + U2;
   T := X * Radix;
   R := Radix * X;
   X := T - Radix;
   X := X - Radix * U2;
   T := R - Radix;
   T := T - Radix * U2;
   X := X * (Radix - One);
   T := T * (Radix - One);
   if (X = Zero) and (Y = Zero) and (Z = Zero) and (T = Zero) then
      GMult := Yes
   else
      begin
      GMult := No;
      TestCondition (SeriousDefect, false,
            '  * lacks guard digit, 1*X <> X         ');
      end;
   Z := Radix * U2;
   X := One + Z;
   Y := abs ((X + Z) - X * X) - U2;
   X := One - U2;
   Z := abs ((X - U2) - X * X) - U1;
   TestCondition (Failure, (Y <= Zero)
         and (Z <= Zero), '  * gets too many final digits wrong.   ');
   Y := One - U2;
   X := One + U2;
   Z := One / Y;
   Y := Z - X;
   X := One / Three;
   Z := Three / Nine;
   X := X - Z;
   T := Nine / TwentySeven;
   Z := Z - T;
   TestCondition (Defect, (X = Zero) and (Y = Zero)
         and (Z = Zero), 'Division error > ulp, 1/3 <> 3/9 <> 9/27');
   Y := F9 / One;
   X := F9 - Half;
   Y := (Y - Half) - X;
   X := One + U2;
   T := X / One;
   X := T - X;
   if (X = Zero) and (Y = Zero) and (Z = Zero) then
      GDiv := Yes
   else
      begin
      GDiv := No;
      TestCondition (SeriousDefect, false,
            '  Division lacks guard digit so X/1 <> X');
      end;
   X := One / (One + U2);
   Y := X - Half - Half;
   TestCondition (SeriousDefect, Y < Zero,
         '  Computed value of 1/1.000..1 >= 1.    ');
   X := One - U2;
   Y := One + Radix * U2;
   Z := X * Radix;
   T := Y * Radix;
   R := Z / Radix;
   StickyBit := T / Radix;
   X := R - X;
   Y := StickyBit - Y;
   TestCondition (Failure, (X = Zero) and (Y = Zero),
            ' * &or / gets too many last digits wrong');
   Y := One - U1;
   X := One - F9;
   Y := One - Y;
   T := Radix - U2;
   Z := Radix - BMinusU2;
   T := Radix - T;
   if (X = U1) and (Y = U1) and (Z = U2) and (T = U2) then
      GAddSub := Yes
   else
      begin
      GAddSub := No;
      TestCondition (SeriousDefect, false,
            '- lacks guard dig.,cancellation obscured');
      end;
   
   if (F9 <> One) and (F9 - One >= Zero) then begin
      TestCondition (SeriousDefect, false,
            'comparison alleges  (1-U1) < 1  although');
      writeln('  subtration yields  (1-U1) - 1 = 0 , thereby vitiating');
      writeln('  such precautions against division by zero as');
      writeln('  ...  if (X=1.0) then ..... else .../(X-1.0)...');
      end;
   if (GMult = Yes) and (GDiv = Yes) and (GAddSub = Yes) then
      writeln (' *, /, and - have guard digits, as they should.');
{=============================================}
   Milestone := 40;
{=============================================}
   Pause;
   writeln ('Checking rounding on multiply, divide and add/subtract.');
   RMult := Other;
   RDiv := Other;
   RAddSub := Other;
   RadixD2 := Radix / Two;
   A1 := Two;
   Done := false;
   repeat
      AInverse := Radix;
      repeat
         X := AInverse;
         AInverse := AInverse / A1;
      until Int (AInverse) <> AInverse;
      Done := (X = One) or (A1 > Three);
      if not Done then
         A1 := Nine + One;
   until Done;
   if X = One then
      A1 := Radix;
   AInverse := One / A1;
   X := A1;
   Y := AInverse;
   Done := false;
   repeat
      Z := X * Y - Half;
      TestCondition (Failure, Z = Half,
            '  X * (1/X) differs from 1.             ');
      Done := X = Radix;
      X := Radix;
      Y := One / X;
   until Done;
   Y2 := One + U2;
   Y1 := One - U2;
   X := OneAndHalf - U2;
   Y := OneAndHalf + U2;
   Z := (X - U2) * Y2;
   T := Y * Y1;
   Z := Z - X;
   T := T - X;
   X := X * Y2;
   Y := (Y + U2) * Y1;
   X := X - OneAndHalf;
   Y := Y - OneAndHalf;
   if (X = Zero) and (Y = Zero) and (Z = Zero) and (T <= Zero) then
      begin
      X := (OneAndHalf + U2) * Y2;
      Y := OneAndHalf - U2 - U2;
      Z := OneAndHalf + U2 + U2;
      T := (OneAndHalf - U2) * Y1;
      X := X - (Z + U2);
      StickyBit := Y * Y1;
      S := Z * Y2;
      T := T - Y;
      Y := (U2 - Y) + StickyBit;
      Z := S - (Z + U2 + U2);
      StickyBit := (Y2 + U2) * Y1;
      Y1 := Y2 * Y1;
      StickyBit := StickyBit - Y2;
      Y1 := Y1 - Half;
      if (X = Zero) and (Y = Zero) and (Z = Zero) and (T = Zero)
            and ( StickyBit = Zero) and (Y1 = Half) then
         begin
         RMult := Rounded;
         writeln ('Multiplication appears to round correctly.');
         end
      else if (X + U2 = Zero) and (Y < Zero) and (Z + U2 = Zero)
            and (T < Zero) and (StickyBit + U2 = Zero)
            and (Y1 < Half) then
         begin
         RMult := Chopped;
         writeln ('Multiplication appears to chop.');
         end
      else
         writeln ('* is neither chopped nor correctly rounded.');
      if (RMult = Rounded) and (GMult = No) then
         notify('multiplication');
      end
   else
      writeln ('* is neither chopped nor correctly rounded.');
{=============================================}
   Milestone := 45;
{=============================================}
   Y2 := One + U2;
   Y1 := One - U2;
   Z := OneAndHalf + U2 + U2;
   X := Z / Y2;
   T := OneAndHalf - U2 - U2;
   Y := (T - U2) / Y1;
   Z := (Z + U2) / Y2;
   X := X - OneAndHalf;
   Y := Y - T;
   T := T / Y1;
   Z := Z - (OneAndHalf + U2);
   T := (U2 - OneAndHalf) + T;
   if not ((X > Zero) or (Y > Zero) or (Z > Zero) or (T > Zero)) then
      begin
      X := OneAndHalf / Y2;
      Y := OneAndHalf - U2;
      Z := OneAndHalf + U2;
      X := X - Y;
      T := OneAndHalf / Y1;
      Y := Y / Y1;
      T := T - (Z + U2);
      Y := Y - Z;
      Z := Z / Y2;
      Y1 := (Y2 + U2) / Y2;
      Z := Z - OneAndHalf;
      Y2 := Y1 - Y2;
      Y1 := (F9 - U1) / F9;
      if (X = Zero) and (Y = Zero) and (Z = Zero) and (T = Zero)
            and (Y2 = Zero) and (Y2 = Zero)
            and (Y1 - Half = F9 - Half ) then
         begin
         RDiv := Rounded;
         writeln ('Division appears to round correctly.');
         if GDiv = No then notify('   division   ');
         end
      else if (X < Zero) and (Y < Zero) and (Z < Zero) and (T < Zero)
            and (Y2 < Zero) and (Y1 - Half < F9 - Half) then
         begin
         RDiv := Chopped;
         writeln ('Division appears to chop.');
         end;
      end;
   if RDiv = Other then
      writeln ('/ is neither chopped nor correctly rounded.');
   BInverse := One / Radix;
   TestCondition (Failure, (BInverse * Radix - Half = Half),
         '  Radix * ( 1 / Radix ) differs from 1. ');
{=============================================}
   Milestone := 50;
{=============================================}
   TestCondition (Failure, ((F9 + U1) - Half = Half)
         and ((BMinusU2 + U2 ) - One = Radix - One),
         'Incomplete carry-propagation in Addition');
   X := One - U1 * U1;
   Y := One + U2 * (One - U2);
   Z := F9 - Half;
   X := (X - Half) - Z;
   Y := Y - One;
   if (X = Zero) and (Y = Zero) then
      begin
      RAddSub := Chopped;
      writeln ('Add/Subtract appears to be chopped.');
      end;
   if GAddSub = Yes then
      begin
      X := (Half + U2) * U2;
      Y := (Half - U2) * U2;
      X := One + X;
      Y := One + Y;
      X := (One + U2) - X;
      Y := One - Y;
      if (X = Zero) and (Y = Zero) then
         begin
         X := (Half + U2) * U1;
         Y := (Half - U2) * U1;
         X := One - X;
         Y := One - Y;
         X := F9 - X;
         Y := One - Y;
         if (X = Zero) and (Y = Zero) then
            begin
            RAddSub := Rounded;
            writeln ('Addition/Subtraction appears to round correctly.');
            if GAddSub = No then notify(' add/subtract ');
            end
         else
            writeln ('Addition/Subtraction neither rounds nor chops.');
         end
      else
         writeln ('Addition/Subtraction neither rounds nor chops.');
      end
   else
      writeln ('Addition/Subtraction neither rounds nor chops.');
   S := One;
   X := One + Half * (One + Half);
   Y := (One + U2) * Half;
   Z := X - Y;
   T := Y - X;
   StickyBit := Z + T;
   if StickyBit <> 0 then
      begin
      S := 0;
      NoErrors [Flaw] := NoErrors [Flaw] + 1;
      write('FLAW:  (X - Y) + (Y - X) is non zero!');
      end;
   StickyBit := Zero;
   if (GMult = Yes) and (GDiv = Yes) and (GAddSub = Yes)
         and (RMult = Rounded) and (RDiv = Rounded)
         and (RAddSub = Rounded) and (Int (RadixD2) = RadixD2) then
      begin
      writeln (' Checking for sticky bit.');
      X := (Half + U1) * U2;
      Y := Half * U2;
      Z := One + Y;
      T := One + X;
      if (Z - One <= Zero) and (T - One >= U2) then
         begin
         Z := T + Y;
         Y := Z - X;
         if (Z - T >= U2) and (Y - T = Zero) then
            begin
            X := (Half + U1) * U1;
            Y := Half * U1;
            Z := One - Y;
            T := One - X;
            if (Z - One = Zero) and (T - F9 = Zero) then
               begin
               Z := (Half - U1) * U1;
               T := F9 - Z;
               Q := F9 - Y;
               if (T - F9 = Zero) and (F9 - U1 - Q = Zero) then
                  begin
                  Z := (One + U2) * OneAndHalf;
                  T := (OneAndHalf + U2) - Z + U2;
                  X := One + Half / Radix;
                  Y := One + Radix * U2;
                  Z := X * Y;
                  if (T = Zero) and (X + Radix * U2 - Z = Zero) then
                     begin
                     if Radix <> Two then
                        begin
                        X := Two + U2;
                        Y := X / Two;
                        if (Y - One = Zero) then
                           StickyBit := S;
                        end
                     else StickyBit := S;
                     end;
                  end;
               end;
            end;
         end;
      end;
   if StickyBit = One then
      writeln ('Sticky bit apparently used correctly.')
   else writeln ('Sticky bit used incorrectly or not at all.');
   if (GMult = No) or (GDiv = No) or (GAddSub = No) or (RMult = Other)
      or (RDiv = Other) or (RAddSub = Other) then begin
        TestCondition (Flaw, false,
             'lack(s) of guard digits or failure(s) to');
        writeln('correctly round or chop (noted above) count as one');
        writeln('flaw in the final tally below.')
        end;
     

{=============================================}
   Milestone := 60;
{=============================================}
   writeln;
   writeln ('Does Multiplication commute? Testing on ', NoTrials,
         ' random pairs.');
   R9 := sqrt (3.0);
   RandomNumber1 := Third;
   I := 1;
   repeat
      X := Random;
      Y := Random;
      Z9 := Y * X;
      Z := X * Y;
      Z9 := Z - Z9;
      I := I + 1;
   until (I > NoTrials) or (Z9 <> Zero);
   if I = NoTrials then
      begin
      RandomNumber1 := One + Half / Three;
      RandomNumber2 := (U2 + U1) + One;
      Z := RandomNumber1 * RandomNumber2;
      Y := RandomNumber2 * RandomNumber1;
      Z9 := (One + Half / Three) * ((U2 + U1) + One) - (One + Half /
            Three) * ((U2 + U1) + One);
      end;
   if not ((I = NoTrials) or (Z9 = Zero)) then
      begin
      NoErrors [Defect] := NoErrors [Defect] + 1;
      writeln ('DEFECT:  X * Y = Y * X trail fails.');
      end
   else
      writeln ('No failures found in ', NoTrials, ' integer pairs.');
{!!
   end;
end.
//GO.SYSIN DD unit1.pas
cat >unit2.pas <<'//GO.SYSIN DD unit2.pas'
unit Unit2;
interface
   uses mainvars;
   procedure mile70170;
implementation
   procedure mile70170;
   begin
 !!}
{=============================================}
   Milestone := 70;
{=============================================}
   writeln;
   writeln ('Running test of square root(x).');
   TestCondition (Failure, (Zero = sqrt (Zero))
         and (- Zero = sqrt (- Zero))
         and (One = sqrt (One)), ' Square root of 0.0, -0.0 or 1.0 wrong  '
         );
   MinSqrtError := Zero;
   MaxSqrtError := Zero;
   J := 0;
   X := Radix;
   OneUlp := U2;
   SqrtXMinX (SeriousDefect);
   X := BInverse;
   OneUlp := BInverse * U1;
   SqrtXMinX (SeriousDefect);
   X := U1;
   OneUlp := U1 * U1;
   SqrtXMinX (SeriousDefect);
   if J <> 0 then
      begin
      NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
      Pause;
      end;
   writeln ('Testing if sqrt(X * X) = X for ', NoTrials, ' integers X.');
   J := 0;
   X := Two;
   Y := Radix;
   if (Radix <> One) then
      repeat
         X := Y;
         Y := Radix * Y;
      until (Y - X >= NoTrials);
   OneUlp := X * U2;
   I := 1;
   Continue := true;
   while (I <= NoTrials) and Continue do (* dgh: 10 --> NoTrials *)
      begin
      X := X + One;
      SqrtXMinX (Defect);
      if J > 0 then
         begin
         Continue := false;
         NoErrors [Defect] := NoErrors [Defect] + 1;
         end;
      I := I + 1;
      end;
   writeln ('Test for Sqrt Monotonicity.');
   I := - 1;
   X := BMinusU2;
   Y := Radix;
   Z := Radix + Radix * U2;
   NotMonot := false;
   Monot := false;
   while not (NotMonot or Monot) do
      begin
      I := I + 1;
      X := sqrt (X);
      Q := sqrt (Y);
      Z := sqrt (Z);
      if (X > Q) or (Q > Z) then
         NotMonot := true
      else
         begin
         Q := Int (Q + Half);
         if not ((I > 0) or (Radix = Q * Q)) then
            Monot := true
         else if I > 0 then
            begin
            if I > 1 then
               Monot := true
            else
               begin
               Y := Y * BInverse;
               X := Y - U1;
               Z := Y + U1;
               end
            end
         else
            begin
            Y := Q;
            X := Y - U2;
            Z := Y + U2;
            end
         end
      end;
   if Monot then
      writeln ('Sqrt has passed a test for Monotonicity.')
   else
      begin
      NoErrors [Defect] := NoErrors [Defect] + 1;
      writeln('DEFECT:  Sqrt(X) is non-monotonic for X near ', Y);
      end;
{=============================================}
   Milestone := 80;
{=============================================}
   MinSqrtError := MinSqrtError + Half;
   MaxSqrtError := MaxSqrtError - Half;
   Y := (sqrt (One + U2) - One) / U2;
   SqrtError := (Y - One) + U2 / Eight;
   if SqrtError > MaxSqrtError then
      MaxSqrtError := SqrtError;
   SqrtError := Y + U2 / Eight;
   if SqrtError < MinSqrtError then
      MinSqrtError := SqrtError;
   Y := ((sqrt (F9) - U2) - (One - U2)) / U1;
   SqrtError := Y + U1 / Eight;
   if SqrtError > MaxSqrtError then
      MaxSqrtError := SqrtError;
   SqrtError := (Y + One) + U1 / Eight;
   if SqrtError < MinSqrtError then
      MinSqrtError := SqrtError;
   OneUlp := U2;
   X := OneUlp;
   for Index := 1 to 3 do
      begin
      Y := sqrt ((X + U1 + X) + F9);
      Y := ((Y - U2) - ((One - U2) + X)) / OneUlp;
      Z := ((U1 - X) + F9) * Half * X * X / OneUlp;
      SqrtError := (Y + Half) + Z;
      if SqrtError < MinSqrtError then
         MinSqrtError := SqrtError;
      SqrtError := (Y - Half) + Z;
      if SqrtError > MaxSqrtError then
         MaxSqrtError := SqrtError;
      if ((Index = 1) or (Index = 3)) then
         X := OneUlp * Sign (X) * Int (Eight / (Nine * sqrt (OneUlp)))
      else
         begin
         OneUlp := U1;
         X := - OneUlp;
         end;
      end;
{=============================================}
   Milestone := 85;
{=============================================}
   SquareRootWrong := false;
   AnomolousArithmetic := false;
   RSqrt := Other; (* ~dgh *)
   if Radix <> One then
      begin
      writeln ('Testing whether sqrt is rounded or chopped: ');
      D := Int (Half + Power (Radix, One + Precision - Int (Precision)))
         ;
   { ... = Radix^(1 + fract) if Precision = integer + fract. }
      X := D / Radix;
      Y := D / A1;
      if (X <> Int (X)) or (Y <> Int (Y)) then
         begin
         AnomolousArithmetic := true;
         end
      else
         begin
         X := Zero;
         Z2 := X;
         Y := One;
         Y2 := Y;
         Z1 := Radix - One;
         FourD := Four * D;
         repeat
            if Y2 > Z2 then
               begin
               Q := Radix;
               Y1 := Y;
               repeat
                  X1 := abs (Q + Int (Half - Q / Y1) * Y1);
                  Q := Y1;
                  Y1 := X1;
               until X1 <= Zero;
               if Q <= One then
                  begin
                  Z2 := Y2;
                  Z := Y;
                  end;
               end;
            Y := Y + Two;
            X := X + Eight;
            Y2 := Y2 + X;
            if Y2 >= FourD then
               Y2 := Y2 - FourD;
         until Y >= D;
         X8 := FourD - Z2;
         Q := (X8 + Z * Z) / FourD;
         X8 := X8 / Eight;
         if Q <> Int (Q) then
            AnomolousArithmetic := true
         else
            begin
            Break := false;
            repeat
               X := Z1 * Z;
               X := X - Int (X / Radix) * Radix;
               if X = One then
                  Break := true
               else
                  Z1 := Z1 - One;
            until Break or (Z1 <= 0);
            if (Z1 <= 0) and (not Break) then
               AnomolousArithmetic := true
            else
               begin
               if Z1 > RadixD2 then
                  Z1 := Z1 - Radix;
               repeat
                  NewD;
               until U2 * D >= F9;
               if D * Radix - D <> W - D then
                  AnomolousArithmetic := true
               else
                  begin
                  Z2 := D;
                  I := 0;
                  Y := D + (One + Z) * Half;
                  X := D + Z + Q;
                  SubRout3750;
                  Y := D + (One - Z) * Half + D;
                  X := D - Z + D;
                  X := X + Q + X;
                  SubRout3750;
                  NewD;
                  if D - Z2 <> W - Z2 then
                     AnomolousArithmetic := true
                  else
                     begin
                     Y := (D - Z2) + (Z2 + (One - Z) * Half);
                     X := (D - Z2) + (Z2 - Z + Q);
                     SubRout3750;
                     Y := (One + Z) * Half;
                     X := Q;
                     SubRout3750;
                     if I = 0 then
                        AnomolousArithmetic := true;
                     end
                  end
               end
            end
         end;
      if (I = 0) or AnomolousArithmetic then
         begin
         NoErrors [Failure] := NoErrors [Failure] + 1;
         writeln ('FAILURE:  Anomolous arithmetic with ',
            'integer < Radix^Precision = ');
         writeln (W, '  fails test whether sqrt rounds or chops.');
         SquareRootWrong := true;
         end
      end;
   if not AnomolousArithmetic then
      begin
      if not ((MinSqrtError < 0) or (MaxSqrtError > 0)) then
         begin
         RSqrt := Rounded;
         writeln ('Square root appears to be correctly rounded.');
         end
      else if (MaxSqrtError + U2 > U2 - Half) or (MinSqrtError > Half)
            or (MinSqrtError + Radix < Half) then
         SquareRootWrong := true
      else
         begin
         RSqrt := Chopped;
         writeln ('Square root appears to be chopped.');
         end;
      end;
   if SquareRootWrong then
      begin
      writeln ('Square root is neither chopped nor correctly rounded.');
      writeln ('Observed errors run from ', MinSqrtError - Half);
      writeln ('to ', Half + MaxSqrtError, ' ulps.');
      TestCondition (SeriousDefect, MaxSqrtError - MinSqrtError < Radix
            * Radix, 'sqrt gets too many last digits wrong.   ');
      end;
{=============================================}
   Milestone := 90;
{=============================================}
   Pause;
   (* fix from dgh: Wichman had effectively commented out here to Milestone 110 *)
      writeln ('Testing powers Z^i for small integers Z and i.');
      N := 0;
   { ... test power of zero. }
      I := 0;
      Z := - Zero;
      M := 3;
      Break := false;
      repeat
         X := One;
         SubRout3980;
         if I <= 10 then
            begin
            I := 1023;
            SubRout3980;
            end;
         if Z = MinusOne then
            Break := true
         else
            begin
            Z := MinusOne;
         { .. if(-1)^N is invalid, replace MinusOne by One. }
            I := - 4;
            end;
      until Break;
      PrintIfNPositive;
      N1 := N;
      N := 0;
      Z := A1;
      M := Int (Two * ln (W) / ln (A1));
      Break := false;
      repeat
         X := Z;
         I := 1;
         SubRout3980;
         if Z = AInverse then
            Break := true
         else
            Z := AInverse;
      until Break;
   {=============================================}
      Milestone := 100;
   {=============================================}
   {  Power of Radix have been tested, }
   {         next try a few primes     }
      M := NoTrials;
      Z := Three;
      repeat
         X := Z;
         I := 1;
         SubRout3980;
         repeat
            Z := Z + Two;
         until (Three * Int (Z / Three) <> Z);
      until (Z >= Eight * Three);
      if N > 0 then
         begin
         writeln ('Error like this may invalidate financial ');
         writeln ('calculations involving interest rates.');
         end;
      PrintIfNPositive;
      N := N + N1;
      if N = 0 then
         writeln ('... no discrepancies found.');
      writeln;
      if N > 0 then
         Pause;
{=============================================}
   Milestone := 110;
{=============================================}
   writeln ('Seeking Underflow thresholds UnderflowThreshold and E0');
   D := U1;
   if (Precision <> Int (Precision)) then
      begin
      D := BInverse;
      X := Precision;
      repeat
         D := D * BInverse;
         X := X - One;
      until X <= Zero;
      end;
   Y := One;
   Z := D;
{ ... D is power of 1/Radix < 1. }
   repeat
      C := Y;
      Y := Z;
      Z := Y * Y;
   until not ((Y > Z) and (Z + Z > Z));
   Y := C;
   Z := Y * D;
   repeat
      C := Y;
      Y := Z;
      Z := Y * D;
   until not ((Y > Z) and (Z + Z > Z));
   if Radix < Two then
      HInverse := Two
   else
      HInverse := Radix;
   H := One / HInverse;
{ ... 1/HInverse = H = Min(1/Radix, 1/2) }
   CInverse := One / C;
   E0 := C;
   Z := E0 * H;
{ ...1/Radix^(BIG integer) << 1 << CInverse = 1/C }
   repeat
      Y := E0;
      E0 := Z;
      Z := E0 * H;
   until not ((E0 > Z) and (Z + Z > Z));
   UnderflowThreshold := E0;
   E1 := Zero;
   Q := Zero;
   E9 := U2;
   S := One + E9;
   D := C * S;

   if D <= C then
      begin
      E9 := Radix * U2;
      S := One + E9;
      D := C * S;
      if D <= C then
         begin
         writeln ('FAILURE:  multiplication gets too many last digits wrong.');
         NoErrors [Failure] := NoErrors [Failure] + 1;
         Underflow := E0;
         Y1 := Zero;
         PseudoZero := Z;
         Pause;
         end
      end
   else
      begin
      Underflow := D;
      PseudoZero := Underflow * H;
      UnderflowThreshold := Zero;
      repeat
         Y1 := Underflow;
         Underflow := PseudoZero;
         if E1 + E1 <= E1 then
            begin
            Y2 := Underflow * HInverse;
            E1 := abs (Y1 - Y2);
            Q := Y1;
            if (UnderflowThreshold = Zero) and (Y1 <> Y2) then
               UnderflowThreshold := Y1;
            end;
         PseudoZero := PseudoZero * H;
      until not ((Underflow > PseudoZero)
            and (PseudoZero + PseudoZero > PseudoZero));
      end;
{ Comment line 4530 .. 4560 }
   if PseudoZero <> Zero then
      begin
      writeln;
      Z := PseudoZero;
   { ... Test PseudoZero for "phoney-zero" violating }
   { ... PseudoZero < Underflow or PseudoZero < PseudoZero + PseudoZero
         ... }
      if PseudoZero <= 0 then
         begin
         NoErrors [Failure] := NoErrors [Failure] + 1;
         writeln ('FAILURE:  Positive expressions can underflow to an ');
         writeln ('allegedly negative value PseudoZero that prints out as:');
         writeln (PseudoZero);
         X := - PseudoZero;
         if X <= 0 then
            begin
            writeln ('But -PseudoZero, which should then be positive, isn''t;');
            writeln (' it prints out as: ', X);
            end
         end
      else
         begin
         NoErrors [Flaw] := NoErrors [Flaw] + 1;
         writeln ('FLAW:  Underflow can stick at an allegedly positive');
         writeln ('value PseudoZero that prints out as: ', PseudoZero);
         end;
      TestPartialUnderflow;
      end;
{=============================================}
   Milestone := 120;
{=============================================}
   if (CInverse * Y > CInverse * Y1) then
      begin
      S := H * S;
      E0 := Underflow;
      end;
   if not ((E1 = 0) or (E1 = E0)) then
      begin
      NoErrors [Defect] := NoErrors [Defect] + 1;
      write ('DEFECT:  ');
      if E1 < E0 then
         begin
         writeln ('Products underflow at a higher');
         writeln ('threshold than differences.');
         if PseudoZero = Zero then
            E0 := E1;
         end
      else
         begin
         writeln ('Difference underflows at a higher');
         writeln ('threshold than products.');
         end;
      end;
   writeln ('Smallest strictly positive number found is E0 =', E0);
   Z := E0;
   TestPartialUnderflow;
   Underflow := E0;
   if N = 1 then
      Underflow := Y;
   I := 4;
   if E1 = Zero then
      I := 3;
   if UnderflowThreshold = Zero then
      I := I - 2;
   UnderflowNotGradual := true;
   case I of
      1:
         begin
         UnderflowThreshold := Underflow;
         if (CInverse * Q) <> ((CInverse * Y) * S) then
            begin
            NoErrors [Failure] := NoErrors [Failure] + 1;
            UnderflowThreshold := Y;
            writeln ('FAILURE:  Either accuracy deteriorates as numbers');
            writeln ('approach a threshold UnderflowThreshold = ',
                  UnderflowThreshold);
            writeln ('coming down from ', C, ' or else multiplication');
            writeln ('gets too many last digits wrong.');
            end;
         Pause;
         end;
      2:
         begin
         NoErrors [Failure] := NoErrors [Failure] + 1;
         writeln ('FAILURE:  Underflow confuses Comparison, which alleges that');
         writeln ('Q = Y while denying that |Q - Y| = 0 ; these values');
         write ('print out as Q = ', Q, ' Y = ', Y2, ' |Q - Y| = ');
         writeln (abs (Q - Y2));
         UnderflowThreshold := Q;
         end;
      3:
         begin
         X := X;
      {FOR PRETTYPRINTER, IS DUMMY}
         end;
      4:
         if (Q = UnderflowThreshold) and (E1 = E0)
               and (abs ( UnderflowThreshold - E1 / E9) <= E1) then
            begin
            UnderflowNotGradual := false;
            writeln ('Underflow is gradual; it incurs Absolute Error =')
               ;
            writeln ('(roundoff in UnderflowThreshold) < E0.');
            Y := E0 * CInverse;
            Y := Y * (OneAndHalf + U2);
            X := CInverse * (One + U2);
            Y := Y / X;
            IEEE := (Y = E0);
            end;
      end;
   if UnderflowNotGradual then
      begin
      writeln;
      R := sqrt (Underflow / UnderflowThreshold);
      if R <= H then
         begin
         Z := R * UnderflowThreshold;
         X := Z * (One + R * H * (One + H));
         end
      else
         begin
         Z := UnderflowThreshold;
         X := Z * (One + H * H * (One + H));
         end;
      if not ((X = Z) or (X - Z <> 0)) then
         begin
         NoErrors [Flaw] := NoErrors [Flaw] + 1;
         writeln ('FLAW:  X = ', X, ' is unequal to Z = ', Z);
         write ('yet X - Z yields ');
         Z9 := X - Z;
         writeln (Z9, '.  Should this NOT signal Underflow,');
         writeln ('this is a SERIOUS DEFECT that causes');
         writeln ('confusion when innocent statements like');
         write ('    if X = Z then  ...  else');
         writeln ('  ...  (f(X) - f(Z)) / (X - Z) ...');
         writeln ('encounter Division by Zero although actually');
         writeln ('X / Z = 1 + ', (X / Z - Half) - Half);
         end;
      end;
   writeln ('The Underflow threshold is ', UnderflowThreshold,
         ' below which');
   writeln ('calculation may suffer larger Relative error then',
         ' merely roundoff.');
   Y2 := U1 * U1;
   Y := Y2 * Y2;
   Y2 := Y * U1;
   if Y2 <= UnderflowThreshold then
      begin
      if Y > E0 then
         begin
         NoErrors [Defect] := NoErrors [Defect] + 1;
         I := 5;
         end
      else
         begin
         NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
         write ('SERIOUS ');
         I := 4;
         end;
      writeln ('DEFECT: Range is too narrow; U1^', I, ' Underflows.');
      end;
{=============================================}
   Milestone := 130;
{=============================================}
{ SKIP
   Y := - Int (Half - TwoForty * ln (UnderflowThreshold) / ln (HInverse)
) / TwoForty;
   Y2 := Y + Y;
   writeln ('Since underflow occurs below the threshold');
   writeln ('UnderflowThreshold = ( ', HInverse, ' ) ^ ( ', Y, ') only u
nderflow');
   writeln ('should afflict the expression ( ', HInverse, ' ) ^ ( ', Y2,
         ' )');
   write ('actually calculating yields: ');
   V9 := Power (HInverse, Y2);
   writeln (V9);
   if not ((V9 >= 0) and (V9 <= (Radix + Radix + E9) * UnderflowThreshol
d)) then
      begin
      NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
      writeln ('SERIOUS DEFECT:  this is not between 0 and');
      writeln (' UnderflowThreshold = ', UnderflowThreshold);
      end
   else if not (V9 > UnderflowThreshold * (One + E9)) then
      writeln ('this computed value is O.K.')
   else
      begin
      NoErrors [Defect] := NoErrors [Defect] + 1;
      writeln ('DEFECT:  This is not between 0 and UnderflowThreshold = ',
               UnderflowThreshold);
      end;
end SKIP }
{=============================================}
   Milestone := 140;
{=============================================}
   writeln;
{ ...calculate Exp2 = exp(2) = 7.389056099... }
   X := 0;
   I := 2;
   Y := Two * Three;
   Q := 0;
   N := 0;
   repeat
      Z := X;
      I := I + 1;
      Y := Y / (I + I);
      R := Y + Q;
      X := Z + R;
      Q := (Z - X) + R;
   until X <= Z;
   Z := (OneAndHalf + One / Eight) + X / (OneAndHalf * ThirtyTwo);
   X := Z * Z;
   Exp2 := X * X;
   X := F9;
   Y := X - U1;
   write ('Testing X^((X + 1) / (X - 1)) vs. exp(2) = ');
   writeln (Exp2, ' as X -> 1.');
   Break := false;
   I := 1;
   while (not Break) do
      begin
      Z := X - BInverse;
      Z := (X + One) / (Z - (One - BInverse));
      Q := Power (X, Z) - Exp2;
      if abs (Q) > TwoForty * U2 then
         begin
         Break := true;
         N := 1;
         NoErrors [Defect] := NoErrors [Defect] + 1;
         writeln ('DEFECT:  Calculated ', Power(X,Z), ' for');
         writeln (' (1 + (', (X - BInverse) - (One - BInverse),
              ')) ^ (', Z, ')');
         writeln (' which differs from the correct value by Q =', Q);
         writeln (' This much error may spoil financial',
              ' calculations involving');
         writeln (' tiny interest rates.');
         end
      else
         begin
         Z := (Y - X) * Two + Y;
         X := Y;
         Y := Z;
         Z := One + (X - F9) * (X - F9);
         if ((Z > One) and (I < NoTrials)) then
            I := I + 1
         else if X > One then
            begin
            if N = 0 then
               writeln ('Accuracy seems adequate.');
            Break := true;
            end
         else
            begin
            X := One + U2;
            Y := U2 + U2 + X;
            I := 1;
            end
         end
      end;

{=============================================}
   Milestone := 150;
   {=============================================}
   writeln ('Testing powers Z^Q at four nearly extreme values.');
   N := 0;
   Z := A1;
   Q := Int (Half - ln (C) / ln (A1));
   Break := false;
   repeat
      X := CInverse;
      Y := Power (Z, Q);
      DoesYequalX;
      Q := - Q;
      X := C;
      Y := Power (Z, Q);
      DoesYequalX;
      if Z < One then
         Break := true
      else
         Z := AInverse;
   until Break;
   PrintIfNPositive;
   if N = 0 then
      writeln (' ... no discrepancies found.');
   writeln;
   if N > 0 then
      Pause;
{=============================================}
   Milestone := 160;
{=============================================}
   Pause;
   writeln ('Searching for Overflow threshold:  ',
        'This may generate an error.');
   writeln ('Try a few values for N, starting with a large one,');
   writeln ('and take the one that just does not stop the machine.');
   writeln ('Did you find the correct value for N yet?');
   readln (input);
   while not eoln (input) do
      read (input, ch);
   Break := false;
   repeat
      writeln ('N = ');
      readln (input);
      while not eoln (input) do
         read (input, NoTimes);
      Y := - CInverse;
      V9 := HInverse * Y;
      Index := 1;
      I := 0;
      repeat
         V := Y;
         Y := V9;
         V9 := HInverse * Y;
         if V9 >= Y then begin I := 1; ch := 'Y'; Index := NoTimes end;
         Index := Index + 1;
         until Index > NoTimes;
      if I = 0 then Y := V9;

      if (ch = 'N') or (ch = 'n') then
         writeln ('N seems not large enough, try again.')
      else
         begin
         writeln ('O.K.');
         Break := true;
         end;
   until Break;
   Z := V9;
   writeln ('Can "Z = -Y" overflow?  Trying it on Y = ', Y);
   V9 := - Y;
   V0 := V9;
   if (V - Y = V + V0) then
      writeln ('Seems O.K.')
   else
      begin
      NoErrors [Flaw] := NoErrors [Flaw] + 1;
      writeln ('Finds a FLAW:  -(-Y) differs from Y.');
      end;
   if Z <> Y then
      begin
      NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
      writeln ('SERIOUS DEFECT:');
      writeln ('  overflow past ', Y, ' shrinks to ', Z);
      end;
   if (I = 1) then
      begin
      Y := V * (HInverse * U2 - HInverse);
      Z := Y + ((One - HInverse) * U2) * V;
      if Z < V0 then
         Y := Z;
      if Y < V0 then
         V := Y;
      if V0 - V < V0 then
         V := V0;
      end
   else begin
      V := Y * (HInverse * U2 - HInverse);
      V := V + ((One - HInverse) * U2) * Y;
      end;
   writeln ('Overflow threshold is V = ', V);
   if (I = 1) then writeln ('Overflow saturates at V0 = ', V0)
      else begin writeln('There is no saturation value because');
          writeln('the system traps on overflow.') end;
   write ('No Overflow should get signaled for V * 1 = ');
   V9 := V * One;
   writeln (V9);
   write ('                            nor for V / 1 = ');
   V9 := V / One;
   writeln (V9);
   writeln ('Any overflow signal separating this * from the one');
   writeln ('above is a DEFECT.');
{=============================================}
   Milestone := 170;
{=============================================}
   TestCondition (Failure, (- V < V) and (- V0 < V0)
         and (- UnderflowThreshold < V)
         and (UnderflowThreshold < V),
           'Comparisons are confused by Overflow.   ');
{!!
   end;
end.
//GO.SYSIN DD unit2.pas
cat >par.pas <<'//GO.SYSIN DD par.pas'
program paranoia(input,output);
uses mainvars, Unit1, Unit2;

   begin (*PARA*)
   start;
   mile2060;
   mile70170;
 !!}

{=============================================}
   Milestone := 175;
{=============================================}
   writeln;
   for Index := 1 to 3 do
      begin
      case Index of
         1:
            Z := UnderflowThreshold;
         2:
            Z := E0;
         3:
            Z := PseudoZero;
         end;
      if Z <> 0 then
         begin
         V9 := sqrt (Z);
         Y := V9 * V9;
         if (Y / (One - Radix * E9) < Z)
               or (Y > (One + Radix * E9) * Z) then (* dgh: + E9 --> * E9 *)
            begin
            if V9 > U1 then
               begin
               NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
               write ('SERIOUS DEFECT:');
               end
            else
               begin
               NoErrors [Defect] := NoErrors [Defect] + 1;
               write ('DEFECT:');
               end;
            writeln ('  Comparison alleges that what prints as Z = ', Z);
            writeln ('is too far from sqrt(Z) ^ 2 = ', Y);
            end;
         end;
      end;

{=============================================}
   Milestone := 180;
{=============================================}
      for Index := 1 to 2 do
         begin
         if Index = 1 then
            Z := V
         else
            Z := V0;
         V9 := sqrt (Z);
         X := (One - Radix * E9) * V9;
         V9 := V9 * X;
         if ((V9 < (One - Two * Radix * E9) * Z) or (V9 > Z)) then
            begin
            Y := V9;
            if X < W then
               begin
               NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
               write ('SERIOUS ');
               end
            else
               NoErrors [Defect] := NoErrors [Defect] + 1;
            writeln ('DEFECT:  Comparison alleges that Z = ', Z);
            writeln ('is too far from sqrt(Z) ^ 2 is: ', Y);
            end;
         end;
{=============================================}
   Milestone := 190;
{=============================================}
   Pause;
   X := UnderflowThreshold * V;
   Y := Radix * Radix;
   if not ((X * Y >= One) and (X <= Y)) then
      begin
      if ((X * Y >= U1) and (X <= Y / U1)) then
         begin
         NoErrors [Flaw] := NoErrors [Flaw] + 1;
         write ('FLAW:');
         end
      else
         begin
         NoErrors [Defect] := NoErrors [Defect] + 1;
         write ('DEFECT: Badly');
         end;
      writeln (' unbalanced range; UnderflowThreshold * V = ');
      writeln (X, ' is too far from 1 .');
      end;
{=============================================}
   Milestone := 200;
{=============================================}
   for Index := 1 to 5 do
      begin
      X := F9;
      case Index of
         1:
            begin { Dummy Body }
            X := X;
            end;
         2:
            X := One + U2;
         3:
            X := V;
         4:
            X := UnderflowThreshold;
         5:
            X := Radix;
         end;
      Y := X;
      V9 := (Y / X - Half) - Half;
      if V9 <> 0 then
         begin
         if (V9 = - U1) and (Index < 5) then
            begin
            NoErrors [Flaw] := NoErrors [Flaw] + 1;
            write ('FLAW:');
            end
         else
            begin
            NoErrors [SeriousDefect] := NoErrors [SeriousDefect] + 1;
            write ('SERIOUS DEFECT:');
            end;
         writeln ('  X / X differs from 1 when X = ', X);
         writeln ('  instead, X / X - 1/2 - 1/2 = ', V9);
         writeln;
         end;
      end;
{=============================================}
   Milestone := 210;
{=============================================}
   MyZero := 0;
   writeln;
   writeln ('What message and/or values does Division by Zero produce?')
      ;
   writeln ('This can interupt your program. You can ',
         'skip this part if you wish.');
   writeln ('Do you wish to compute 1 / 0? ');
   readln (input);
   read (input, ch);
   if (ch = 'Y') or (ch = 'y') then
      writeln ('Trying to compute 1 / 0 produces: ', One / MyZero)
   else
      writeln ('O.K.');
   writeln ('Do you wish to compute 0 / 0?');
   readln (input);
   read (input, ch);
   if (ch = 'Y') or (ch = 'y') then
      writeln ('Trying to compute 0 / 0 produces: ', MyZero / MyZero)
   else
      writeln ('O.K.');
{=============================================}
   Milestone := 220;
{=============================================}
   Pause;
   writeln;
   if NoErrors[Failure] > 0 then begin
      write ('The number of  FAILUREs  encountered =        ');
      writeln (NoErrors [Failure]);
      end;
   if NoErrors[SeriousDefect] > 0 then begin
      write ('The number of  SERIOUS DEFECTs  encountered = ');
      writeln (NoErrors [SeriousDefect]);
      end;
   if NoErrors[Defect] > 0 then begin
      write ('The number of  DEFECTs  encountered =         ');
      writeln (NoErrors [Defect]);
      end;
   if NoErrors[Flaw] > 0 then begin
      write ('The number of  FLAWs  encountered =           ');
      writeln (NoErrors [Flaw]);
      end;
   if (NoErrors [Failure] + NoErrors [SeriousDefect] + NoErrors [Defect]
         + NoErrors [Flaw]) > 0 then
      begin
      writeln;
      if (NoErrors [Failure] + NoErrors [SeriousDefect] + NoErrors [
            Defect] = 0) and (NoErrors [Flaw] > 0) then
         begin
         write ('The arithmetic diagnosed seems ');
         writeln ('Satisfactory though flawed.');
         end;
      if (NoErrors [Failure] + NoErrors [SeriousDefect] = 0)
            and ( NoErrors [Defect] > 0) then
         begin
         writeln ('The arithmetic diagnosed may be Acceptable');
         writeln ('despite inconvenient Defects.');
         end;
         (* dgh: Defect --> SeriousDefect in next line *)
      if (NoErrors [Failure] + NoErrors [SeriousDefect] > 0) then
         begin
         write ('The arithmetic diagnosed has ');
         writeln ('unacceptable Serious Defects.');
         end;
      if (NoErrors [Failure] > 0) then
         writeln ('Potentially fatal FAILURE may have spoiled this',
                  ' program''s subsequent diagnoses.');
      end
   else
      begin
      writeln ('No failures, defects nor flaws have been discovered.');
      if not ((RMult = Rounded) and (RDiv = Rounded)
            and (RAddSub = Rounded) and (RSqrt = Rounded)) then
         writeln ('The arithmetic diagnosed seems Satisfactory.')
      else begin
        if (StickyBit >= One)
            and ((Radix - Two) * (Radix - Nine - One) = 0) then begin
         write ('Rounding appears to conform to ');
         write ('the proposed IEEE standard P');
         if (Radix = Two)
               and ((Precision - Four * Three * Two) * ( Precision -
               TwentySeven - TwentySeven + One) = Zero) then
            write ('754')
         else
            write ('854');
         if IEEE then writeln('.')
          else begin
            writeln(',');
            writeln ('except possibly for Double Rounding',
               ' during Gradual Underflow.');
            end;
         end;
        writeln ('The arithmetic diagnosed appears to be Excellent!')
        end;
      end;
   writeln ('END OF TEST.');
   end (* PARA *).
{!!
//GO.SYSIN DD par.pas
 !!}
