{ The required standard functions (issue #83): abs, arctan, cos, exp, ln,
  sin, sqr, sqrt, round, trunc, chr, ord, pred, succ, odd, eof and eoln. Each
  resolves as a call like any other (issue #105), but none has a fixed
  Signature: abs, sqr, pred and succ return whatever type they were given,
  rather than one type a Signature could name.

  Covers: abs and sqr on both an integer and a real, the functions that
  always yield a real (and accept an integer too, since it widens), round
  and trunc narrowing a real to an integer, chr and ord converting between
  integer and char, ord on a boolean and an enumerated type, pred and succ
  preserving an ordinal's own type -- integer, char and an enumerated type --
  odd, and eof/eoln on a file, whose type this system does not model.
  Expect NO errors.

  Check with: bin/aquarius --check test_standard_functions.pas }

program Test_Standard_Functions;

type
   Colour = (red, green, blue);

var
   i, j : integer;
   r, s : real;
   c    : char;
   flag : boolean;
   col  : Colour;
   f    : text;

begin
   i := abs(-3);
   r := abs(-3.5);
   i := sqr(4);
   s := sqr(2.5);

   r := arctan(r);
   r := cos(r);
   r := exp(r);
   r := ln(r);
   r := sin(r);
   r := sqrt(r);
   r := sqrt(i);         { an integer widens to real }

   j := round(r);
   j := trunc(r);

   c := chr(65);
   i := ord(c);
   i := ord(flag);
   i := ord(col);

   c := succ(c);
   c := pred(c);
   j := succ(i);
   j := pred(i);
   col := succ(red);
   col := pred(blue);

   flag := odd(j);

   flag := eof(f);
   flag := eoln(f)
end.
