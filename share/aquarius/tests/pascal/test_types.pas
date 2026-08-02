{ The minimal type system: integer, real, boolean and char (issue #88).
  Everything here is well typed. Expect NO errors.

  Covers: a declared type on every variable, an alias of a built-in, the two
  divisions, mixed arithmetic, integer-to-real promotion, comparisons, boolean
  operators (including the true/false constants, issue #88), an ordinal
  control variable, and a call checked against a typed parameter list.

  Check with: bin/aquarius --check test_types.pas }

program Test_Types;

const
   Limit  = 10;
   Letter = 'K';

type
   Counter = integer;

var
   i, j : integer;
   n    : Counter;
   r, s : real;
   flag : boolean;
   c    : char;

function Scale(x : real; by : integer) : real;
begin
   writeln(x)
end;

begin
   i := 42;
   j := i;
   n := Limit;

   r := 3.14;
   s := r;
   r := i;                  { integer widens to real }
   r := i + 1;
   s := r * 2;              { mixed arithmetic yields a real }
   s := i / j;              { '/' is real division, even on integers }
   i := i div j;
   i := i mod j;

   c := 'a';
   c := Letter;

   flag := i > 0;
   flag := r <= s;
   flag := i < r;           { integer compares against real }
   flag := c = 'z';
   flag := flag and (i = j);
   flag := not flag;
   flag := (i > 0) or (j > 0);
   flag := true;
   flag := false;
   flag := true and not false;

   for i := 1 to Limit do
      j := j + i;

   for c := 'a' to 'z' do
      j := j + 1;

   s := Scale(r, i);
   writeln(s)
end.
