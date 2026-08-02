{ Subscript resolution against the structural type model (issue #126). Expect
  exactly FIVE errors, each a deliberately broken subscript -- everything else
  here is valid and should produce nothing: a one-dimensional array, a
  two-dimensional one written 'g[i, j]', an array of records (chaining a
  subscript into a field selection), and a non-integer index type.

     too many subscripts for r         -- r is one-dimensional
     not enough subscripts for g       -- g is two-dimensional
     subscript 1 of r must be ordinal  -- a real index
     subscript 1 of ls is char, but integer was given
     cannot subscript i: not an array  -- i is a scalar

  Check with: bin/aquarius --check test_subscripts.pas }

program Test_Subscripts;

type
   Row = array [1 .. 5] of integer;
   Grid = array [1 .. 2, 1 .. 3] of integer;
   Letters = array ['a' .. 'z'] of integer;
   Point = record
      x, y : integer
   end;
   Points = array [1 .. 3] of Point;

var
   i, j : integer;
   r    : Row;
   g    : Grid;
   ls   : Letters;
   ps   : Points;
   c    : char;

begin
   r[1] := 10;
   i := r[2] + r[3];

   g[1, 2] := 5;
   i := g[2, 1];

   ps[1].x := 1;
   ps[2].y := ps[1].x;

   c := 'a';
   ls[c] := 7;

   i := r[1, 2];            { error }
   i := g[1];                { error }
   i := r[3.14];             { error }
   i := ls[5];               { error }
   i := i[1]                 { error }
end.
