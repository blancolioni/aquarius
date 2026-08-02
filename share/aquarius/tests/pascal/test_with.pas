{ 'with' as a scope (issue #127). Expect exactly ONE error -- everything else
  here is a valid use of 'with' and should produce nothing.

     with requires a record: i is not a record

  Covers: a plain record variable, a nested record's field written through the
  enclosing record's own with, a NESTED with ('with a, b do', which opens one
  scope per variable, innermost last), a with-variable that is itself an
  array element (the issue's own galaxy[i, j] example), a with-bound field
  shadowing an outer variable of the same name, and 'with' on something that
  is not a record -- reported once, with the body still resolving names
  against the ENCLOSING scope rather than cascading into "undeclared".

  Check with: bin/aquarius --check test_with.pas }

program Test_With;

type
   Point = record
      x, y : integer
   end;

   Segment = record
      p1, p2 : Point;
      len    : integer
   end;

   Points = array [1 .. 3] of Point;

var
   p  : Point;
   s  : Segment;
   ps : Points;
   i  : integer;
   x  : integer;

begin
   x := 99;

   with p do
   begin
      x := 1;             { shadows the outer x: this is p.x }
      y := 2
   end;

   with s do
   begin
      len := 10;
      p1.x := 3;
      p1.y := p1.x
   end;

   with s.p1, s.p2 do
   begin
      x := 5;              { innermost wins: s.p2.x }
      y := x
   end;

   i := 1;
   with ps[i] do
   begin
      x := 7;
      y := 8
   end;

   with i do                { error }
      x := 0                { the outer x again: no scope was opened }
end.
