{ Whole-record and whole-array assignment (issue #104's remaining gap):
  Structured_Type is the one place it is a legal operand at all, and only
  when both sides name the SAME record or array -- Structure_Index has to
  match, not just the shared Structured_Type code, or two unrelated record
  types would wrongly type-check against each other.

  Covers: a plain record copy, an array copy, a copy through a type alias of
  a record (which shares the aliased type's Structure_Index, not a new one),
  and copying into and out of a record field that is itself a record.
  Expect NO errors.

  Check with: bin/aquarius --check test_structured_assignment.pas }

program Test_Structured_Assignment;

type
   Point = record
      x, y : integer
   end;

   Segment = record
      p1, p2 : Point
   end;

   Row = array [1 .. 10] of integer;

   Coordinate = Point;

var
   a, b   : Point;
   s      : Segment;
   r1, r2 : Row;
   c      : Coordinate;

begin
   b := a;               { record copy }
   r2 := r1;             { array copy }
   c := a;               { copy through a type alias -- same structure }
   a := c;               { and back }
   s.p1 := a;            { copy into a field that is itself a record }
   b := s.p2             { copy out of one }
end.
