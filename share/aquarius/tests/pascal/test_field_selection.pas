{ Field selection against the structural type model (issue #125). Expect
  exactly THREE errors, each a deliberately broken chain -- everything else
  here is a valid field selection and should produce nothing, which is the
  point: a resolved field now types like any other value, read, written,
  passed through a var parameter, or used in an operator.

     no field z in p                             -- p has no such field
     cannot select a field of i: not a record     -- i is a scalar
     cannot assign real to integer                -- p.x is integer, ordinary
                                                       type check on a FIELD's
                                                       type, not a field-
                                                       selection error

  Also covers: a nested record (Segment.p1.x), a var parameter of record type
  (Move's pt -- this used to report "not a record" until a parameter's
  type_identifier carried its Structure_Index too), and a record with a typed
  variant discriminant (Reading.tag, itself a real field since issue #125).

  Check with: bin/aquarius --check test_field_selection.pas }

program Test_Field_Selection;

type
   Point = record
      x, y : integer
   end;

   Segment = record
      p1, p2 : Point
   end;

   Reading = record
      kind : integer;
      case tag : integer of
         0 : (whole : integer);
         1 : (approx : real)
   end;

var
   i  : integer;
   r  : real;
   p  : Point;
   s  : Segment;
   rd : Reading;

procedure Move(var pt : Point; dx, dy : integer);
begin
   pt.x := pt.x + dx;
   pt.y := pt.y + dy
end;

begin
   p.x := 1;
   p.y := 2;
   i := p.x + p.y;

   s.p1.x := 3;
   s.p1.y := s.p1.x;

   Move(p, 1, 1);

   rd.kind := 1;
   rd.tag := 0;
   rd.whole := 5;
   rd.tag := 1;
   r := rd.approx;

   i := p.z;               { error }
   i := i.x;                { error }
   p.x := r                { error }
end.
