{ A structural type model for records and arrays (issue #124).

  This builds a model for every record and array declared here -- fields with
  their offsets and widths, array dimensions and element types -- but does not
  resolve a field selection or a subscript against it (that is #125 and #126)
  and generates no code for one. So this file only DECLARES structured types
  and variables of them; it does not read or write a field or an element.
  Expect NO errors.

  Covers: a plain record, a record nested inside another record's field, a
  record with an array field, an array of a scalar, a two-dimensional array,
  an array of a record, a record with a variant part, and a type alias of a
  record and of an array.

  Check with: bin/aquarius --check test_structured_types.pas }

program Test_Structured_Types;

type
   Point = record
      x, y : integer
   end;

   Segment = record
      p1, p2 : Point
   end;

   Row = array [1 .. 10] of integer;

   Grid = array [1 .. 3, 1 .. 3] of real;

   Points = array [1 .. 5] of Point;

   Shape = record
      corner : Point;
      sizes  : Row
   end;

   Reading = record
      kind : integer;
      case tag : integer of
         0 : (whole : integer);
         1 : (approx : real; exact : boolean)
   end;

   Coordinate = Point;
   Histogram  = Row;

var
   a, b : Point;
   s    : Segment;
   r    : Row;
   g    : Grid;
   ps   : Points;
   sh   : Shape;
   rd   : Reading;
   c    : Coordinate;
   h    : Histogram;

begin
   writeln(1)
end.
