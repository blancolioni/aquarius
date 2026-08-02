{ Enumerated types (issue #139): a type declaration binds the type name AND
  each value as a constant of it, so 'colour = (red, green, blue)' makes red,
  green and blue usable directly wherever a colour is needed.

  Covers: declaring the type and variables of it, assigning and comparing
  values, an enum used as an array index type (its declared value count
  becomes the dimension's extent), a type alias, and an enum-typed for-loop
  control variable (issue #139 makes it Is_Ordinal). Expect NO errors.

  Check with: bin/aquarius --check test_enumerated_types.pas }

program Test_Enumerated_Types;

type
   Colour  = (red, green, blue);
   Shade   = Colour;
   Palette = array [Colour] of integer;

var
   c, d     : Colour;
   s        : Shade;
   counts   : Palette;
   matched  : boolean;

begin
   c := red;
   d := blue;
   s := green;

   matched := c = d;
   matched := c <> d;

   counts[red] := 1;
   counts[green] := 2;
   counts[blue] := counts[red] + counts[green];

   for c := red to blue do
      counts[c] := 0
end.
