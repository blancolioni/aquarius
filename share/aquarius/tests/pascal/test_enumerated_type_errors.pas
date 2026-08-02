{ Enumerated types, the error cases (issue #139). Expect exactly THREE errors:

     duplicate declaration: red      -- Wrong redeclares red, already Colour's
     cannot assign enumerated type to integer
     operator requires numeric operands, found enumerated type and
        enumerated type            -- '+' does not apply to enum values

  Check with: bin/aquarius --check test_enumerated_type_errors.pas }

program Test_Enumerated_Type_Errors;

type
   Colour = (red, green, blue);
   Wrong  = (yellow, red);          { error: red already declared }

var
   c, d : Colour;
   i    : integer;

begin
   c := red;
   i := c;                          { error }
   d := c + red                     { error }
end.
