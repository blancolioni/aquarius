{ The type system, the error cases (issue #88). Expect exactly TWELVE errors:

     cannot assign real to integer                  -- narrowing needs round/trunc
     cannot assign integer to boolean
     cannot assign integer to char
     operator requires integer operands, ...        -- 'div' on a real
     operator requires integer operands, ...        -- 'mod' on a real
     operator requires boolean operands, ...        -- 'and' on an integer
     'not' needs a boolean operand, found integer
     cannot compare integer with boolean
     a sign needs a numeric operand, found boolean
     a loop variable must be ordinal, but r is real
     argument 1 of Takes_Integer is integer, but a real was given
     unsupported type for an assigned value         -- a record is not modelled

  One message per mistake, not one per enclosing operator: an operand whose
  problem has been reported becomes compatible with everything, so the
  assignment around it says nothing further.

  The last one is the strict reading of an unmodelled type. Records, arrays,
  pointers and sets are reported where they are USED rather than silently
  accepted; typing them is issue #104.

  Check with: bin/aquarius --check test_type_errors.pas }

program Test_Type_Errors;

type
   Point = record
      x, y : integer
   end;

var
   i    : integer;
   r    : real;
   flag : boolean;
   c    : char;
   p    : Point;

procedure Takes_Integer(n : integer);
begin
   writeln(n)
end;

begin
   i := r;                  { error }
   flag := i;               { error }
   c := i;                  { error }

   i := i div r;            { error }
   i := i mod r;            { error }
   flag := flag and i;      { error }
   flag := not i;           { error }
   flag := i = flag;        { error }
   i := -flag;              { error }

   for r := 1 to 10 do      { error }
      i := i + 1;

   Takes_Integer(r);        { error }

   i := p                   { error }
end.
