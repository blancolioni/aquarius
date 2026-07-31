{ Constant identifiers, issue #103. Expect NO errors.

  A constant binds to a value rather than to storage, so a use of one generates
  the literal and takes no frame slot. Constants live in the scope they are
  declared in, like any other name, so one declared in a procedure is invisible
  outside it and may shadow an outer one.

  Check with: bin/aquarius --check test_constants.pas }

program Test_Constants;

const
   Max = 10;
   Min = -4;              { a signed constant }
   Plus_Signed = +7;
   Limit = Max;           { defined from an earlier constant }
   Neg_Limit = -Max;      { and negated }
   Star = '*';            { a character constant is its code point }
   Letter = 'K';

var
   i, total : integer;
   c : char;

procedure Uses_Outer_Constants;
begin
   total := Max + Limit
end;

procedure Has_Its_Own;
   const
      Max = 99;           { shadows the program's Max }
   var
      local_total : integer;
begin
   local_total := Max;    { 99, not 10 }
   total := total + local_total
end;

begin
   total := Max;
   total := total + Min + Plus_Signed + Limit + Neg_Limit;
   c := Star;
   c := Letter;
   for i := 1 to Max do   { a constant as a loop bound }
      total := total + i;
   Uses_Outer_Constants;
   Has_Its_Own
end.
