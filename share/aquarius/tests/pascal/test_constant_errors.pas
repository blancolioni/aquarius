{ Constants, the error cases, issue #103. Expect exactly THREE errors:

     duplicate declaration: Max          -- redeclared in the same scope
     cannot assign to a constant: Max    -- a constant is not storage
     cannot use a constant as a loop variable: Max

  A constant shares its scope with the block's variables, so redeclaring the name
  is an error just as it is for two variables -- while a constant declared in an
  inner block may shadow an outer one, as test_constants.pas shows.

  Check with: bin/aquarius --check test_constant_errors.pas }

program Test_Constant_Errors;

const
   Max = 10;
   Max = 20;              { error: same scope }

var
   total : integer;

begin
   total := Max;          { fine }
   Max := 1;              { error: not storage }
   for Max := 1 to 5 do   { error: the loop assigns to it }
      total := total + 1
end.
