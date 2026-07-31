{ Parameters, the error cases, issue #82. Expect exactly THREE errors:

     duplicate declaration: n      -- a local repeats a parameter name
     duplicate declaration: dup    -- one parameter list repeats a name
     undeclared variable: hidden   -- another routine's parameter

  Parameters and locals share the routine's scope, so repeating a name in either
  is a redeclaration -- while shadowing a name from an ENCLOSING scope is legal,
  as test_parameters.pas shows.

  Check with: bin/aquarius --check test_parameter_errors.pas }

program Test_Parameter_Errors;

var
   g : integer;

procedure Repeats_A_Parameter(n : integer);
   var n : integer;        { error: same scope as the parameter }
begin
   g := n
end;

procedure Repeats_In_The_List(dup : integer; dup : integer);
begin
   g := dup
end;

procedure Owns_It(hidden : integer);
begin
   g := hidden             { fine: its own parameter }
end;

procedure Cannot_See_It;
begin
   g := hidden             { error: belongs to Owns_It }
end;

begin
   g := 0;
   Owns_It(1);
   Cannot_See_It
end.
