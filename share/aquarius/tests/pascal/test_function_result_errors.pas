{ The boundaries of a function's own name in scope (issue #84). Expect
  exactly THREE errors:

     cannot assign integer to boolean
        -- Is_Even's own name is boolean, not integer
     not supported yet: assignment to the result of Loop_Body
        -- a PROCEDURE has no result, so its own bare name assigned to is
           still the (unsupported) old call-target case, not a variable
     wrong number of arguments for Plus_1: expected 1, found 0
        -- Plus_1's own name, referenced bare from OUTSIDE its body, is an
           ordinary call, not its result

  Check with: bin/aquarius --check test_function_result_errors.pas }

program Test_Function_Result_Errors;

var
   flag : boolean;
   a    : integer;

function Is_Even(n : integer) : boolean;
begin
   Is_Even := n              { error }
end;

procedure Loop_Body;
begin
   Loop_Body := 5            { error }
end;

function Plus_1(x : integer) : integer;
begin
   Plus_1 := x + 1
end;

begin
   flag := Is_Even(4);
   Loop_Body;
   a := Plus_1               { error }
end.
