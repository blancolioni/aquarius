{ The standard functions, the error cases (issue #83). Expect exactly EIGHT
  errors:

     argument of abs must be numeric, but found boolean
     argument of ord must be ordinal (integer, boolean, char or an enumerated
       type), but found real
     argument of chr must be integer, but found real
     argument of odd must be integer, but found real
     cannot assign real to integer          -- sqrt always yields a real,
       even from a real argument; narrowing needs round/trunc
     wrong number of arguments for sqrt: expected 1, found 0
     wrong number of arguments for sqrt: expected 1, found 2
     not supported yet: assignment to the result of abs

  One message per mistake: an operand whose problem has been reported
  becomes compatible with everything, so the assignment around it says
  nothing further.

  Check with: bin/aquarius --check test_standard_function_errors.pas }

program Test_Standard_Function_Errors;

var
   i, j : integer;
   r    : real;
   c    : char;
   flag : boolean;

begin
   i := abs(flag);         { error }
   i := ord(r);             { error }
   c := chr(r);              { error }
   flag := odd(r);           { error }
   i := sqrt(r);             { error }
   j := sqrt;                { error }
   j := sqrt(r, r);          { error }
   abs(i) := 5               { error }
end.
