{ A function's own name is in scope inside its own body (issue #84): unlike
  anywhere else, it does not mean a call there -- assigning to it sets the
  result, and it may be read back before the function returns.

  Covers: a simple result assignment, reading the result back (Clamp), a
  recursive call written the ordinary way -- with parentheses -- alongside a
  bare assignment to the same name in the same function (Factorial), and a
  boolean result. Expect NO errors.

  Check with: bin/aquarius --check test_function_result.pas }

program Test_Function_Result;

var
   a, b : integer;
   flag : boolean;

function Plus_1(x : integer) : integer;
begin
   Plus_1 := x + 1
end;

function Clamp(x : integer) : integer;
begin
   Clamp := x;
   if Clamp > 100 then Clamp := 100;
   if Clamp < 0 then Clamp := 0
end;

function Factorial(n : integer) : integer;
begin
   if n <= 1 then
      Factorial := 1
   else
      Factorial := n * Factorial(n - 1)   { a recursive call, written with parens }
end;

function Is_Positive(n : integer) : boolean;
begin
   Is_Positive := n > 0
end;

begin
   a := Plus_1(1);
   b := Clamp(150);
   a := Factorial(5);
   flag := Is_Positive(a)
end.
