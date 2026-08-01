{ Calls checked against their parameter lists, issue #105. Expect NO errors.

  A call is resolved against the callee's signature, which is recorded on the
  routine's binding in the ENCLOSING scope -- the routine's own scope, which
  knows its parameters, is gone by the time a call is written.

  Check with: bin/aquarius --check test_calls.pas }

program Test_Calls;

var
   g, h : integer;

procedure No_Arguments;
begin
   g := 0
end;

procedure One_Argument(x : integer);
begin
   g := x
end;

procedure Several(a, b : integer; c : integer);
begin
   g := a + b + c
end;

procedure By_Ref(var total : integer; increment : integer);
begin
   total := total + increment
end;

function Add(a, b : integer) : integer;
begin
   writeln(a + b)
end;

procedure Calls_Its_Sibling(n : integer);
begin
   One_Argument(n);        { a call from inside another routine }
   By_Ref(g, n)            { a global is a variable, so it may be passed by ref }
end;

begin
   No_Arguments;
   One_Argument(1);
   Several(1, 2, 3);
   By_Ref(g, 1);
   By_Ref(h, g + 1);       { only the first parameter is var }
   Calls_Its_Sibling(2);
   g := Add(1, 2);
   writeln(g, h, g + h);   { write and writeln take any number of arguments }
   writeln(g:5)            { a field width is part of one argument, not a second }
end.
