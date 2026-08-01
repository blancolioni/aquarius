{ Calls, the error cases, issue #105. Expect exactly SEVEN errors:

     wrong number of arguments for One_Argument: expected 1, found 0
     wrong number of arguments for One_Argument: expected 1, found 2
     undeclared procedure: Nosuch
     wrong number of arguments for Add: expected 2, found 1
     a procedure has no value, so it cannot be used here: No_Arguments
     argument 1 of By_Ref is a var parameter, so it needs a variable
     argument 1 of By_Ref is a var parameter, so it needs a variable

  Every one of these was accepted silently before: a call was never compared
  with the routine it called.

  Check with: bin/aquarius --check test_call_errors.pas }

program Test_Call_Errors;

var
   g : integer;

procedure No_Arguments;
begin
   g := 0
end;

procedure One_Argument(x : integer);
begin
   g := x
end;

procedure By_Ref(var total : integer);
begin
   total := 0
end;

function Add(a, b : integer) : integer;
begin
   writeln(a + b)
end;

begin
   One_Argument;           { too few }
   One_Argument(1, 2);     { too many }
   Nosuch(1);              { no such routine }
   g := Add(1);            { too few, in an expression }
   g := No_Arguments;      { a procedure has no value }
   By_Ref(42);             { a literal has no address }
   By_Ref(g + 1)           { nor has an expression }
end.
