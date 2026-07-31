{ Lexical scopes, issue #81, the other direction. Expect exactly TWO errors:
  "undeclared variable: local_to_p" and "duplicate declaration: dup".

  A procedure's local is not visible outside it, and a name declared twice in
  ONE block is still an error -- shadowing is legal, redeclaring is not.

  Check with: bin/aquarius --check test_scope_leak.pas }

program Test_Scope_Leak;

var
   dup : integer;
   dup : integer;     { error: same block, not shadowing }

procedure P;
   var local_to_p : integer;
begin
   local_to_p := 1
end;

begin
   local_to_p := 2    { error: P's local is out of scope here }
end.
