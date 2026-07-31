{ Lexical scopes, issue #81. Expect NO errors.

  Each block gets its own scope, so an inner declaration shadows an outer one of
  the same name instead of colliding with it, and sibling procedures may reuse a
  name freely. Every variable still gets a distinct frame slot: one routine is
  generated for the whole program, so the slots must not overlap even where the
  names do.

  Check with: bin/aquarius --check test_scopes.pas }

program Test_Scopes;

var
   x : integer;      { program x }
   shared : integer;

procedure Outer;
   var x : integer;  { shadows the program x }

   procedure Inner;
      var x : integer;   { shadows Outer's x }
   begin
      x := 3;           { Inner's x }
      shared := 3       { reaches outward to the program's shared }
   end;

begin
   x := 4                { Outer's x }
end;

procedure Sibling;
   var x : integer;      { same name as Outer's local, a different variable }
begin
   x := 5
end;

begin
   x := 1;               { the program's x }
   shared := 0
end.
