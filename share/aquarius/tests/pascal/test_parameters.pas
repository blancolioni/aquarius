{ Procedure and function parameters in scope, issue #82. Expect NO errors.

  A parameter is a name in the routine's own scope, so it resolves in the body
  like a local does, and it may shadow an outer name. Parameters and locals
  share that scope, which is what makes a local repeating a parameter name an
  error -- see test_parameter_errors.pas for that direction.

  Check with: bin/aquarius --check test_parameters.pas }

program Test_Parameters(input, output);

var
   g : integer;      { global, reachable from every body below }
   x : integer;      { shadowed by parameters named x }

procedure One_Value(x : integer);
begin
   writeln(x)        { the parameter, not the program's x }
end;

procedure Several(a, b : integer; c : integer);
   var t : integer;  { a local alongside the parameters }
begin
   t := a + b;
   t := t + c;
   g := t            { still reaches the global }
end;

procedure By_Ref(var total : integer; increment : integer);
begin
   total := total + increment
end;

procedure Outer(x : integer);

   procedure Inner(x : integer);   { shadows Outer's parameter }
   begin
      writeln(x)                   { Inner's x }
   end;

begin
   Inner(x)                        { Outer's x, passed on }
end;

function Add(left, right : integer) : integer;
   var sum : integer;
begin
   sum := left + right;
   writeln(sum)
end;

procedure Loops(limit : integer);
   var i : integer;
begin
   for i := 1 to limit do          { a parameter as the loop bound }
      g := g + i
end;

begin
   x := 0;
   g := 0;
   One_Value(1);
   Several(1, 2, 3);
   By_Ref(g, 1);
   Outer(2);
   Loops(3)
end.
