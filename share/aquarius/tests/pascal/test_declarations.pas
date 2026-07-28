program Declarations;

const
   Max = 10;

type
   Counter = integer;

var
   i : integer;
   total : integer;

procedure Report(unused : integer);
begin
   writeln(total)
end;

function Compute(seed : integer) : integer;
begin
   writeln(total)
end;

begin
   total := 0;
   for i := 1 to 10 do
      total := total + i;
   writeln(total)
end.
