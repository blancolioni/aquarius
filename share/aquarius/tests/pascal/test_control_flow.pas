program Test_Control_Flow;
   var
      I : Integer;
      Sum : Integer;
begin
   Sum := 0;
   for I := 1 to 10 do
      Sum := Sum + I;
   if Sum > 50 then
      Sum := Sum - 1
   else
      Sum := 0;
   while Sum > 0 do
      Sum := Sum - 1
end.
