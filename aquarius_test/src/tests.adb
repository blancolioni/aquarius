with Aquarius.Library;
with Aquarius.Tests;

procedure Tests is
begin
   if Aquarius.Library.Initialize then
      Aquarius.Tests.Run_Tests;
      Aquarius.Library.Shut_Down;
   end if;
end Tests;
