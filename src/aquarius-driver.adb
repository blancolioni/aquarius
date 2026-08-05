with Ada.Text_IO;

with Aquarius.Library;

procedure Aquarius.Driver is
begin
   if not Aquarius.Library.Run then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "aquarius: GUI disabled in this build; nothing to do.");
   end if;
end Aquarius.Driver;
