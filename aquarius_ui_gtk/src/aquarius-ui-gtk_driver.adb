with Aquarius.Library;
with Aquarius.UI.Gtk_View;

procedure Aquarius.UI.Gtk_Driver is
begin
   if Aquarius.Library.Initialize then
      Aquarius.UI.Gtk_View.Launch
        (Icon_Dir => Aquarius.Library.Configuration_Path & "/icons");
      Aquarius.Library.Shut_Down;
   end if;
end Aquarius.UI.Gtk_Driver;
