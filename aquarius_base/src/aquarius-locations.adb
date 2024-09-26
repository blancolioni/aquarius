package body Aquarius.Locations is

   -------------------
   -- Update_Column --
   -------------------

   procedure Update_Column
     (This       : in out Updateable_Location_Interface'Class;
      New_Column : Column_Index)
   is
   begin
      This.Update_Location
        (Offset =>
           This.Offset + Location_Offset (New_Column - This.Column + 1),
         Line   => This.Line,
         Column => New_Column);
   end Update_Column;

   ---------------------
   -- Update_Location --
   ---------------------

   overriding procedure Update_Location
     (This : in out Instance;
      From : Location_Interface'Class)
   is
   begin
      This.Offset := From.Offset;
      This.Line := From.Line;
      This.Column := From.Column;
   end Update_Location;

   ---------------------
   -- Update_Location --
   ---------------------

   procedure Update_Location
     (This   : in out Updateable_Location_Interface'Class;
      Offset : Location_Offset;
      Line   : Line_Index;
      Column : Column_Index)
   is
   begin
      This.Update_Location (To_Location (Offset, Line, Column));
   end Update_Location;

end Aquarius.Locations;
