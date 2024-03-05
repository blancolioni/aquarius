with WL.String_Maps;

package body Aquarius.Devices is

   package Device_Maps is
     new WL.String_Maps (Reference);

   Device_Map : Device_Maps.Map;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Reference is
   begin
      if Device_Map.Contains (Name) then
         return Device_Map.Element (Name);
      else
         return null;
      end if;
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name   : String;
      Device : Reference)
   is
   begin
      Device_Map.Insert (Name, Device);
   end Register;

end Aquarius.Devices;
