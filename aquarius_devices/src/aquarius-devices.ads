with Aqua.Devices;

package Aquarius.Devices is

   subtype Parent is Aqua.Devices.Instance;
   type Instance is abstract new Parent with private;
   type Reference is access all Instance'Class;

   function Word_Count
     (This : Instance)
      return Natural
      is abstract;

   procedure Register
     (Name   : String;
      Device : Reference);

   function Get (Name : String) return Reference;

private

   type Instance is abstract new Parent with
      record
         null;
      end record;

end Aquarius.Devices;
