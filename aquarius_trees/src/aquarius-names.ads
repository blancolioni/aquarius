package Aquarius.Names is

   pragma Preelaborate;

   --  A package for handling Strings biased for short strings
   --  that are initialised once and never changed afterwards

   type Aquarius_Name is private;

   Null_Aquarius_Name : constant Aquarius_Name;

   function To_Aquarius_Name (Item : String) return Aquarius_Name;
   function To_String (Item : Aquarius_Name) return String;

   function "=" (Left  : Aquarius_Name;
                 Right : String)
                return Boolean;
   function "=" (Left  : String;
                 Right : Aquarius_Name)
                return Boolean;

   type Aquarius_Name_Value is new Root_Aquarius_Object with private;

   type Aquarius_Name_Value_Access is access all Aquarius_Name_Value'Class;

   overriding
   function Name (Item : Aquarius_Name_Value)
                 return String;

   function Name_Value (Name : String)
                       return Aquarius_Name_Value_Access;

   function Allocated_Name_Count
     return Natural;

private

   type Aquarius_Name is new Natural;

   subtype Real_Aquarius_Name is Aquarius_Name range 1 .. Aquarius_Name'Last;

   Null_Aquarius_Name : constant Aquarius_Name := 0;

   type Aquarius_Name_Value is new Root_Aquarius_Object with
      record
         Value : Aquarius_Name;
      end record;

end Aquarius.Names;
