with Ada.Calendar;

with Ack.Classes;

package Ack.Compile is

   type Compilation_Result is tagged private;

   function Newest_Class_Source
     (Result : Compilation_Result'Class)
     return Ada.Calendar.Time;

   function Compiled_Classes_Count
     (Result : Compilation_Result'Class)
      return Natural;

   procedure Compile_Class
     (Source_Path : String;
      Result      : in out Compilation_Result'Class;
      Feature_Callback : access
        procedure (Class        : not null access constant
                     Ack.Classes.Class_Entity_Record'Class;
                   Feature      : not null access constant
                     Root_Entity_Type'Class));

   procedure Load_Root_Class
     (Source_Path : String);

   procedure Check_Assembly_Package
     (Package_Name : String;
      Assembled    : out Boolean);
   --  Check if Package_Name needs to be assembled.
   --  Package_Name - the name of the package, without extension
   --  Assembled    - set to true if assembly was required, otherwise false

private

   type Compilation_Result is tagged
      record
         Compilation_Count   : Natural := 0;
         Newest_Class_Source : Ada.Calendar.Time;
      end record;

   function Compiled_Classes_Count
     (Result : Compilation_Result'Class)
      return Natural
   is (Result.Compilation_Count);

   function Newest_Class_Source
     (Result : Compilation_Result'Class)
      return Ada.Calendar.Time
   is (Result.Newest_Class_Source);

end Ack.Compile;
