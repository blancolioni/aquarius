with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with Parse_Args;

package body Aquarius.Options is

   use Parse_Args;

   AP : Argument_Parser;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Source_File_Vector : String_Vectors.Vector;

   ----------------
   -- Aqua_Trace --
   ----------------

   function Aqua_Trace return Boolean is
   begin
      return AP.Boolean_Value ("aqua trace");
   end Aqua_Trace;

   ------------------
   -- Code_Trigger --
   ------------------

   function Code_Trigger return Boolean is
   begin
      return AP.Boolean_Value ("code trigger");
   end Code_Trigger;

   ----------
   -- Load --
   ----------

   function Load return Boolean is
   begin

      AP.Add_Option
        (O             => Make_Boolean_Option (False),
         Name          => "aqua trace",
         Long_Option   => "aqua-trace",
         Usage         => "Enable trace for Aqua execution");

      AP.Add_Option
        (O             => Make_Boolean_Option (False),
         Name          => "code trigger",
         Long_Option   => "code-trigger",
         Usage         => "Run the code trigger on source files");

      AP.Add_Option
        (O             => Make_String_Option (""),
         Name          => "start class",
         Long_Option   => "start-class",
         Usage         => "Create and run the Aqua class found in this path");

      AP.Add_Option
        (O             => Make_Boolean_Option (False),
         Name          => "trace server",
         Short_Option  => 't',
         Long_Option   => "trace-server",
         Usage         => "Write a trace file when running Aqua programs");

      AP.Allow_Tail_Arguments ("files ...");

      AP.Parse_Command_Line;

      if not AP.Parse_Success then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            AP.Parse_Message);
         return False;
      end if;

      for File_Name of AP.Tail loop
         Source_File_Vector.Append (File_Name);
      end loop;

      return True;

   end Load;

   -----------------
   -- Start_Class --
   -----------------

   function Start_Class return String is
   begin
      return AP.String_Value ("start class");
   end Start_Class;

   -----------------
   -- Source_File --
   -----------------

   function Source_File (Index : Positive) return String is
   begin
      return Source_File_Vector (Index);
   end Source_File;

   -----------------------
   -- Source_File_Count --
   -----------------------

   function Source_File_Count return Natural is
   begin
      return Source_File_Vector.Last_Index;
   end Source_File_Count;

   ------------------
   -- Trace_Server --
   ------------------

   function Trace_Server return Boolean is
   begin
      return AP.Boolean_Value ("trace server");
   end Trace_Server;

end Aquarius.Options;
