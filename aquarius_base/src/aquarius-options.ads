package Aquarius.Options is

   function Load return Boolean;

   function Trace_Server return Boolean;
   function Start_Class return String;

   function Source_File_Count return Natural;
   function Source_File (Index : Positive) return String;

   function Code_Trigger return Boolean;
   function Aqua_Trace return Boolean;

end Aquarius.Options;
