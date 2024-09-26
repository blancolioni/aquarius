package Aquarius.Options is

   function Load return Boolean;

   function Start_Class return String;

   function Source_File_Count return Natural;
   function Source_File (Index : Positive) return String;

   function Code_Trigger return Boolean;
   function Aqua_Trace return Boolean;

   function Self_Test return Boolean;

   function Show_Full_Path return Boolean;
   --  when reporting file names, use the full path

   function Report_Files return Boolean;
   --  when files are read or written, report action
   --  on standard output

   function Tagatha_Trace_P_Code return Boolean;
   function Tagatha_Trace_Transfers return Boolean;
   function Tagatha_Trace_Improvements return Boolean;

end Aquarius.Options;
