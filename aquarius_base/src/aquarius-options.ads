package Aquarius.Options is

   function Load return Boolean;

   function Start_Class return String;

   function Check_File return String;

   function Test_File return String;
   --  path of a file to load and check for errors; empty if not given

   function Source_File_Count return Natural;
   function Source_File (Index : Positive) return String;

   function Code_Trigger return Boolean;
   function Aqua_Trace return Boolean;

   function Self_Test return Boolean;

   function Clear_Cache return Boolean;
   --  when set, empty the temporary (cache) folder before continuing

   function Arch return String;
   --  architecture that generated code targets: "pdp11" (the default),
   --  "aqua" or "6502", lower-cased.  Load rejects anything else.  This is
   --  the target a language plugin generates for; it does not affect the Aqua
   --  compiler, which always generates for the Aqua VM.

   function Show_Full_Path return Boolean;
   --  when reporting file names, use the full path

   function Report_Files return Boolean;
   --  when files are read or written, report action
   --  on standard output

   function Tagatha_Trace_P_Code return Boolean;
   function Tagatha_Trace_Transfers return Boolean;
   function Tagatha_Trace_Improvements return Boolean;

end Aquarius.Options;
