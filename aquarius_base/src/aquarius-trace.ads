package Aquarius.Trace is

   pragma Elaborate_Body;

   type Debug_Class is (Actions, Buffers, Configuration, Cursors, Entries,
                        Errors, Files, Formatting, Grammar, Parsing, Plugins,
                        Program_Trees, Properties, Scanning, Syntax_Trees,
                        Tokens, Trees, Types, Watchers);

   pragma Ordered (Debug_Class);

   function Debug_Name (Item : Debug_Class) return String;

   procedure Enable (Class : Debug_Class);
   procedure Enable_All;

   --  Enable a particular debug setting based on its name
   --  (as returned by the Debug_Name function).  If the
   --  class does not exist, Constraint_Error is raised.
   --  If Name is a comma-separated list, it is interpreted
   --  as a list of debug classes
   procedure Enable (Names : String);

   procedure Trace_Put_Line (Class   : Debug_Class;
                             Message : String);
   procedure Set_Col (Class  : Debug_Class;
                      Column : Positive);

   procedure Start_Trace;
   procedure End_Trace;

   --  Save current settings, then clear them
   --  Note: "stack" isn't really a stack
   procedure Push_Settings;

   --  Restore saved settings.
   procedure Pop_Settings;

end Aquarius.Trace;
