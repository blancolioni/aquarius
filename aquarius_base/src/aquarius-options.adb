with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Aquarius.Options is

   use Ada.Strings.Unbounded;

   type Option_Kind is (Bool_Kind, Str_Kind);

   type Option_Info is record
      Name        : Unbounded_String;
      Long_Option : Unbounded_String;
      Usage       : Unbounded_String;
      Kind        : Option_Kind;
   end record;

   package Option_Vectors is
     new Ada.Containers.Vectors (Positive, Option_Info);

   package Boolean_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Boolean);

   package String_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Defined_Options    : Option_Vectors.Vector;
   Bool_Values        : Boolean_Maps.Map;
   Str_Values         : String_Maps.Map;
   Source_File_Vector : String_Vectors.Vector;

   Aqua_Trace_Option   : constant String := "aqua trace";
   Arch_Option         : constant String := "arch";
   Check_File_Option   : constant String := "check file";
   Clear_Cache_Option  : constant String := "clear cache";
   Code_Trigger_Option : constant String := "code trigger";
   Help_Option         : constant String := "help";
   Line_Length_Option  : constant String := "line length";
   Output_Path_Option  : constant String := "output path";
   Pretty_Print_Option : constant String := "pretty print";
   Self_Test_Option    : constant String := "self test";
   Start_Class_Option  : constant String := "start class";
   Test_File_Option    : constant String := "test file";
   Verbose_Option      : constant String := "verbose";

   Default_Arch : constant String := "pdp11";

   function Is_Known_Arch (Name : String) return Boolean
   is (Name in "aqua" | "6502" | "pdp11");
   --  Kept in step with the registry in Tagatha.Arch.Loader, which this crate
   --  cannot see.  Worth the duplication: it turns a typo into a message at
   --  startup rather than a Constraint_Error part-way through code generation.

   Show_Full_Path_Option : constant String := "show full path";
   Report_Files_Option   : constant String := "report files";

   Tagatha_Trace_P_Code_Option       : constant String := "tagatha p-code";
   Tagatha_Trace_Transfers_Option    : constant String := "tagatha transfers";
   Tagatha_Trace_Improvements_Option : constant String :=
                                         "tagatha improvements";

   procedure Add_Option
     (Name        : String;
      Long_Option : String;
      Usage       : String;
      Kind        : Option_Kind);

   function Find_Long (Long : String) return Natural;

   procedure Show_Usage;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option
     (Name        : String;
      Long_Option : String;
      Usage       : String;
      Kind        : Option_Kind)
   is
   begin
      Defined_Options.Append
        (Option_Info'
           (Name        => To_Unbounded_String (Name),
            Long_Option => To_Unbounded_String (Long_Option),
            Usage       => To_Unbounded_String (Usage),
            Kind        => Kind));
      case Kind is
         when Bool_Kind =>
            Bool_Values.Insert (Name, False);
         when Str_Kind =>
            Str_Values.Insert (Name, "");
      end case;
   end Add_Option;

   ----------
   -- Arch --
   ----------

   function Arch return String is
      Value : constant String := Str_Values (Arch_Option);
   begin
      if Value = "" then
         return Default_Arch;
      else
         return Ada.Characters.Handling.To_Lower (Value);
      end if;
   end Arch;

   ----------------
   -- Aqua_Trace --
   ----------------

   function Aqua_Trace return Boolean is
   begin
      return Bool_Values (Aqua_Trace_Option);
   end Aqua_Trace;

   ----------------
   -- Check_File --
   ----------------

   function Check_File return String is
   begin
      return Str_Values (Check_File_Option);
   end Check_File;

   -----------------
   -- Clear_Cache --
   -----------------

   function Clear_Cache return Boolean is
   begin
      return Bool_Values (Clear_Cache_Option);
   end Clear_Cache;

   ---------------
   -- Test_File --
   ---------------

   function Test_File return String is
   begin
      return Str_Values (Test_File_Option);
   end Test_File;

   ------------------
   -- Code_Trigger --
   ------------------

   function Code_Trigger return Boolean is
   begin
      return Bool_Values (Code_Trigger_Option);
   end Code_Trigger;

   ---------------
   -- Find_Long --
   ---------------

   function Find_Long (Long : String) return Natural is
   begin
      for I in 1 .. Defined_Options.Last_Index loop
         if To_String (Defined_Options (I).Long_Option) = Long then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Long;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length return Natural is
      Value : constant String := Str_Values (Line_Length_Option);
   begin
      if Value = "" then
         return 72;
      else
         return Natural'Value (Value);
      end if;
   exception
      when others =>
         return 0;
   end Line_Length;

   ----------
   -- Load --
   ----------

   function Load return Boolean is
      use Ada.Command_Line;
      use Ada.Strings.Fixed;

      Arg_Index       : Natural := 1;
      No_More_Options : Boolean := False;

      function Fail (Message : String) return Boolean;

      ----------
      -- Fail --
      ----------

      function Fail (Message : String) return Boolean is
      begin
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
         return False;
      end Fail;

   begin
      Add_Option (Aqua_Trace_Option, "aqua-trace",
                  "Enable trace for Aqua execution", Bool_Kind);
      Add_Option (Code_Trigger_Option, "code-trigger",
                  "Run the code trigger on source files", Bool_Kind);
      Add_Option (Start_Class_Option, "start-class",
                  "Create and run the Aqua class found in this path",
                  Str_Kind);
      Add_Option (Check_File_Option, "check",
                  "Load a file, report any errors, and exit", Str_Kind);
      Add_Option (Test_File_Option, "test",
                  "Load a file, run the grammar's test actions, and exit",
                  Str_Kind);
      Add_Option (Pretty_Print_Option, "pretty-print",
                  "Apply format rules to the input files", Bool_Kind);
      Add_Option (Output_Path_Option, "output-path",
                  "Write output files to this directory", Str_Kind);
      Add_Option (Line_Length_Option, "line-length",
                  "Maximum line length when pretty printing", Str_Kind);
      Add_Option (Self_Test_Option, "self-test",
                  "Run unit tests", Bool_Kind);
      Add_Option (Clear_Cache_Option, "clear-cache",
                  "Empty the temporary folder before continuing", Bool_Kind);
      Add_Option (Arch_Option, "arch",
                  "Architecture that generated code targets: "
                  & "pdp11 (default), aqua or 6502",
                  Str_Kind);
      Add_Option (Help_Option, "help",
                  "Show help", Bool_Kind);
      Add_Option (Show_Full_Path_Option, "show-full-path",
                  "use full path when reporting file names", Bool_Kind);
      Add_Option (Report_Files_Option, "report-files",
                  "report all filesystem activity", Bool_Kind);
      Add_Option (Tagatha_Trace_Improvements_Option,
                  "tagatha-trace-improvements",
                  "Log code improvements applied by Tagatha", Bool_Kind);
      Add_Option (Tagatha_Trace_P_Code_Option, "tagatha-trace-p-code",
                  "Log code p-code generated by Tagatha", Bool_Kind);
      Add_Option (Tagatha_Trace_Transfers_Option, "tagatha-trace-transfers",
                  "Log code transfers generated by Tagatha", Bool_Kind);
      Add_Option (Verbose_Option, "verbose",
                  "Extra informational output", Bool_Kind);

      while Arg_Index <= Argument_Count loop
         declare
            Arg : constant String := Argument (Arg_Index);
         begin
            if not No_More_Options and then Arg = "--" then
               No_More_Options := True;
            elsif not No_More_Options
              and then Arg'Length >= 2
              and then Arg (Arg'First .. Arg'First + 1) = "--"
            then
               declare
                  Text : constant String := Arg (Arg'First + 2 .. Arg'Last);
                  Eq   : constant Natural := Index (Text, "=");
                  Long : constant String :=
                           (if Eq = 0 then Text
                            else Text (Text'First .. Eq - 1));
                  Opt  : constant Natural := Find_Long (Long);
               begin
                  if Opt = 0 then
                     return Fail ("unknown option: --" & Long);
                  end if;

                  declare
                     Info : constant Option_Info := Defined_Options (Opt);
                     Name : constant String := To_String (Info.Name);
                  begin
                     case Info.Kind is
                        when Bool_Kind =>
                           if Eq /= 0 then
                              return Fail
                                ("option --" & Long
                                 & " does not take a value");
                           end if;
                           Bool_Values (Name) := True;
                        when Str_Kind =>
                           if Eq /= 0 then
                              Str_Values.Replace
                                (Name, Text (Eq + 1 .. Text'Last));
                           elsif Arg_Index = Argument_Count then
                              return Fail
                                ("option --" & Long
                                 & " requires a value");
                           else
                              Arg_Index := Arg_Index + 1;
                              Str_Values.Replace
                                (Name, Argument (Arg_Index));
                           end if;
                     end case;
                  end;
               end;
            else
               Source_File_Vector.Append (Arg);
            end if;
         end;
         Arg_Index := Arg_Index + 1;
      end loop;

      if Bool_Values (Help_Option) then
         Show_Usage;
         return False;
      end if;

      if not Is_Known_Arch (Arch) then
         return Fail
           ("unknown architecture: " & Str_Values (Arch_Option)
            & " (expected aqua, 6502 or pdp11)");
      end if;

      return True;
   end Load;

   -----------------
   -- Output_Path --
   -----------------

   function Output_Path return String is
   begin
      return Str_Values (Output_Path_Option);
   end Output_Path;

   ------------------
   -- Pretty_Print --
   ------------------

   function Pretty_Print return Boolean is
   begin
      return Bool_Values (Pretty_Print_Option);
   end Pretty_Print;

   ------------------
   -- Report_Files --
   ------------------

   function Report_Files return Boolean is
   begin
      return Bool_Values (Report_Files_Option);
   end Report_Files;

   ---------------
   -- Self_Test --
   ---------------

   function Self_Test return Boolean is
   begin
      return Bool_Values (Self_Test_Option);
   end Self_Test;

   --------------------
   -- Show_Full_Path --
   --------------------

   function Show_Full_Path return Boolean is
   begin
      return Bool_Values (Show_Full_Path_Option);
   end Show_Full_Path;

   ----------------
   -- Show_Usage --
   ----------------

   procedure Show_Usage is
      use Ada.Text_IO;
   begin
      Put_Line ("Usage: aquarius [options] [files ...]");
      for Info of Defined_Options loop
         Put_Line
           ("  --" & To_String (Info.Long_Option)
            & "  " & To_String (Info.Usage));
      end loop;
   end Show_Usage;

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

   -----------------
   -- Start_Class --
   -----------------

   function Start_Class return String is
   begin
      return Str_Values (Start_Class_Option);
   end Start_Class;

   --------------------------------
   -- Tagatha_Trace_Improvements --
   --------------------------------

   function Tagatha_Trace_Improvements return Boolean is
   begin
      return Bool_Values (Tagatha_Trace_Improvements_Option);
   end Tagatha_Trace_Improvements;

   --------------------------
   -- Tagatha_Trace_P_Code --
   --------------------------

   function Tagatha_Trace_P_Code return Boolean is
   begin
      return Bool_Values (Tagatha_Trace_P_Code_Option);
   end Tagatha_Trace_P_Code;

   -----------------------------
   -- Tagatha_Trace_Transfers --
   -----------------------------

   function Tagatha_Trace_Transfers return Boolean is
   begin
      return Bool_Values (Tagatha_Trace_Transfers_Option);
   end Tagatha_Trace_Transfers;

   -------------
   -- Verbose --
   -------------

   function Verbose return Boolean is
   begin
      return Bool_Values (Verbose_Option);
   end Verbose;

end Aquarius.Options;
