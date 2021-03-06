with Ada.Command_Line;
with Ada.Strings.Fixed;

package body Aquarius.Command_Line is

   function Get_Flag (Short_Name : String;
                      Long_Name  : String)
                      return Boolean
     with Pre => Short_Name /= "" or else Long_Name /= "";

   function Get_Argument (Short_Name : String;
                          Long_Name  : String;
                          Default    : String := "")
                         return String
     with Pre => Short_Name /= "" or else Long_Name /= "";

   ----------------------
   -- Ack_Execute_Root --
   ----------------------

   function Ack_Execute_Root return String is
   begin
      return Get_Argument ("", "ack-execute-root");
   end Ack_Execute_Root;

   ---------------------
   -- Ack_Stack_Check --
   ---------------------

   function Ack_Stack_Check return Boolean is
   begin
      return Get_Flag ("", "ack-stack-check");
   end Ack_Stack_Check;

   ------------------------------
   -- Ack_Trace_Class_Analysis --
   ------------------------------

   function Ack_Trace_Class_Analysis return Boolean is
   begin
      return Get_Flag ("", "ack-trace-class-analysis");
   end Ack_Trace_Class_Analysis;

   -----------------------
   -- Ack_Write_Listing --
   -----------------------

   function Ack_Write_Listing return Boolean is
   begin
      return Get_Flag ("", "ack-write-listing");
   end Ack_Write_Listing;

   ----------------------
   -- Ack_Write_Tables --
   ----------------------

   function Ack_Write_Tables return Boolean is
   begin
      return Get_Flag ("", "ack-write-tables");
   end Ack_Write_Tables;

   ------------
   -- Action --
   ------------

   function Action      return String is
   begin
      return Get_Argument ("a", "action");
   end Action;

   ---------------------
   -- Aqua_Trace_Code --
   ---------------------

   function Aqua_Trace_Code return Boolean is
   begin
      return Get_Flag ("", "aqua-trace-code");
   end Aqua_Trace_Code;

   ---------------------
   -- Aqua_Trace_Link --
   ---------------------

   function Aqua_Trace_Link return Boolean is
   begin
      return Get_Flag ("", "aqua-trace-link");
   end Aqua_Trace_Link;

   --------------------------
   -- Assertion_Monitoring --
   --------------------------

   function Assertion_Monitoring return Ack.Assertion_Monitoring_Level is
      Value : constant String :=
                Get_Argument ("", "assertion-monitoring", "all");
   begin
      if Value = "none" then
         return Ack.Monitor_None;
      else
         return Ack.Monitor_All;
      end if;
   end Assertion_Monitoring;

   -----------------
   -- Clear_Cache --
   -----------------

   function Clear_Cache return Boolean is
   begin
      return Get_Flag ("", "clear-cache");
   end Clear_Cache;

   -----------------
   -- Edit_Plugin --
   -----------------

   function Edit_Plugin return String is
   begin
      return Get_Argument ("", "edit-plugin", "");
   end Edit_Plugin;

   ------------------
   -- Enable_Debug --
   ------------------

   function Enable_Debug return String is
   begin
      return Get_Argument ("d", "debug");
   end Enable_Debug;

   --------------------
   -- Enable_Plugins --
   --------------------

   function Enable_Plugins return Boolean is
   begin
      return not Get_Flag ("", "no-plugins");
   end Enable_Plugins;

   ---------------------
   -- Extra_Arguments --
   ---------------------

   function Extra_Arguments return String is
      use Ada.Command_Line;
      Skip : Boolean := False;
   begin
      for I in 1 .. Argument_Count loop
         if not Skip then
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg (Arg'First .. Arg'First + 1) = "--" then
                  null;
               elsif Arg (Arg'First) = '-' then
                  Skip := True;
               else
                  return Arg;
               end if;
            end;
         else
            Skip := False;
         end if;
      end loop;
      return "";
   end Extra_Arguments;

   ------------
   -- Filter --
   ------------

   function Filter  return Boolean is
   begin
      return Get_Flag ("f", "filter");
   end Filter;

   ------------------
   -- Get_Argument --
   ------------------

   function Get_Argument (Short_Name : String;
                          Long_Name  : String;
                          Default    : String := "")
                         return String
   is
      Short_Argument : constant String := '-' & Short_Name;
      Long_Argument  : constant String := "--" & Long_Name & "=";
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String := Ada.Command_Line.Argument (I);
         begin
            exit when Argument = "--";
            if Short_Name /= ""
              and then I < Ada.Command_Line.Argument_Count
              and then Argument = Short_Argument
            then
               return Ada.Command_Line.Argument (I + 1);
            elsif Ada.Strings.Fixed.Head (Argument, Long_Argument'Length)
              = Long_Argument
            then
               return Ada.Strings.Fixed.Tail (Argument,
                                              Argument'Length -
                                                Long_Argument'Length);
            end if;
         end;
      end loop;
      return Default;
   end Get_Argument;

   --------------
   -- Get_Flag --
   --------------

   function Get_Flag (Short_Name : String;
                      Long_Name  : String)
                     return Boolean
   is
      Short_Flag : constant String := '-' & Short_Name;
      Long_Flag  : constant String := "--" & Long_Name;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String := Ada.Command_Line.Argument (I);
         begin
            exit when Argument = "--";
            if (Short_Name /= ""
                and then  Argument = Short_Flag)
              or else (Long_Name /= ""
                       and then Argument = Long_Flag)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Get_Flag;

   ----------------------
   -- GPR_Project_Name --
   ----------------------

   function GPR_Project_Name  return String is
   begin
      return Get_Argument ("P", "gpr-project-name");
   end GPR_Project_Name;

   -------------
   -- Grammar --
   -------------

   function Grammar  return String is
   begin
      return Get_Argument ("g", "grammar");
   end Grammar;

   ----------
   -- Help --
   ----------

   function Help    return Boolean is
   begin
      return Get_Flag ("?", "help");
   end Help;

   function Initialize_Project return Boolean is
   begin
      return Get_Flag ("", "initialize-project");
   end Initialize_Project;

   ----------------
   -- Input_File --
   ----------------

   function Input_File  return String is
   begin
      return Get_Argument ("i", "input");
   end Input_File;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length  return Positive is
      Line_Length_Text : constant String := Get_Argument ("l", "line-length");
   begin
      if Line_Length_Text /= "" then
         return Integer'Max (Integer'Value (Line_Length_Text), 1);
      else
         return 72;
      end if;
   end Line_Length;

   -----------------
   -- Output_File --
   -----------------

   function Output_File  return String is
   begin
      return Get_Argument ("o", "output");
   end Output_File;

   -----------------
   -- Plugin_Name --
   -----------------

   function Plugin_Name  return String is
   begin
      return Get_Argument ("", "plugin");
   end Plugin_Name;

   -----------------
   -- Plugin_Path --
   -----------------

   function Plugin_Path  return String is
   begin
      return Get_Argument ("", "plugin-dir");
   end Plugin_Path;

   -------------
   -- Profile --
   -------------

   function Profile return Boolean is
   begin
      return Get_Flag ("", "profile");
   end Profile;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name  return String is
   begin
      return Get_Argument ("", "project-name");
   end Project_Name;

   ------------------
   -- Project_Root --
   ------------------

   function Project_Root return String is
   begin
      return Get_Argument ("", "project-root");
   end Project_Root;

   --------------------------------
   -- Project_Source_Directories --
   --------------------------------

   function Project_Source_Directories return String is
   begin
      return Get_Argument ("", "source-directories");
   end Project_Source_Directories;

   -------------------------------
   -- Project_Source_Extensions --
   -------------------------------

   function Project_Source_Extensions return String is
   begin
      return Get_Argument ("", "source-extensions");
   end Project_Source_Extensions;

   --------------
   -- Renderer --
   --------------

   function Renderer  return String is
   begin
      return Get_Argument ("r", "render", "text");
   end Renderer;

   ------------------
   -- Session_File --
   ------------------

   function Session_File  return String is
   begin
      return Get_Argument ("S", "session");
   end Session_File;

   ------------
   -- Target --
   ------------

   function Target  return String is
   begin
      return Get_Argument ("t", "target");
   end Target;

   -----------
   -- Theme --
   -----------

   function Theme  return String is
   begin
      return Get_Argument ("T", "theme");
   end Theme;

   -------------
   -- Version --
   -------------

   function Version return Boolean is
   begin
      return Get_Flag ("v", "version");
   end Version;

end Aquarius.Command_Line;
