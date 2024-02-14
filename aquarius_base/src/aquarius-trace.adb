with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Aquarius.Trace is

   Trace_File : Ada.Text_IO.File_Type;

   type Debug_Settings is array (Debug_Class) of Boolean;
   Enabled         : Debug_Settings := [others => False];
   Saved_Settings  : Debug_Settings;
   Tracing         : Boolean := False;
   Disable_Changes : Boolean := False;

   procedure Enable_By_Name (Name : String);

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Item : Debug_Class) return String is
   begin
      case Item is
         when Actions =>
            return "action";
         when Buffers =>
            return "buffer";
         when Configuration =>
            return "config";
         when Cursors =>
            return "cursor";
         when Entries =>
            return "entry";
         when Errors =>
            return "error";
         when Files =>
            return "files";
         when Formatting =>
            return "format";
         when Grammar =>
            return "grammar";
         when Parsing =>
            return "parse";
         when Plugins =>
            return "plugin";
         when Program_Trees =>
            return "program";
         when Properties =>
            return "property";
         when Scanning =>
            return "scan";
         when Syntax_Trees =>
            return "syntax";
         when Tokens =>
            return "token";
         when Trees =>
            return "tree";
         when Types =>
            return "types";
         when Watchers =>
            return "watcher";
      end case;
   end Debug_Name;

   ------------
   -- Enable --
   ------------

   procedure Enable (Class : Debug_Class) is
   begin
      if not Disable_Changes then
         Enabled (Class) := True;
         Tracing := True;
      end if;
   end Enable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Names : String) is
      Start, Finish : Natural := Names'First;
   begin
      Start := 1;
      while Finish > 0 loop
         Finish := Ada.Strings.Fixed.Index (Names, ",", Start);
         if Finish = 0 then
            Enable_By_Name (Names (Start .. Names'Last));
         else
            Enable_By_Name (Names (Start .. Finish - 1));
         end if;
         Start := Finish + 1;
      end loop;
   end Enable;

   ----------------
   -- Enable_All --
   ----------------

   procedure Enable_All is
   begin
      if not Disable_Changes then
         Enabled := [others => True];
         Tracing := True;
      end if;
   end Enable_All;

   --------------------
   -- Enable_By_Name --
   --------------------

   procedure Enable_By_Name (Name : String) is
   begin
      for I in Debug_Class loop
         if Debug_Name (I) = Name then
            Enable (I);
            return;
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Name & ": unknown debug setting");
      raise Constraint_Error with
        Name & ": unknown debug setting";
   end Enable_By_Name;

   ---------------
   -- End_Trace --
   ---------------

   procedure End_Trace is
   begin
      if Tracing then
         Ada.Text_IO.Put (Trace_File,
                          "Trace finished cleanly");
         Ada.Text_IO.Close (Trace_File);
         Disable_Changes := False;
         Tracing         := False;
         Enabled         := [others => False];
      end if;
   end End_Trace;

   ------------------
   -- Pop_Settings --
   ------------------

   procedure Pop_Settings is
   begin
      Enabled := Saved_Settings;
   end Pop_Settings;

   -------------------
   -- Push_Settings --
   -------------------

   procedure Push_Settings is
   begin
      Saved_Settings := Enabled;
      Enabled := [others => False];
   end Push_Settings;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (Class  : Debug_Class;
                      Column : Positive)
   is
   begin
      if Enabled (Class) then
         Ada.Text_IO.Set_Col (Trace_File,
                              Ada.Text_IO.Positive_Count (Column));
      end if;
   end Set_Col;

   -----------------
   -- Start_Trace --
   -----------------

   procedure Start_Trace is
   begin
      if Tracing then
         Ada.Text_IO.Create (Trace_File, Ada.Text_IO.Out_File, "trace.txt");
         Disable_Changes := True;
         Ada.Text_IO.Put_Line (Trace_File, "Aquarius trace started");
         Ada.Text_IO.Put (Trace_File,
                          "Enabled traces:");
         for I in Enabled'Range loop
            if Enabled (I) then
               Ada.Text_IO.Put (Trace_File, " " & Debug_Name (I));
            end if;
         end loop;
         Ada.Text_IO.New_Line (Trace_File);
      end if;
   end Start_Trace;

   --------------------
   -- Trace_Put_Line --
   --------------------

   procedure Trace_Put_Line (Class   : Debug_Class;
                             Message : String)
   is
   begin
      if Enabled (Class) then
         Ada.Text_IO.Put_Line (Trace_File,
                               Debug_Name (Class) & ": " & Message);
         Ada.Text_IO.Flush (Trace_File);
      end if;
   end Trace_Put_Line;

end Aquarius.Trace;
