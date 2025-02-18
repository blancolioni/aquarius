with Ada.Directories;
with Ada.Text_IO;

package body Aquarius.Logging is

   Current_Log_Level : Log_Level := 3;
   Started           : Boolean := False;
   HT                : constant Character :=
                         Character'Val (9);
   Log_File          : Ada.Text_IO.File_Type;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Log (3, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Level   : Log_Level;
      Message : String)
   is
   begin
      if Started and then Level <= Current_Log_Level then
         declare
            Log_Message : constant String :=
                            Message;
         begin
            Ada.Text_IO.Put_Line (Log_File, Log_Message);
         end;
      end if;
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Category : String;
                  Message  : String)
   is
   begin
      Log (3, Category, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Level    : Log_Level;
                  Category : String;
                  Message  : String)
   is
   begin
      Log (Level, Category &  HT & Message);
   end Log;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Message : String;
      E       : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Log_Exception (1, Message, E);
   end Log_Exception;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Level   : Log_Level;
      Message : String;
      E       : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      if Started then
         Log
           (Level   => Level,
            Message =>
              Message & ": "
            & Ada.Exceptions.Exception_Name (E)
            & ": " & Ada.Exceptions.Exception_Message (E));
         Log (Ada.Exceptions.Exception_Information (E));
      end if;
   end Log_Exception;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging
     (Path : String;
      Level : Log_Level := 3)
   is
   begin
      if not Ada.Directories.Exists ("log") then
         Ada.Directories.Create_Directory ("log");
      end if;
      Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File, Path);
      Current_Log_Level := Level;
      Started := True;
      Log ("log started");
   end Start_Logging;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging (Level : Log_Level := 3) is
   begin
      Start_Logging ("log/aquarius.log", Level);
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      if Started then
         Log (1, "end of log");
         Ada.Text_IO.Close (Log_File);
         Started := False;
      end if;
   end Stop_Logging;

end Aquarius.Logging;
