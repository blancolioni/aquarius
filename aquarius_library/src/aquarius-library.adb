with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Ack.Compile;
with Ack.Loader;

with Aqua.Server;

with Aquarius.Actions;
with Aquarius.Configuration;
with Aquarius.Devices.Character_Handler;
with Aquarius.Devices.Meta;
with Aquarius.Devices.Real_Handler;
with Aquarius.Devices.Tagatha_Device;
with Aquarius.Devices.Text_Writer;
with Aquarius.Grammars.Manager;
with Aquarius.Logging;
with Aquarius.Messages.Console;
with Aquarius.Reader;
with Aquarius.Options;
with Aquarius.Plugins.Manager;
with Aquarius.Programs.Arrangements;
with Aquarius.Programs.Device;
with Aquarius.Rendering.Text;
with Aquarius.Sources.Files;
with Aquarius.Streams.Files;
with Aquarius.Streams.Strings;

with Kosei.Json;

with Resources;

with Tagatha.Code;

package body Aquarius.Library is

   function Find_Configuration return String;

   function Load_Aqua_Class
     (Path : String)
      return Aquarius.Programs.Program_Tree;

   procedure Check_Assembly_Package (Name : String);

   procedure Check_Directory (Path : String);

   package Aquarius_Resources is
     new Resources ("aquarius");
   --  The literal crate name "aquarius" is intentional here, not the
   --  generated Aquarius_Library_Config.Crate_Name ("aquarius_library"):
   --  it is used to locate share/aquarius/... at the installation prefix,
   --  which does not move when this package moves between crates.

   ----------------------------
   -- Check_Assembly_Package --
   ----------------------------

   procedure Check_Assembly_Package (Name : String) is
      Assembled : Boolean;
   begin
      Ack.Compile.Check_Assembly_Package (Name, Assembled);
      if Assembled then
         Ada.Text_IO.Put_Line ("built " & Name);
      end if;
   end Check_Assembly_Package;

   ---------------------
   -- Check_Directory --
   ---------------------

   procedure Check_Directory (Path : String) is
   begin
      if not Ada.Directories.Exists (Path) then
         Ada.Directories.Create_Directory (Path);
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("cannot create directory: " & Path);
   end Check_Directory;

   ------------------------
   -- Configuration_Path --
   ------------------------

   function Configuration_Path return String is
   begin
      return Aquarius_Resources.Resource_Path;
   end Configuration_Path;

   ------------------------
   -- Find_Configuration --
   ------------------------

   function Find_Configuration return String is

      function Try (Path : String) return Boolean;

      ---------
      -- Try --
      ---------

      function Try (Path : String) return Boolean is
      begin
         return Ada.Directories.Exists
           (Ada.Directories.Compose
              (Path, "aquarius.json"));
      end Try;

   begin
      declare
         Path : constant String := Aquarius_Resources.Resource_Path;
      begin
         if Try (Path) then
            return Path;
         end if;
      end;

      return "";
   end Find_Configuration;

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Boolean is
   begin
      if not Aquarius.Options.Load then
         return False;
      end if;

      declare
         Config_Path : constant String := Find_Configuration;
      begin
         if Config_Path = "" then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Unable to locate Aquarius configuration files");
            raise Configuration_Error;
         end if;

         Kosei.Json.Add_Json_Config
           (Ada.Directories.Compose
              (Config_Path, "aquarius.json"));

         Ack.Loader.Set_Loader (Load_Aqua_Class'Access);

         --  --clear-cache: empty the temporary folder before it is
         --  repopulated. Must run after the config is loaded (paths come
         --  from Kosei) but before the assembly packages below are rebuilt
         --  into it.
         if Aquarius.Options.Clear_Cache then
            declare
               Temp : constant String :=
                        Aquarius.Configuration.Temporary_Path;
            begin
               if Ada.Directories.Exists (Temp) then
                  Ada.Text_IO.Put_Line ("clearing cache: " & Temp);
                  Ada.Directories.Delete_Tree (Temp);
               end if;
            end;
         end if;

         Check_Directory (Aquarius.Configuration.Temporary_Path);
         Check_Directory (Aquarius.Configuration.Generated_Path);
         Check_Directory (Aquarius.Configuration.Assembly_Path);
         Check_Directory (Aquarius.Configuration.Object_Path);

         Check_Assembly_Package ("system-os");
         Check_Assembly_Package ("mm");
         Check_Assembly_Package ("aquarius-init");
         Check_Assembly_Package ("system-exceptions");

         Aquarius.Devices.Register
           ("aqua-text-writer", Aquarius.Devices.Text_Writer.Create);
         Aquarius.Devices.Register
           ("aqua-character-handler",
            Aquarius.Devices.Character_Handler.Create);
         Aquarius.Devices.Register
           ("aqua-real-handler",
            Aquarius.Devices.Real_Handler.Create);
         Aquarius.Devices.Register
           ("aquarius-program-tree",
            Aquarius.Programs.Device.Aquarius_Tree_Driver);
         Aquarius.Devices.Register
           ("tagatha", Aquarius.Devices.Tagatha_Device.Create);

         if not Aquarius.Plugins.Manager.Load
           (Aquarius.Grammars.Manager.Get_Grammar ("ebnf"))
         then
            raise Program_Error with
              "failed to creat EBNF Grammar";
         end if;

         if Aquarius.Options.Aqua_Trace then
            if Ada.Directories.Exists ("trace.txt") then
               Ada.Directories.Delete_File ("trace.txt");
            end if;
         end if;

         Aquarius.Logging.Start_Logging;

         Tagatha.Code.Set_Trace_Callback (Aquarius.Logging.Log'Access);

         Tagatha.Code.Enable_Trace
           (Enable_P_Code    => Aquarius.Options.Tagatha_Trace_P_Code,
            Enable_Transfers => Aquarius.Options.Tagatha_Trace_Transfers,
            Enable_Improvements =>
              Aquarius.Options.Tagatha_Trace_Improvements);
      end;

      return True;
   end Initialize;

   ---------------------
   -- Load_Aqua_Class --
   ---------------------

   function Load_Aqua_Class
     (Path : String)
      return Aquarius.Programs.Program_Tree
   is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar ("aqua");
      Source  : constant Aquarius.Sources.Source_Reference :=
                  Aquarius.Sources.Files.File_Source (Path);
      Stream  : constant Aquarius.Streams.Reader_Reference :=
                  Aquarius.Streams.Files.File_Reader (Path);
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Reader.Read
                    (Grammar =>  Grammar,
                     Source  =>  Source,
                     Stream  =>  Stream);
   begin
      return Program;
   end Load_Aqua_Class;

   ---------
   -- Run --
   ---------

   function Run return Boolean is
   begin

      if not Initialize then
         return True;
      end if;

      declare
         Start_Class : constant String := Aquarius.Options.Start_Class;
      begin
         if Start_Class /= "" then
            Ack.Compile.Load_Root_Class
              (Source_Path => Start_Class);

            declare
               Server : constant Aqua.Server.Reference :=
                 Aqua.Server.Create
                   (Configuration_Path
                    & "/aqua_vm/aqua.config",
                    Configuration_Path
                    & "/tmp/obj",
                    Quiet => not Aquarius.Options.Verbose);
               Base_Name : constant String :=
                             Ada.Directories.Base_Name (Start_Class);
               Object_Path : constant String :=
                 Ada.Directories.Compose
                   (Configuration_Path & "/tmp/obj",
                    Base_Name, "o");
            begin
               Server.Install_Device
                 (Base   => 16#FFFF_F200#,
                  Bound  => 16#FFFF_F300#,
                  Device => Aquarius.Devices.Meta.Create (Server));

               Server.Load (Object_Path);
               Server.Run (Trace => Aquarius.Options.Aqua_Trace);
               declare
                  Exit_Status : constant Natural :=
                                  Natural (Server.Exit_Status);
               begin
                  Ada.Command_Line.Set_Exit_Status
                    (Ada.Command_Line.Exit_Status (Exit_Status));
               end;
            end;
            Shut_Down;
            return True;
         end if;
      end;

      declare
         Check_Path : constant String := Aquarius.Options.Check_File;
      begin
         if Check_Path /= "" then
            declare
               use type Aquarius.Grammars.Aquarius_Grammar;
               use Aquarius.Messages;
            begin
               if Ada.Directories.Extension (Check_Path) = "ebnf" then

                  --  Load the file as a grammar: this compiles it, runs the
                  --  analyse actions and Check_Grammar, reporting any errors
                  --  to the console. A null result means errors were found.

                  declare
                     Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                       Aquarius.Grammars.Manager.Load_Grammar_From_File
                         (Name => Ada.Directories.Base_Name (Check_Path),
                          Path => Check_Path);
                  begin
                     if Grammar = null then
                        Ada.Command_Line.Set_Exit_Status (1);
                     else
                        Ada.Text_IO.Put_Line (Check_Path & ": no errors");
                     end if;
                  end;

               else

                  --  Parse the file under its grammar and report any
                  --  messages attached to the resulting tree, without
                  --  rendering it.

                  declare
                     Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                       Aquarius.Grammars.Manager.Get_Grammar_For_File
                         (File_Name => Check_Path);
                  begin
                     if Grammar = null then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           Check_Path & ": no grammar found");
                        Ada.Command_Line.Set_Exit_Status (1);
                     elsif not Aquarius.Plugins.Manager.Load (Grammar) then
                        Ada.Command_Line.Set_Exit_Status (1);
                     else
                        declare
                           Source : constant
                             Aquarius.Sources.Source_Reference :=
                             Aquarius.Sources.Files.File_Source (Check_Path);
                           Stream : constant
                             Aquarius.Streams.Reader_Reference :=
                             Aquarius.Streams.Files.File_Reader (Check_Path);
                           Program : constant Aquarius.Programs.Program_Tree :=
                             Aquarius.Reader.Read
                               (Grammar => Grammar,
                                Source  => Source,
                                Stream  => Stream);
                           List : Message_List;
                        begin
                           --  Run the semantic checks, then report messages
                           --  attached to the tree. Syntax errors are
                           --  recorded by the reader as an error message on
                           --  the tree root (details printed to standard
                           --  error).
                           Grammar.Run_Action_Trigger
                             (Program, Aquarius.Actions.Semantic_Trigger);
                           Program.Get_Messages (List);
                           Aquarius.Messages.Console.Show_Messages (List);
                           if Highest_Level (List) > Warning then
                              Ada.Command_Line.Set_Exit_Status (1);
                           else
                              Ada.Text_IO.Put_Line
                                (Check_Path & ": no errors");
                           end if;
                        end;
                     end if;
                  end;

               end if;
            end;
            Shut_Down;
            return True;
         end if;
      end;

      declare
         Test_Path : constant String := Aquarius.Options.Test_File;
      begin
         if Test_Path /= "" then
            declare
               use type Aquarius.Grammars.Aquarius_Grammar;
               use Aquarius.Messages;
               Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                 Aquarius.Grammars.Manager.Get_Grammar_For_File
                   (File_Name => Test_Path);
            begin
               if Grammar = null then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Test_Path & ": no grammar found");
                  Ada.Command_Line.Set_Exit_Status (1);
               elsif not Aquarius.Plugins.Manager.Load (Grammar) then
                  Ada.Command_Line.Set_Exit_Status (1);
               else
                  declare
                     Source : constant Aquarius.Sources.Source_Reference :=
                       Aquarius.Sources.Files.File_Source (Test_Path);
                     Stream : constant Aquarius.Streams.Reader_Reference :=
                       Aquarius.Streams.Files.File_Reader (Test_Path);
                     Program : constant Aquarius.Programs.Program_Tree :=
                       Aquarius.Reader.Read
                         (Grammar => Grammar,
                          Source  => Source,
                          Stream  => Stream);
                     List : Message_List;
                  begin
                     --  Run the semantic checks to populate the entity
                     --  model, then run the test actions, which inspect the
                     --  recorded declarations and cross references and
                     --  attach any failure messages to the tree.
                     Grammar.Run_Action_Trigger
                       (Program, Aquarius.Actions.Semantic_Trigger);
                     Grammar.Run_Action_Trigger
                       (Program, Aquarius.Actions.Test_Trigger);
                     Program.Get_Messages (List);
                     Aquarius.Messages.Console.Show_Messages (List);
                     if Highest_Level (List) > Warning then
                        Ada.Command_Line.Set_Exit_Status (1);
                     else
                        Ada.Text_IO.Put_Line (Test_Path & ": tests passed");
                     end if;
                  end;
               end if;
            end;
            Shut_Down;
            return True;
         end if;
      end;

      if Aquarius.Options.Source_File_Count > 0 then
         for I in 1 .. Aquarius.Options.Source_File_Count loop
            declare
               use all type Ada.Directories.File_Kind;
               use type Aquarius.Grammars.Aquarius_Grammar;
               Path : constant String := Aquarius.Options.Source_File (I);
               Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                           Aquarius.Grammars.Manager.Get_Grammar_For_File
                             (File_Name => Path);
               Output_Path : constant String :=
                               Aquarius.Options.Output_Path;
            begin

               if Output_Path /= "" then
                  if not Ada.Directories.Exists (Output_Path) then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        Output_Path & ": no such directory");
                     Ada.Command_Line.Set_Exit_Status (1);
                     Shut_Down;
                     return True;
                  end if;
                  if Ada.Directories.Kind (Output_Path)
                    /= Directory
                  then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        Output_Path & ": not a directory");
                     Ada.Command_Line.Set_Exit_Status (1);
                     Shut_Down;
                     return True;
                  end if;
               end if;

               if Grammar /= null then
                  if not Aquarius.Plugins.Manager.Load (Grammar) then
                     Ada.Command_Line.Set_Exit_Status (1);
                     Shut_Down;
                     return True;
                  end if;

                  declare
                     use type Aquarius.Messages.Message_Level;
                     Source : constant Aquarius.Sources.Source_Reference :=
                                Aquarius.Sources.Files.File_Source (Path);
                     Stream : constant Aquarius.Streams.Reader_Reference :=
                                Aquarius.Streams.Files.File_Reader (Path);
                     Program : constant Aquarius.Programs.Program_Tree :=
                                 Aquarius.Reader.Read
                                   (Grammar =>  Grammar,
                                    Source  =>  Source,
                                    Stream  =>  Stream);
                     Writer  : constant Aquarius.Streams.Writer_Reference :=
                                 Aquarius.Streams.Strings.String_Writer;
                     Render  :
                       Aquarius.Rendering.Root_Aquarius_Renderer'Class :=
                         Aquarius.Rendering.Text.Stream_Renderer
                           (Writer);
                     Messages : Aquarius.Messages.Message_List;
                  begin
                     Grammar.Run_Action_Trigger
                       (Program, Aquarius.Actions.Semantic_Trigger);

                     Program.Get_Messages (Messages);
                     Aquarius.Messages.Console.Show_Messages (Messages);

                     if Aquarius.Messages.Highest_Level (Messages)
                       > Aquarius.Messages.Warning
                     then
                        Ada.Command_Line.Set_Exit_Status (1);
                     else
                        Aquarius.Programs.Arrangements.Arrange_Via_Docs
                          (Program, 30);
                        Aquarius.Programs.Arrangements.Render
                          (Program, Render);

                        if Aquarius.Options.Pretty_Print then
                           if Output_Path = "" then
                              Ada.Text_IO.Put_Line (Writer.To_String);
                           else
                              declare
                                 use Ada.Directories, Ada.Text_IO;
                                 File      : File_Type;
                                 Full_Path : constant String :=
                                               Compose (Output_Path,
                                                        Simple_Name (Path));
                              begin
                                 Create (File, Out_File, Full_Path);
                                 Put_Line (File, Writer.To_String);
                                 Close (File);
                              exception
                                 when Ada.Text_IO.Name_Error =>
                                    Ada.Text_IO.Put_Line
                                      (Compose (Output_Path,
                                       Simple_Name (Path))
                                       & ": cannot open for writing");
                                    Ada.Command_Line.Set_Exit_Status (1);
                              end;
                           end if;
                        end if;

                        if Aquarius.Options.Code_Trigger then
                           Grammar.Run_Action_Trigger
                             (Program, Aquarius.Actions.Code_Trigger);
                        end if;
                     end if;
                  end;
               else
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Path & ": no grammar found");
               end if;
            end;
         end loop;
         Shut_Down;
         return True;
      end if;

      Shut_Down;
      return False;

   exception
      when others =>
         Shut_Down;
         raise;
   end Run;

   ---------------
   -- Shut_Down --
   ---------------

   procedure Shut_Down is
   begin
      Aquarius.Logging.Stop_Logging;
   end Shut_Down;

end Aquarius.Library;
