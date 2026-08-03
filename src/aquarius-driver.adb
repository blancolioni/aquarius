with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Ack.Compile;

with Aqua.Server;

with Aquarius.Actions;
with Aquarius.Devices.Meta;
with Aquarius.Grammars.Manager;
with Aquarius.Library;
with Aquarius.Reader;
with Aquarius.Messages.Console;
with Aquarius.Options;
with Aquarius.Plugins.Manager;
with Aquarius.Programs;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Text;
with Aquarius.Sources.Files;
with Aquarius.Streams.Files;
with Aquarius.Tests;

procedure Aquarius.Driver is
begin

   if not Aquarius.Options.Load then
      return;
   end if;

   Aquarius.Library.Initialize;

   if Aquarius.Options.Self_Test then
      Aquarius.Tests.Run_Tests;
      Aquarius.Library.Shut_Down;
      return;
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
                (Aquarius.Library.Configuration_Path
                 & "/aqua_vm/aqua.config",
                 Aquarius.Library.Configuration_Path
                 & "/tmp/obj");
            Base_Name : constant String :=
                          Ada.Directories.Base_Name (Start_Class);
            Object_Path : constant String :=
              Ada.Directories.Compose
                (Aquarius.Library.Configuration_Path & "/tmp/obj",
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
         Aquarius.Library.Shut_Down;
         return;
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

               --  Parse the file under its grammar and report any messages
               --  attached to the resulting tree, without rendering it.

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
                        Source : constant Aquarius.Sources.Source_Reference :=
                          Aquarius.Sources.Files.File_Source (Check_Path);
                        Stream : constant Aquarius.Streams.Reader_Reference :=
                          Aquarius.Streams.Files.File_Reader (Check_Path);
                        Program : constant Aquarius.Programs.Program_Tree :=
                          Aquarius.Reader.Read
                            (Grammar => Grammar,
                             Source  => Source,
                             Stream  => Stream);
                        List : Message_List;
                     begin
                        --  Run the semantic checks, then report messages
                        --  attached to the tree. Syntax errors are recorded
                        --  by the reader as an error message on the tree
                        --  root (details printed to standard error).
                        Grammar.Run_Action_Trigger
                          (Program, Aquarius.Actions.Semantic_Trigger);
                        Program.Get_Messages (List);
                        Aquarius.Messages.Console.Show_Messages (List);
                        if Highest_Level (List) > Warning then
                           Ada.Command_Line.Set_Exit_Status (1);
                        else
                           Ada.Text_IO.Put_Line (Check_Path & ": no errors");
                        end if;
                     end;
                  end if;
               end;

            end if;
         end;
         Aquarius.Library.Shut_Down;
         return;
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
                  --  Run the semantic checks to populate the entity model,
                  --  then run the test actions, which inspect the recorded
                  --  declarations and cross references and attach any failure
                  --  messages to the tree.
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
         Aquarius.Library.Shut_Down;
         return;
      end if;
   end;

   if Aquarius.Options.Source_File_Count > 0 then
      for I in 1 .. Aquarius.Options.Source_File_Count loop
         declare
            use type Aquarius.Grammars.Aquarius_Grammar;
            Path : constant String := Aquarius.Options.Source_File (I);
            Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                        Aquarius.Grammars.Manager.Get_Grammar_For_File
                          (File_Name => Path);
         begin
            if Grammar /= null then
               if not Aquarius.Plugins.Manager.Load (Grammar) then
                  Ada.Command_Line.Set_Exit_Status (1);
                  return;
               end if;

               declare
                  Source : constant Aquarius.Sources.Source_Reference :=
                             Aquarius.Sources.Files.File_Source (Path);
                  Stream : constant Aquarius.Streams.Reader_Reference :=
                             Aquarius.Streams.Files.File_Reader (Path);
                  Program : constant Aquarius.Programs.Program_Tree :=
                              Aquarius.Reader.Read
                                (Grammar =>  Grammar,
                                 Source  =>  Source,
                                 Stream  =>  Stream);
                  Render  : Aquarius.Rendering.Root_Aquarius_Renderer'Class :=
                              Aquarius.Rendering.Text.File_Renderer
                                (Ada.Directories.Base_Name (Path)
                                 & "."
                                 & Ada.Directories.Extension (Path));
               begin
                  Grammar.Run_Action_Trigger
                    (Program, Aquarius.Actions.Semantic_Trigger);

                  Aquarius.Programs.Arrangements.Arrange_Via_Docs
                    (Program, 30);
                  Aquarius.Programs.Arrangements.Render
                    (Program, Render);

                  if Aquarius.Options.Code_Trigger then
                     Grammar.Run_Action_Trigger
                       (Program, Aquarius.Actions.Code_Trigger);
                  end if;
               end;
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Path & ": no grammar found");
            end if;
         end;
      end loop;
      Aquarius.Library.Shut_Down;
      return;
   end if;

   --  The GtkAda UI (aquarius_ui_gtk) is temporarily disabled; a bare
   --  launch with no file/command is a no-op for now.
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "aquarius: GUI disabled in this build; nothing to do.");
   Aquarius.Library.Shut_Down;

exception
   when others =>
      Aquarius.Library.Shut_Down;
      raise;
end Aquarius.Driver;
