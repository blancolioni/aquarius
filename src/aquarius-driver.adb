with Ada.Directories;
with Ada.Text_IO;

with Ack.Compile;

with Aqua.Server;

with Aquarius.Devices.Meta;
with Aquarius.Grammars.Manager;
with Aquarius.Library;
with Aquarius.Loader;
with Aquarius.Options;
with Aquarius.Plugins.Manager;
with Aquarius.Programs;
with Aquarius.Version;

procedure Aquarius.Driver is
   Trace_Server : Boolean;
begin

   if not Aquarius.Options.Load then
      return;
   end if;

   Trace_Server := Aquarius.Options.Trace_Server;

   Aquarius.Library.Initialize;

   Aquarius.Plugins.Manager.Load
     (Aquarius.Grammars.Manager.Get_Grammar ("ebnf"));

   declare
      Start_Class : constant String := Aquarius.Options.Start_Class;
   begin
      if Start_Class /= "" then
         Ack.Compile.Load_Root_Class
           (Source_Path => Start_Class);

         declare
            Server : constant Aqua.Server.Reference :=
                       Aqua.Server.Create ("./share/aqua_vm/aqua.config",
                                           "./.aquarius/tmp/obj");
            Base_Name : constant String :=
                          Ada.Directories.Base_Name (Start_Class);
            Object_Path : constant String :=
                            Ada.Directories.Compose
                              ("./.aquarius/tmp/obj/", Base_Name, "o");
         begin
            Server.Install_Device
              (Base   => 16#FFFF_F200#,
               Bound  => 16#FFFF_F300#,
               Device => Aquarius.Devices.Meta.Create (Server));

            Server.Load (Object_Path);
            Server.Run (Trace => Trace_Server);
         end;
         return;
      end if;
   end;

   if Aquarius.Options.Source_File_Count > 0 then
      for I in 1 .. Aquarius.Options.Source_File_Count loop
         declare
            Path : constant String := Aquarius.Options.Source_File (I);
            Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                        Aquarius.Grammars.Manager.Get_Grammar_For_File
                          (File_Name => Path);
            Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Loader.Load_From_File
                          (Grammar, Path);
         begin
            Ada.Text_IO.Put_Line (Program.Image);
         end;
      end loop;
      return;
   end if;

   Ada.Text_IO.Put_Line (Aquarius.Version.Version_String);

end Aquarius.Driver;
