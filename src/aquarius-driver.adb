with Ada.Directories;
with Ada.Text_IO;

with Ack.Compile;

with Aqua.Server;

with Aquarius.Grammars.Manager;
with Aquarius.Library;
with Aquarius.Options;
with Aquarius.Plugins.Manager;
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
      Assembled : Boolean;
   begin
      Ack.Compile.Check_Assembly_Package ("system-os", Assembled);
      if Assembled then
         Ada.Text_IO.Put_Line ("built System.OS");
      end if;
   end;

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
            Server.Load (Object_Path);
            Server.Run (Trace => Trace_Server);
         end;
         return;
      end if;
   end;

   Ada.Text_IO.Put_Line (Aquarius.Version.Version_String);

end Aquarius.Driver;
