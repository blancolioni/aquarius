with Ada.Text_IO;

with Ack.Compile;

with Aqua.Server;

with Aquarius.Grammars.Manager;
with Aquarius.Library;
with Aquarius.Loader;
with Aquarius.Plugins.Manager;
with Aquarius.Programs;

with Kosei;

procedure Aquarius.Driver is
   Trace_Server : constant Boolean := False;
   Test_Name    : constant String := "test";
begin
   Aquarius.Library.Initialize;
   Ada.Text_IO.Put_Line
     (Kosei.Get ("/version"));
   Ada.Text_IO.Put_Line
     (Kosei.Get ("/path"));

   Aquarius.Plugins.Manager.Load
     (Aquarius.Grammars.Manager.Get_Grammar ("ebnf"));

   declare
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar ("json");
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Loader.Load_From_File
                    (Grammar,
                     "./share/aquarius/tests/json/simple.json");
   begin
      Ada.Text_IO.Put_Line (Program.Image);
   end;

   declare
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar ("idle");
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Loader.Load_From_File
                    (Grammar,
                     "./.aquarius/grammar/idle/samples/memory.idl");
   begin
      Ada.Text_IO.Put_Line (Program.Image);
   end;

   Ack.Compile.Load_Root_Class
     (Source_Path => "./share/aquarius/tests/aqua/" & Test_Name & ".aqua");

   Aquarius.Plugins.Manager.Loaded_Plugin_Report;

   declare
      Assembled : Boolean;
   begin
      Ack.Compile.Check_Assembly_Package ("system-os", Assembled);
      if Assembled then
         Ada.Text_IO.Put_Line ("built System.OS");
      end if;
   end;

   declare
      Server : constant Aqua.Server.Reference :=
                 Aqua.Server.Create ("./share/aqua_vm/aqua.config",
                                     "./.aquarius/tmp/obj");
   begin
      Server.Load ("./.aquarius/tmp/obj/" & Test_Name & ".o");
      Server.Run (Trace => Trace_Server);
   end;

end Aquarius.Driver;
