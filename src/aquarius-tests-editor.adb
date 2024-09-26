with Aquarius.Grammars.Manager;
with Aquarius.Locations;
with Aquarius.Programs;
with Aquarius.Reader;
with Aquarius.Sources;
with Aquarius.Streams.Strings;
with Aquarius.UI.Commands;
with Aquarius.UI.Editor;
with Aquarius.UI.Factories;

with WL.Unit.Compare_Test;

package body Aquarius.Tests.Editor is

   function Id (S : String) return String is (S);

   function Read_Stream_Test return String;
   function Insert_Character_Test return String;

   package Editor_Tests is
      new WL.Unit.Compare_Test (String, Id, Compare);

   ---------------------------
   -- Insert_Character_Test --
   ---------------------------

   function Insert_Character_Test return String is
      Source  : constant Aquarius.Sources.Source_Reference :=
                  Aquarius.Sources.Internal_Source
                    ("a/b/c.json", "json");
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar (Source);
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Programs.New_Program_Tree
                    (Syntax => Grammar.Get_Top_Level_Syntax,
                     Source => Source,
                     Location => Aquarius.Locations.To_Location (0, 1, 1));
      Editor  : constant Aquarius.UI.Editor.Model_Reference :=
                  Aquarius.UI.Factories.Editor_Model_Factory
                    .Create (Program);
      Command : constant Aquarius.UI.Command_Interface'Class :=
                  Editor.Insert_Character_At_Point ('x');
      Manager : Aquarius.UI.Commands.Command_Manager_Interface'Class :=
                  Aquarius.UI.Commands.Create_Command_Manager;
   begin
      Manager.Append_Command (Command);
      Manager.Next_Command;
      return Editor.Contents;
   end Insert_Character_Test;

   ----------
   -- Load --
   ----------

   procedure Load (Suite : in out WL.Unit.Test_Suite) is
   begin
      Suite.Append
        (Editor_Tests.Test
           ("insert-single-character", Insert_Character_Test'Access, "x"));
      Suite.Append
        (Editor_Tests.Test
           ("read-simple-stream", Read_Stream_Test'Access,
            "{" & Character'Val (10)
            & "   ""prop"": 1" & Character'Val (10)
            & "}"));
   end Load;

   ----------------------
   -- Read_Stream_Test --
   ----------------------

   function Read_Stream_Test return String is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar_For_File
                    (File_Name => "input.json");
      Reader  : constant Aquarius.Streams.Reader_Reference :=
                  Aquarius.Streams.Strings.String_Reader
                    ("{ ""prop"": 1 }");
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Reader.Read
                    (Grammar => Grammar,
                     Source  =>
                       Aquarius.Sources.Internal_Source
                         ("read-stream-test", "json"),
                     Stream  => Reader);
      Editor  : constant Aquarius.UI.Editor.Model_Reference :=
                  Aquarius.UI.Factories.Editor_Model_Factory
                    .Create (Program);
   begin
      return Editor.Contents;
   end Read_Stream_Test;

end Aquarius.Tests.Editor;
