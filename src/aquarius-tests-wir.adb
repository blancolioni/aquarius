with Aquarius.Grammars.Manager;
with Aquarius.Programs.Arrangements;
with Aquarius.Reader;
with Aquarius.Rendering.Text;
with Aquarius.Sources.Files;
with Aquarius.Streams.Files;
with Aquarius.Streams.Strings;

with WL.Unit.Compare_Test;

package body Aquarius.Tests.Wir is

   function Id (S : String) return String is (S);

   package Wir_Tests is
      new WL.Unit.Compare_Test (String, Id, Compare);

   function Render (Path : String; Width : Positive) return String;
   --  Parse the .wir file at Path, lay it out with the Arrange_Via_Docs
   --  pilot (not the old Arrange), and render the result to a string --
   --  the same non-GTK render path Aquarius.UI.Models.Contents uses,
   --  minus the live-edit Point/Partial arguments this harness has no
   --  use for. Reads a real file, not a string, because
   --  Aquarius.Streams.Strings.Reader_Instance's Line/Column tracking
   --  is broken for multi-line input (Line is hardcoded to 1, Column
   --  never resets at a newline) -- a pre-existing bug unrelated to
   --  this pilot, sidestepped rather than fixed here.

   function Short_Call_Test return String;
   function Long_Argument_List_Test return String;
   function Long_Condition_Test return String;

   ----------
   -- Load --
   ----------

   NL : constant Character := Character'Val (10);

   procedure Load (Suite : in out WL.Unit.Test_Suite) is
   begin
      Suite.Append
        (Wir_Tests.Test
           ("wir-short-call-fits", Short_Call_Test'Access,
            "routine foo args 0 locals 0 is" & NL
            & "   call bar ();" & NL
            & "end"));
      Suite.Append
        (Wir_Tests.Test
           ("wir-long-argument-list-wraps",
            Long_Argument_List_Test'Access,
            "routine foo args 0 locals 0" & NL
            & "is" & NL
            & "   call bar (arg 1," & NL
            & "   arg 2," & NL
            & "   arg 3," & NL
            & "   arg 4," & NL
            & "   arg 5);" & NL
            & "end"));
      Suite.Append
        (Wir_Tests.Test
           ("wir-long-condition-closing-on-own-line",
            Long_Condition_Test'Access,
            "routine foo args 0 locals 0" & NL
            & "is" & NL
            & "   if (arg 1 = arg 2)" & NL
            & "   then" & NL
            & "      call bar ();" & NL
            & "   end if;" & NL
            & "end"));
   end Load;

   ------------------------------
   -- Long_Argument_List_Test --
   ------------------------------

   function Long_Argument_List_Test return String is
   begin
      return Render
        ("share/aquarius/tests/wir/pilot_long_argument_list.wir", 20);
   end Long_Argument_List_Test;

   --------------------------
   -- Long_Condition_Test --
   --------------------------

   function Long_Condition_Test return String is
   begin
      return Render
        ("share/aquarius/tests/wir/pilot_long_condition.wir", 20);
   end Long_Condition_Test;

   ------------
   -- Render --
   ------------

   function Render (Path : String; Width : Positive) return String is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Grammars.Manager.Get_Grammar ("wir");
      Source  : constant Aquarius.Sources.Source_Reference :=
                  Aquarius.Sources.Files.File_Source (Path);
      Reader  : constant Aquarius.Streams.Reader_Reference :=
                  Aquarius.Streams.Files.File_Reader (Path);
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Reader.Read
                    (Grammar => Grammar,
                     Source  => Source,
                     Stream  => Reader);
      Stream   : constant Aquarius.Streams.Writer_Reference :=
                   Aquarius.Streams.Strings.String_Writer;
      Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                   Aquarius.Rendering.Text.Stream_Renderer (Stream);
   begin
      Aquarius.Programs.Arrangements.Arrange_Via_Docs (Program, Width);
      Aquarius.Programs.Arrangements.Render (Program, Renderer);
      return Stream.To_String;
   end Render;

   -----------------------
   -- Short_Call_Test --
   -----------------------

   function Short_Call_Test return String is
   begin
      return Render ("share/aquarius/tests/wir/pilot_short_call.wir", 40);
   end Short_Call_Test;

end Aquarius.Tests.Wir;
