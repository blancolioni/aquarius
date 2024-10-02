with Aquarius.Locations;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Text;
with Aquarius.Streams.Strings;
with Aquarius.Tokens;

package body Aquarius.UI.Models is

   type Insert_Character_Instance is
     new Model_Command_Instance with
      record
         Ch : Character;
      end record;

   overriding function Tag
     (This : Insert_Character_Instance) return String
   is ("insert-character");

   overriding function Description
     (This : Insert_Character_Instance)
      return String
   is ("insert '" & This.Ch & "'");

   overriding procedure Execute (This : Insert_Character_Instance);
   overriding procedure Undo (This : Insert_Character_Instance);

   --------------
   -- Contents --
   --------------

   overriding function Contents
     (This : Instance)
      return String
   is
      Stream : constant Aquarius.Streams.Writer_Reference :=
                 Aquarius.Streams.Strings.String_Writer;
      Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                   Aquarius.Rendering.Text.Stream_Renderer (Stream);
      Line     : Aquarius.Locations.Line_Index;
      Col      : Aquarius.Locations.Column_Index;
   begin

      Aquarius.Programs.Arrangements.Arrange
        (Item           => This.Program,
         Point          => This.Point,
         Partial        => -This.Partial,
         Updating       => False,
         Partial_Line   => Line,
         Partial_Column => Col);

      Aquarius.Programs.Arrangements.Render
        (Program        => This.Program,
         Renderer       => Renderer,
         Point          => This.Point,
         Partial        => -This.Partial,
         Updating       => False,
         Partial_Line   => Line,
         Partial_Column => Col);

      return Stream.To_String;
   end Contents;

   ------------------
   -- Create_Model --
   ------------------

   function Create_Model
     (Grammar : Aquarius.Grammars.Aquarius_Grammar;
      Program : Aquarius.Programs.Program_Tree)
      return Aquarius.UI.Editor.Model_Reference
   is
      Model : Instance := Instance'
        (Grammar => Grammar,
         Program => Program,
         Context => <>,
         Point   => Aquarius.Trees.Cursors.Left_Of_Tree (Program),
         Partial => <>);
   begin
      Model.Context.Initialise_Parse_Context
        (Grammar     => Grammar,
         Root        => Program,
         Interactive => True,
         Run_Actions => True);
      return new Instance'(Model);
   end Create_Model;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (This : Insert_Character_Instance) is
   begin
      Ada.Strings.Unbounded.Append (This.Model.Partial, This.Ch);
      This.Model.Read_Partial (False);
   end Execute;

   -------------------------------
   -- Insert_Character_At_Point --
   -------------------------------

   overriding function Insert_Character_At_Point
     (This : not null access Instance;
      Ch   : Character)
      return Command_Interface'Class
   is
   begin
      return Insert_Character_Instance'
        (Model => Reference (This), Ch => Ch);
   end Insert_Character_At_Point;

   ------------------
   -- Read_Partial --
   ------------------

   procedure Read_Partial
     (This    : in out Instance'Class;
      Leaving : Boolean)
   is
      Text       : constant String := -This.Partial;
      Complete   : Boolean;
      Unique     : Boolean;
      Have_Class : Boolean;
      Class      : Aquarius.Tokens.Token_Class;
      Tok        : Aquarius.Tokens.Token;
      First      : Natural := 1;
      Next       : Natural;

      function Token_OK (Tok : Aquarius.Tokens.Token) return Boolean
      is (Aquarius.Programs.Parser.Token_OK (Tok, This.Context));

   begin
      Aquarius.Tokens.Scan
        (Frame      => This.Grammar.Frame,
         Text       => Text,
         Partial    => not Leaving,
         Complete   => Complete,
         Have_Class => Have_Class,
         Unique     => Unique,
         Class      => Class,
         Tok        => Tok,
         First      => First,
         Last       => Next,
         Token_OK   => Token_OK'Access);
      if Have_Class then
         if Token_OK (Tok) then
            Aquarius.Programs.Parser.Parse_Token
              (Tok, Text (First .. Next), This.Context);
         end if;
      end if;
   end Read_Partial;

   ----------
   -- Undo --
   ----------

   overriding procedure Undo
     (This : Insert_Character_Instance)
   is
      use Ada.Strings.Unbounded;
   begin
      Delete
        (Source => This.Model.Partial,
         From    => Length (This.Model.Partial),
         Through => Length (This.Model.Partial));
   end Undo;

end Aquarius.UI.Models;
