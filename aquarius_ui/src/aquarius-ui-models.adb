with Aquarius.Locations;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Text;
with Aquarius.Streams.Strings;

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
     (Program : Aquarius.Programs.Program_Tree)
      return Aquarius.UI.Editor.Model_Reference
   is
   begin
      return new Instance'
        (Program => Program,
         Point   => Aquarius.Trees.Cursors.Left_Of_Tree (Program),
         Partial => <>);
   end Create_Model;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (This : Insert_Character_Instance) is
   begin
      Ada.Strings.Unbounded.Append (This.Model.Partial, This.Ch);
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
