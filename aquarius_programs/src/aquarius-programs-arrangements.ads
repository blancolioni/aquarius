with Aquarius.Rendering;
with Aquarius.Trees.Cursors;

package Aquarius.Programs.Arrangements is

   procedure Arrange
     (Item        : Program_Tree;
      Messages    : out Aquarius.Messages.Message_List;
      Line_Length : Positive      := 72);

   procedure Arrange
     (Item             : Program_Tree;
      Point            : Aquarius.Trees.Cursors.Cursor;
      Partial          : String;
      Updating         : Boolean;
      Partial_Line     : out Aquarius.Layout.Line_Number;
      Partial_Column   : out Aquarius.Layout.Column_Number;
      Line_Length      : Positive      := 72);

   procedure Render
     (Program        : Program_Tree;
      Renderer       : in out Rendering.Root_Aquarius_Renderer'Class;
      Point          : Aquarius.Trees.Cursors.Cursor;
      Partial        : String;
      Updating       : Boolean;
      Partial_Line   : Aquarius.Layout.Line_Number;
      Partial_Column : Aquarius.Layout.Column_Number);

   --  Render Program using the given Renderer, and the Partial
   --  edit at Point, at the position given by Partial_Start

   procedure Render
     (Program     : Program_Tree;
      Renderer    : in out Aquarius.Rendering.Root_Aquarius_Renderer'Class);

end Aquarius.Programs.Arrangements;
