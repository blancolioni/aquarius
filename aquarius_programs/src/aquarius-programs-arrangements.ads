with Aquarius.Rendering;
with Aquarius.Trees.Cursors;

package Aquarius.Programs.Arrangements is

   procedure Arrange
     (Item        : not null Program_Tree;
      Messages    : out Aquarius.Messages.Message_List;
      Line_Length : Positive      := 72);

   procedure Arrange
     (Item             : not null Program_Tree;
      Point            : Aquarius.Trees.Cursors.Cursor;
      Partial          : String;
      Updating         : Boolean;
      Partial_Line     : out Aquarius.Locations.Line_Index;
      Partial_Column   : out Aquarius.Locations.Column_Index;
      Line_Length      : Positive      := 72);

   procedure Render
     (Program        : not null Program_Tree;
      Renderer       : in out Rendering.Root_Aquarius_Renderer'Class;
      Point          : Aquarius.Trees.Cursors.Cursor;
      Partial        : String;
      Updating       : Boolean;
      Partial_Line   : Aquarius.Locations.Line_Index;
      Partial_Column : Aquarius.Locations.Column_Index);
   --  Render Program using the given Renderer, and the Partial
   --  edit at Point, at the position given by Partial_Start

   procedure Render
     (Program     : not null Program_Tree;
      Renderer    : in out Aquarius.Rendering.Root_Aquarius_Renderer'Class);

   procedure Arrange_Via_Docs
     (Item  : not null Program_Tree;
      Width : Positive := 72);
   --  Pilot alternative to Arrange: builds a Doc (Doc_Builder) and lays
   --  it out (Aquarius.Docs.Layout) instead of the imperative walk +
   --  Reformatting backward-patch. Does not touch Arrange/Reformatting
   --  or anything they use -- only code that calls this explicitly
   --  exercises the new path.

end Aquarius.Programs.Arrangements;
