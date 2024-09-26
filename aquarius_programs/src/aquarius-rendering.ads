with Aquarius.Locations;
with Aquarius.Programs;

package Aquarius.Rendering is

   type Renderer_Interface is interface
     and Aquarius.Locations.Location_Interface;

   type Root_Aquarius_Renderer is abstract new Renderer_Interface with private;

   overriding function Offset
     (This : Root_Aquarius_Renderer)
      return Aquarius.Locations.Location_Offset;

   overriding function Line
     (This : Root_Aquarius_Renderer)
      return Aquarius.Locations.Line_Index;

   overriding function Column
     (This : Root_Aquarius_Renderer)
      return Aquarius.Locations.Column_Index;

   procedure Set_Text (Renderer  : in out Root_Aquarius_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Line      : Aquarius.Locations.Line_Index;
                       Column    : Aquarius.Locations.Column_Index;
                       Class     : String;
                       Text      : String)
      is abstract;

   procedure Begin_Render (Renderer : in out Root_Aquarius_Renderer)
     is null;
   procedure End_Render (Renderer : in out Root_Aquarius_Renderer)
     is null;

   procedure Set_Point
     (Renderer : in out Root_Aquarius_Renderer;
      Line     : Aquarius.Locations.Line_Index;
      Column   : Aquarius.Locations.Column_Index)
   is null;

   subtype Aquarius_Renderer is Root_Aquarius_Renderer'Class;

private

   type Root_Aquarius_Renderer is abstract new Renderer_Interface with
      record
         Offset   : Aquarius.Locations.Location_Offset := 0;
         Line     : Aquarius.Locations.Line_Index   := 1;
         Column   : Aquarius.Locations.Column_Index := 1;
      end record;

   overriding function Offset
     (This : Root_Aquarius_Renderer)
      return Aquarius.Locations.Location_Offset
   is (This.Offset);

   overriding function Line
     (This : Root_Aquarius_Renderer)
      return Aquarius.Locations.Line_Index
   is (This.Line);

   overriding function Column
     (This : Root_Aquarius_Renderer)
      return Aquarius.Locations.Column_Index
   is (This.Column);

end Aquarius.Rendering;
