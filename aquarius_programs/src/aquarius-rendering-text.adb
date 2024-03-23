with Ada.Text_IO;

package body Aquarius.Rendering.Text is

   type Root_Text_Renderer is new Root_Aquarius_Renderer with
      record
         File : Ada.Text_IO.File_Type;
      end record;

   overriding procedure Set_Text
     (Renderer    : in out Root_Text_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : Aquarius.Layout.Line_Number;
      Column      : Aquarius.Layout.Column_Number;
      Class       : String;
      Text        : String);

   type Root_File_Renderer is new Root_Text_Renderer with
      record
         null;
      end record;

   overriding procedure End_Render (Renderer : in out Root_File_Renderer);

   ----------------
   -- End_Render --
   ----------------

   overriding procedure End_Render (Renderer : in out Root_File_Renderer) is
   begin
      Ada.Text_IO.Close (Renderer.File);
   end End_Render;

   -------------------
   -- File_Renderer --
   -------------------

   function File_Renderer
     (Path : String)
      return Aquarius_Renderer
   is
   begin
      return R : Root_File_Renderer do
         Ada.Text_IO.Create (R.File, Ada.Text_IO.Out_File, Path);
      end return;
   end File_Renderer;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer    : in out Root_Text_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : Aquarius.Layout.Line_Number;
      Column      : Aquarius.Layout.Column_Number;
      Class       : String;
      Text        : String)
   is
      pragma Unreferenced (Terminal);
      pragma Unreferenced (Class);
      use Ada.Text_IO;
      use Aquarius.Layout;
   begin
      if Renderer.Line < Line then
         New_Line (Renderer.File,
                   Ada.Text_IO.Positive_Count
                     (Line - Renderer.Line));
         Renderer.Set_Current_Position (Line, 1);
      end if;

      Set_Col (Renderer.File, Ada.Text_IO.Positive_Count (Column));

      Put (Renderer.File, Text);

      Renderer.Set_Current_Position
        (Line, Column + Column_Offset (Text'Length));
   end Set_Text;

   -------------------
   -- Text_Renderer --
   -------------------

   function Text_Renderer return Aquarius_Renderer is
   begin
      return Root_Text_Renderer'
        (Root_Aquarius_Renderer with File => Ada.Text_IO.Standard_Output);
   end Text_Renderer;

end Aquarius.Rendering.Text;
