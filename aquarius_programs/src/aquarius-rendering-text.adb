with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Aquarius.Rendering.Text is

   type File_Access is access Ada.Text_IO.File_Type;

   type Root_Text_Renderer is new Root_Aquarius_Renderer with
      record
         File : File_Access;
      end record;

   overriding procedure Set_Text
     (Renderer    : in out Root_Text_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : Aquarius.Locations.Line_Index;
      Column      : Aquarius.Locations.Column_Index;
      Class       : String;
      Text        : String);

   type Root_File_Renderer is new Root_Text_Renderer with
      record
         null;
      end record;

   overriding procedure End_Render (Renderer : in out Root_File_Renderer);

   type Root_Stream_Renderer is new Root_Aquarius_Renderer with
      record
         Stream      : Aquarius.Streams.Writer_Reference;
      end record;

   overriding procedure Set_Text
     (Renderer    : in out Root_Stream_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : Aquarius.Locations.Line_Index;
      Column      : Aquarius.Locations.Column_Index;
      Class       : String;
      Text        : String);

   ----------------
   -- End_Render --
   ----------------

   overriding procedure End_Render (Renderer : in out Root_File_Renderer) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ada.Text_IO.File_Type, File_Access);
   begin
      if Renderer.File /= null then
         Ada.Text_IO.Close (Renderer.File.all);
         Free (Renderer.File);
      end if;
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
         R.File := new Ada.Text_IO.File_Type;
         Ada.Text_IO.Create (R.File.all, Ada.Text_IO.Out_File, Path);
      end return;
   end File_Renderer;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer    : in out Root_Text_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : Aquarius.Locations.Line_Index;
      Column      : Aquarius.Locations.Column_Index;
      Class       : String;
      Text        : String)
   is
      pragma Unreferenced (Terminal);
      pragma Unreferenced (Class);
      use Ada.Text_IO;
      use Aquarius.Locations;
   begin
      if Renderer.Line < Line then
         New_Line ((if Renderer.File = null
                   then Ada.Text_IO.Standard_Output
                   else Renderer.File.all),
                   Ada.Text_IO.Positive_Count
                     (Line - Renderer.Line));
         Renderer.Line := Line;
         Renderer.Column := 1;
      end if;

      Set_Col ((if Renderer.File = null
               then Ada.Text_IO.Standard_Output
               else Renderer.File.all),
               Ada.Text_IO.Positive_Count (Column));

      Put ((if Renderer.File = null
           then Ada.Text_IO.Standard_Output
           else Renderer.File.all),
           Text);

      Renderer.Line := Line;
      Renderer.Column := Column + Column_Count (Text'Length);
   end Set_Text;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer    : in out Root_Stream_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : Aquarius.Locations.Line_Index;
      Column      : Aquarius.Locations.Column_Index;
      Class       : String;
      Text        : String)
   is
      pragma Unreferenced (Terminal);
      pragma Unreferenced (Class);
      use Aquarius.Locations;
   begin
      while Renderer.Line < Line loop
         Renderer.Stream.New_Line;
         Renderer.Line := Renderer.Line + 1;
         Renderer.Column := 1;
      end loop;

      if Renderer.Column < Column then
         declare
            Space_Count : constant Natural :=
                            Natural (Column)
                            - Natural (Renderer.Column);
            Spaces      : constant String (1 .. Space_Count) :=
                            [others => ' '];
         begin
            Renderer.Stream.Put (Spaces);
            Renderer.Column := Column;
         end;
      end if;

      Renderer.Stream.Put (Text);
      Renderer.Column := Renderer.Column
        + Column_Count (Text'Length);

   end Set_Text;

   ---------------------
   -- Stream_Renderer --
   ---------------------

   function Stream_Renderer
     (Stream : Aquarius.Streams.Writer_Reference)
      return Aquarius_Renderer
   is
   begin
      return Root_Stream_Renderer'(Stream => Stream, others => <>);
   end Stream_Renderer;

   -------------------
   -- Text_Renderer --
   -------------------

   function Text_Renderer return Aquarius_Renderer is
   begin
      return Root_Text_Renderer'
        (Root_Aquarius_Renderer with File => null);
   end Text_Renderer;

end Aquarius.Rendering.Text;
