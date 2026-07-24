with Ada.Characters.Latin_1;

with Glib;                use Glib;
with Glib.Properties;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Pango.Enums;

package body Aquarius.UI.Gtk_Views.Tree_Render is

   use Aquarius.Locations;

   package Styles renames Aquarius.Rendering.Styles;

   LF : Character renames Ada.Characters.Latin_1.LF;

   function Hex (C : Styles.Colour) return String;
   function Get_Or_Create_Tag
     (Renderer : in out Buffer_Renderer;
      Class    : String;
      Terminal : Aquarius.Programs.Program_Tree)
      return Gtk.Text_Tag.Gtk_Text_Tag;

   ---------
   -- Hex --
   ---------

   function Hex (C : Styles.Colour) return String is
      Digits_Set : constant String := "0123456789abcdef";

      function Byte (X : Styles.Colour_Component) return String is
         V : constant Natural := Natural (Float (X) * 255.0);
      begin
         return Digits_Set (Digits_Set'First + V / 16)
              & Digits_Set (Digits_Set'First + V mod 16);
      end Byte;
   begin
      return "#" & Byte (C.Red) & Byte (C.Green) & Byte (C.Blue);
   end Hex;

   -----------------------
   -- Get_Or_Create_Tag --
   -----------------------

   function Get_Or_Create_Tag
     (Renderer : in out Buffer_Renderer;
      Class    : String;
      Terminal : Aquarius.Programs.Program_Tree)
      return Gtk.Text_Tag.Gtk_Text_Tag
   is
      Tag : Gtk.Text_Tag.Gtk_Text_Tag;
   begin
      if Renderer.Tags.Contains (Class) then
         return Renderer.Tags.Element (Class);
      end if;

      declare
         Style : constant Styles.Text_Style :=
                   Renderer.Styler.Style_For (Class, Terminal);
      begin
         Tag := Renderer.Buffer.Create_Tag ("");

         Glib.Properties.Set_Property
           (Tag, Gtk.Text_Tag.Foreground_Property, Hex (Style.Foreground));

         if Style.Has_Background then
            Glib.Properties.Set_Property
              (Tag, Gtk.Text_Tag.Background_Property, Hex (Style.Background));
         end if;

         if Style.Bold then
            Pango.Enums.Weight_Properties.Set_Property
              (Tag,
               Pango.Enums.Weight_Properties.Property
                 (Gtk.Text_Tag.Weight_Property),
               Pango.Enums.Pango_Weight_Bold);
         end if;

         if Style.Italic then
            Pango.Enums.Style_Properties.Set_Property
              (Tag,
               Pango.Enums.Style_Properties.Property
                 (Gtk.Text_Tag.Style_Property),
               Pango.Enums.Pango_Style_Italic);
         end if;

         if Style.Underline then
            Pango.Enums.Underline_Properties.Set_Property
              (Tag,
               Pango.Enums.Underline_Properties.Property
                 (Gtk.Text_Tag.Underline_Property),
               Pango.Enums.Pango_Underline_Single);
         end if;
      end;

      Renderer.Tags.Insert (Class, Tag);
      return Tag;
   end Get_Or_Create_Tag;

   ------------
   -- Create --
   ------------

   function Create
     (Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Styler : Aquarius.Rendering.Styles.Styler_Reference)
      return Buffer_Renderer
   is
   begin
      return Buffer_Renderer'
        (Aquarius.Rendering.Root_Aquarius_Renderer with
           Buffer   => Gtk.Text_Buffer.Gtk_Text_Buffer (Buffer),
           Styler   => Styler,
           Tags     => Tag_Maps.Empty_Map,
           Cur_Line => 1,
           Cur_Col  => 1);
   end Create;

   ------------------
   -- Begin_Render --
   ------------------

   overriding procedure Begin_Render (Renderer : in out Buffer_Renderer) is
   begin
      Renderer.Buffer.Set_Text ("");
      Renderer.Cur_Line := 1;
      Renderer.Cur_Col  := 1;
   end Begin_Render;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer : in out Buffer_Renderer;
      Terminal : Aquarius.Programs.Program_Tree;
      Line     : Aquarius.Locations.Line_Index;
      Column   : Aquarius.Locations.Column_Index;
      Class    : String;
      Text     : String)
   is
      Tag      : constant Gtk.Text_Tag.Gtk_Text_Tag :=
                   Get_Or_Create_Tag (Renderer, Class, Terminal);
      End_Iter : Gtk_Text_Iter;
   begin
      while Renderer.Cur_Line < Line loop
         Renderer.Buffer.Get_End_Iter (End_Iter);
         Renderer.Buffer.Insert (End_Iter, [1 => LF]);
         Renderer.Cur_Line := Renderer.Cur_Line + 1;
         Renderer.Cur_Col  := 1;
      end loop;

      if Renderer.Cur_Col < Column then
         declare
            Spaces : constant String
              (1 .. Natural (Column) - Natural (Renderer.Cur_Col)) :=
                [others => ' '];
         begin
            Renderer.Buffer.Get_End_Iter (End_Iter);
            Renderer.Buffer.Insert (End_Iter, Spaces);
            Renderer.Cur_Col := Column;
         end;
      end if;

      declare
         Start_Off  : constant Glib.Gint := Renderer.Buffer.Get_Char_Count;
         Start_Iter : Gtk_Text_Iter;
      begin
         Renderer.Buffer.Get_End_Iter (End_Iter);
         Renderer.Buffer.Insert (End_Iter, Text);
         Renderer.Buffer.Get_Iter_At_Offset (Start_Iter, Start_Off);
         Renderer.Buffer.Get_End_Iter (End_Iter);
         Renderer.Buffer.Apply_Tag (Tag, Start_Iter, End_Iter);
      end;

      Renderer.Cur_Col := Column + Column_Count (Text'Length);
   end Set_Text;

end Aquarius.UI.Gtk_Views.Tree_Render;
