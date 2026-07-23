private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;
private with Gtk.Text_Tag;

with Gtk.Text_Buffer;

with Aquarius.Locations;
with Aquarius.Programs;
with Aquarius.Rendering;
with Aquarius.Rendering.Styles;

--  A renderer (Aquarius.Rendering) that emits an arranged program tree into a
--  GtkTextBuffer, applying one GtkTextTag per render class. The tags are built
--  on demand from a toolkit-neutral Styler (Aquarius.Rendering.Styles): this
--  is the only place Text_Style is translated into GtkAda primitives.

package Aquarius.UI.Gtk_Views.Tree_Render is

   type Buffer_Renderer is
     new Aquarius.Rendering.Root_Aquarius_Renderer with private;

   function Create
     (Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Styler : Aquarius.Rendering.Styles.Styler_Reference)
      return Buffer_Renderer;

private

   package Tag_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Gtk.Text_Tag.Gtk_Text_Tag,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Gtk.Text_Tag."=");

   type Buffer_Renderer is new Aquarius.Rendering.Root_Aquarius_Renderer with
      record
         Buffer   : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Styler   : Aquarius.Rendering.Styles.Styler_Reference;
         Tags     : Tag_Maps.Map;
         Cur_Line : Aquarius.Locations.Line_Index   := 1;
         Cur_Col  : Aquarius.Locations.Column_Index := 1;
      end record;

   overriding procedure Begin_Render (Renderer : in out Buffer_Renderer);

   overriding procedure Set_Text
     (Renderer : in out Buffer_Renderer;
      Terminal : Aquarius.Programs.Program_Tree;
      Line     : Aquarius.Locations.Line_Index;
      Column   : Aquarius.Locations.Column_Index;
      Class    : String;
      Text     : String);

end Aquarius.UI.Gtk_Views.Tree_Render;
