with Glib;

with Gtk.Text_Tag_Table;
--  with Gtk.Widget;

with Pango.Enums;

package body Aquarius.GUI.Text is

   Local_Default_Font : Pango.Font.Pango_Font_Description;
   Default_Foreground : Gdk.Color.Gdk_Color;
   Default_Background : Gdk.Color.Gdk_Color;
--     Color_Map          : Gdk.Color.Gdk_Colormap;

   ------------------
   -- Default_Font --
   ------------------

   function Default_Font return Pango.Font.Pango_Font_Description is
   begin
      return Local_Default_Font;
   end Default_Font;

   -----------------------
   -- Default_Tag_Entry --
   -----------------------

   function Default_Tag_Entry (TB    : Gtk.Text_Buffer.Gtk_Text_Buffer)
                              return Gtk.Text_Tag.Gtk_Text_Tag
   is
      use type Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Table : constant Gtk.Text_Tag_Table.Gtk_Text_Tag_Table :=
        TB.Get_Tag_Table;
      Tag : Gtk.Text_Tag.Gtk_Text_Tag := Tag_Table.Lookup ("default");
   begin
      if Tag /= null then
         return Tag;
      end if;

      Tag := TB.Create_Tag ("default");

      declare
         use Aquarius.Fonts;
         use Pango.Font;
         use Gtk.Text_Tag;
         Desc : constant Pango_Font_Description := Copy (Default_Font);
      begin
         Set_Weight (Desc, Pango.Enums.Pango_Weight_Normal);
         Set_Style (Desc, Pango.Enums.Pango_Style_Normal);
         Set_Property (Tag, Font_Desc_Property, Desc);
         Gdk.Color.Set_Property
           (Tag, Foreground_Gdk_Property, Default_Foreground);
         Gdk.Color.Set_Property
           (Tag, Background_Gdk_Property, Default_Background);
      end;

      return Tag;

   end Default_Tag_Entry;

   --------------------
   -- Get_Gdk_Colour --
   --------------------

   function Get_Gdk_Colour (Colour   : Aquarius.Fonts.Aquarius_Colour)
                           return Gdk.Color.Gdk_Color
   is
      use Glib, Gdk.Color;
      Result  : Gdk.Color.Gdk_Color;
   begin
      Set_Rgb (Result,
               Guint16 (Aquarius.Fonts.Red (Colour)) * 256,
               Guint16 (Aquarius.Fonts.Green (Colour)) * 256,
               Guint16 (Aquarius.Fonts.Blue (Colour)) * 256);
      --  Alloc_Color (Colormap, Result, Success => Success);
      return Result;
   end Get_Gdk_Colour;

   -------------------
   -- Get_Tag_Entry --
   -------------------

   function Get_Tag_Entry (Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
                           Name   : String;
                           Style  : Aquarius.Styles.Aquarius_Style)
                          return Gtk.Text_Tag.Gtk_Text_Tag
   is
      use Gtk.Text_Tag, Gtk.Text_Tag_Table;
      Tag_Table : constant Gtk.Text_Tag_Table.Gtk_Text_Tag_Table :=
        Buffer.Get_Tag_Table;
      Result    : Gtk_Text_Tag;
   begin
      if Name = "" then
         return Default_Tag_Entry (Buffer);
      end if;

      Result := Tag_Table.Lookup (Name);
      if Result = null then
         Result := Buffer.Create_Tag (Name);
         declare
            use Aquarius.Fonts;
            use Pango.Font;
            Font : constant Aquarius.Fonts.Aquarius_Font :=
              Style.Font (Name);
            Desc : constant Pango_Font_Description :=
              Copy (Default_Font);
         begin
            if Is_Bold (Font) then
               Set_Weight (Desc, Pango.Enums.Pango_Weight_Bold);
            end if;
            if Is_Italic (Font) then
               Set_Style (Desc, Pango.Enums.Pango_Style_Italic);
            end if;
            if Is_Underlined (Font) then
               --  later
               null;
            end if;
            Set_Property (Result, Font_Desc_Property, Desc);
            if Has_Foreground (Font) then
               Gdk.Color.Set_Property
                 (Result, Foreground_Gdk_Property,
                  Get_Gdk_Colour (Get_Foreground (Font)));
            end if;
            if Has_Background (Font) then
               Gdk.Color.Set_Property
                 (Result, Background_Gdk_Property,
                  Get_Gdk_Colour (Get_Background (Font)));
            end if;
         end;
      end if;
      return Result;
   end Get_Tag_Entry;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
      use Gtk.Text_Tag_Table;
--        Success : Boolean;
   begin
      Local_Default_Font :=
        Pango.Font.To_Font_Description ("Courier new",
                                        Size => 10);
      --  Color_Map := Gtk.Widget.Get_Default_Colormap;
      Gdk.Color.Set_Rgb (Default_Background,
                         65535, 65535, 65535);
--        Gdk.Color.Alloc_Color (Color_Map, Default_Background,
--                               Success => Success);
      Gdk.Color.Set_Rgb (Default_Foreground, 0, 0, 0);
--        Gdk.Color.Alloc_Color (Color_Map, Default_Foreground,
--                               Success => Success);

   end Initialise;

end Aquarius.GUI.Text;
