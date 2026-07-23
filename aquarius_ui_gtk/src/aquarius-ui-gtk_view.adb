with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;

with Gdk;
with Gdk.Cursor;                use Gdk.Cursor;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Window;                use Gdk.Window;

with Cairo;                     use Cairo;
with Cairo.Pattern;             use Cairo.Pattern;

with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Drawing_Area;          use Gtk.Drawing_Area;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.File_Chooser;          use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;   use Gtk.File_Chooser_Dialog;
with Gtk.Layout;                use Gtk.Layout;
with Gtk.Main;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Aquarius.Grammars;
with Aquarius.Grammars.Manager;
with Aquarius.Programs;
with Aquarius.Programs.Models;
with Aquarius.Reader;
with Aquarius.Sources;
with Aquarius.Sources.Files;
with Aquarius.Streams;
with Aquarius.Streams.Files;

with Aquarius.Models;
with Aquarius.Models.Text;
with Aquarius.UI.Layout;
with Aquarius.UI.Views;
with Aquarius.UI.Views.Registry;
with Aquarius.UI.Gtk_Views;      use Aquarius.UI.Gtk_Views;
with Aquarius.UI.Gtk_Views.Register;

package body Aquarius.UI.Gtk_View is

   package Models renames Aquarius.Models;
   package Tree_Models renames Aquarius.Programs.Models;

   use type Aquarius.Programs.Program_Tree;
   package Views renames Aquarius.UI.Views;
   package Text_Models renames Aquarius.Models.Text;

   --  Size of the (fixed) bubble canvas in pixels.
   Canvas_W : constant Gint := 2000;
   Canvas_H : constant Gint := 2000;

   --  Scale and margin used when projecting the canvas into the overview
   --  strip along the top of the window.
   Overview_Scale : constant Gdouble := 0.08;
   Overview_Pad   : constant Gdouble := 8.0;

   Border_Alpha  : constant Gdouble := 0.5;
   Border_Width  : constant Gdouble := 10.0;
   Title_Height  : constant Gdouble := 42.0;   --  chrome reserved for title
   Content_Inset : constant Gdouble := Border_Width + 6.0;

   Default_Bubble_W : constant Gdouble := 340.0;
   Default_Bubble_H : constant Gdouble := 190.0;

   Bubble_Gap : constant := 12.0;   --  min space kept between bubbles

   --  Border grab band for resizing, and minimum bubble size. The band matches
   --  the chrome outside the content widget, so it never overlaps the content.
   Resize_Margin  : constant Gdouble := Content_Inset;
   Min_Bubble_W   : constant Gdouble := 160.0;
   Min_Bubble_H   : constant Gdouble := 120.0;

   type Colour is record
      R, G, B : Gdouble;
   end record;

   --  Palette.
   Overview_Grey : constant Colour := (0.80, 0.80, 0.83);
   Blue_Top      : constant Colour := (0.86, 0.93, 0.98);
   Blue_Bottom   : constant Colour := (0.60, 0.77, 0.90);
   Bubble_Fill   : constant Colour := (0.98, 0.98, 1.00);
   Title_Fill    : constant Colour := (0.90, 0.91, 0.96);  --  title band
   Text_Colour   : constant Colour := (0.16, 0.16, 0.22);
   Black         : constant Colour := (0.0, 0.0, 0.0);

   Close_Size   : constant Gdouble := 16.0;   --  close-button glyph size
   Close_Margin : constant Gdouble := 13.0;   --  gap from the right edge

   --  Border colours cycled through as bubbles are created.
   Border_Palette : constant array (0 .. 4) of Colour :=
     [(0.486, 0.227, 0.929),   --  purple
      (0.10, 0.62, 0.60),      --  teal
      (0.90, 0.49, 0.13),      --  orange
      (0.30, 0.65, 0.30),      --  green
      (0.85, 0.25, 0.45)];     --  pink

   type Bubble is record
      X, Y, W, H : Gdouble;
      Title      : Unbounded_String;
      Border     : Colour;
      Model      : Models.Model_Reference := null;
      View       : Views.View_Reference := null;
   end record;

   package Bubble_Vectors is new Ada.Containers.Vectors (Positive, Bubble);

   --  Runtime state, at package level so the request entry points and the
   --  redraw/scroll callbacks can all reach it.
   Bubbles          : Bubble_Vectors.Vector;
   New_Bubble_Count : Natural := 0;

   --  Overview interaction state: on press we defer; a click repositions on
   --  release, a drag repositions live. Grab_Off_X/Y keep the grabbed point
   --  fixed relative to the viewport rectangle while dragging.
   Overview_Pressed : Boolean := False;
   Overview_Dragged : Boolean := False;
   Grab_Off_X       : Gdouble := 0.0;
   Grab_Off_Y       : Gdouble := 0.0;

   --  Bubble drag state: index of the bubble being dragged by its title bar
   --  (0 = none) and the grab offset from the bubble's top-left.
   Dragging_Bubble : Natural := 0;
   Bubble_Grab_X   : Gdouble := 0.0;
   Bubble_Grab_Y   : Gdouble := 0.0;

   --  Bubble resize state: index of the bubble being resized by a border
   --  (0 = none), which edges are being dragged, and the anchored (fixed)
   --  edges at the start of the drag. The top edge is never resized (it is
   --  the title bar, used for moving), so the top is always the anchor.
   Resizing_Bubble : Natural := 0;
   Resize_L        : Boolean := False;
   Resize_R        : Boolean := False;
   Resize_B        : Boolean := False;
   Anchor_L        : Gdouble := 0.0;
   Anchor_T        : Gdouble := 0.0;
   Anchor_R        : Gdouble := 0.0;

   --  Cursors and hover state for the title-bar and border affordances.
   Move_Cursor      : Gdk.Gdk_Cursor;
   Close_Cursor     : Gdk.Gdk_Cursor;
   Default_Cursor   : Gdk.Gdk_Cursor;
   Resize_H_Cursor  : Gdk.Gdk_Cursor;   --  left / right edge
   Resize_V_Cursor  : Gdk.Gdk_Cursor;   --  bottom edge
   Resize_BR_Cursor : Gdk.Gdk_Cursor;   --  bottom-right corner
   Resize_BL_Cursor : Gdk.Gdk_Cursor;   --  bottom-left corner

   type Hover_Zone is
     (Zone_None, Zone_Title, Zone_Close,
      Zone_Resize_H, Zone_Resize_V, Zone_Resize_BR, Zone_Resize_BL);
   Current_Zone : Hover_Zone := Zone_None;

   Main_Window   : Gtk_Window;
   Overview      : Gtk_Drawing_Area;
   Bubble_Scroll : Gtk_Scrolled_Window;
   Bubble_Area   : Gtk_Layout;

   procedure Set_Colour (Cr : Cairo_Context; C : Colour);
   --  Set Cr's source to the opaque colour C.

   procedure Draw_Bubble (Cr : Cairo_Context; B : Bubble);
   --  Draw one bubble's chrome (body fill, coloured border, title).

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);

   function Draw_Canvas
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean;

   function Draw_Overview
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean;

   --  Request entry points. Open_Model is the core "give me a bubble for this
   --  model" operation; future producers (drag-and-drop, OS open) resolve to a
   --  model and call it. Open_File is one such producer.
   procedure Open_Model (Model : Models.Model_Reference; Title : String);
   procedure Open_File (Path : String);

   procedure Resolve_Overlaps (Seed : Positive);
   --  Move bubbles so none overlap, treating Seed as anchored (frozen
   --  wavefront), then reposition the affected content widgets.

   ----------------
   -- Set_Colour --
   ----------------

   procedure Set_Colour (Cr : Cairo_Context; C : Colour) is
   begin
      Set_Source_Rgb (Cr, C.R, C.G, C.B);
   end Set_Colour;

   -----------------
   -- Draw_Bubble --
   -----------------

   procedure Close_Box (B : Bubble; Bx, By, Bs : out Gdouble);
   --  Geometry of a bubble's close button (top-right of the title bar).

   procedure Close_Box (B : Bubble; Bx, By, Bs : out Gdouble) is
   begin
      Bs := Close_Size;
      Bx := B.X + B.W - Close_Size - Close_Margin;
      By := B.Y + (Title_Height - Close_Size) / 2.0;
   end Close_Box;

   procedure Draw_Bubble (Cr : Cairo_Context; B : Bubble) is
      Bx, By, Bs : Gdouble;
   begin
      --  Body.
      Rectangle (Cr, B.X, B.Y, B.W, B.H);
      Set_Colour (Cr, Bubble_Fill);
      Cairo.Fill (Cr);

      --  Title bar in a slightly different colour.
      Rectangle (Cr, B.X, B.Y, B.W, Title_Height);
      Set_Colour (Cr, Title_Fill);
      Cairo.Fill (Cr);

      --  Coloured border around the whole bubble.
      Rectangle (Cr, B.X, B.Y, B.W, B.H);
      Set_Source_Rgba (Cr, B.Border.R, B.Border.G, B.Border.B, Border_Alpha);
      Set_Line_Width (Cr, Border_Width);
      Stroke (Cr);

      --  Title text.
      Set_Colour (Cr, Text_Colour);
      Select_Font_Face
        (Cr, "sans-serif", Cairo_Font_Slant_Normal, Cairo_Font_Weight_Bold);
      Set_Font_Size (Cr, 20.0);
      Move_To (Cr, B.X + 20.0, B.Y + 30.0);
      Show_Text (Cr, To_String (B.Title));

      --  Close button (an X).
      Close_Box (B, Bx, By, Bs);
      Set_Source_Rgba (Cr, 0.35, 0.35, 0.40, 0.9);
      Set_Line_Width (Cr, 2.0);
      Move_To (Cr, Bx + 3.0, By + 3.0);
      Line_To (Cr, Bx + Bs - 3.0, By + Bs - 3.0);
      Move_To (Cr, Bx + Bs - 3.0, By + 3.0);
      Line_To (Cr, Bx + 3.0, By + Bs - 3.0);
      Stroke (Cr);
   end Draw_Bubble;

   -----------------
   -- Draw_Canvas --
   -----------------

   function Draw_Canvas
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean
   is
      --  The Gtk_Layout "draw" context is in fixed viewport coordinates, but
      --  child widgets live on the scrolled bin window. Translate by the
      --  scroll offset so the Cairo chrome scrolls with the content widgets.
      Hval : constant Gdouble := Get_Value (Bubble_Scroll.Get_Hadjustment);
      Vval : constant Gdouble := Get_Value (Bubble_Scroll.Get_Vadjustment);
      Aw   : constant Gdouble := Gdouble (Self.Get_Allocated_Width);
      Ah   : constant Gdouble := Gdouble (Self.Get_Allocated_Height);
      W    : constant Gdouble := Gdouble'Max (Gdouble (Canvas_W), Hval + Aw);
      H    : constant Gdouble := Gdouble'Max (Gdouble (Canvas_H), Vval + Ah);
      Grad : constant Cairo_Pattern := Create_Linear (0.0, 0.0, 0.0, H);
   begin
      Save (Cr);
      Translate (Cr, -Hval, -Vval);

      Add_Color_Stop_Rgb (Grad, 0.0, Blue_Top.R, Blue_Top.G, Blue_Top.B);
      Add_Color_Stop_Rgb
        (Grad, 1.0, Blue_Bottom.R, Blue_Bottom.G, Blue_Bottom.B);
      Set_Source (Cr, Grad);
      Rectangle (Cr, 0.0, 0.0, W, H);
      Cairo.Fill (Cr);
      Destroy (Grad);

      for B of Bubbles loop
         Draw_Bubble (Cr, B);
      end loop;

      Restore (Cr);
      --  Return False so Gtk_Layout's default draw runs after us and paints
      --  the child content widgets on top of the chrome.
      return False;
   end Draw_Canvas;

   -------------------
   -- Draw_Overview --
   -------------------

   function Draw_Overview
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean
   is
   begin
      Set_Colour (Cr, Overview_Grey);
      Rectangle
        (Cr, 0.0, 0.0,
         Gdouble (Self.Get_Allocated_Width),
         Gdouble (Self.Get_Allocated_Height));
      Cairo.Fill (Cr);

      for B of Bubbles loop
         Rectangle
           (Cr,
            Overview_Pad + B.X * Overview_Scale,
            Overview_Pad + B.Y * Overview_Scale,
            B.W * Overview_Scale,
            B.H * Overview_Scale);
         Set_Colour (Cr, B.Border);
         Fill_Preserve (Cr);
         Set_Colour (Cr, Black);
         Set_Line_Width (Cr, 1.0);
         Stroke (Cr);
      end loop;

      --  Shaded rectangle showing the region of the canvas currently visible
      --  in the scrolled bubble area.
      declare
         Hadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Hadjustment;
         Vadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Vadjustment;
      begin
         if Hadj /= null and then Vadj /= null then
            Rectangle
              (Cr,
               Overview_Pad + Get_Value (Hadj) * Overview_Scale,
               Overview_Pad + Get_Value (Vadj) * Overview_Scale,
               Get_Page_Size (Hadj) * Overview_Scale,
               Get_Page_Size (Vadj) * Overview_Scale);
            Set_Source_Rgba (Cr, 0.15, 0.18, 0.28, 0.18);
            Fill_Preserve (Cr);
            Set_Source_Rgba (Cr, 0.15, 0.18, 0.28, 0.70);
            Set_Line_Width (Cr, 1.5);
            Stroke (Cr);
         end if;
      end;
      return True;
   end Draw_Overview;

   ----------------------
   -- Resolve_Overlaps --
   ----------------------

   procedure Resolve_Overlaps (Seed : Positive) is
      use type Views.View_Reference;
      N     : constant Natural := Natural (Bubbles.Length);
      Rects : Aquarius.UI.Layout.Rectangle_Array (1 .. N);
   begin
      if N = 0 then
         return;
      end if;

      for I in 1 .. N loop
         Rects (I) :=
           (X => Long_Float (Bubbles (I).X),
            Y => Long_Float (Bubbles (I).Y),
            W => Long_Float (Bubbles (I).W),
            H => Long_Float (Bubbles (I).H));
      end loop;

      Aquarius.UI.Layout.Remove_Overlaps (Rects, Seed, Bubble_Gap);
      --  Keep everything at non-negative coordinates (GtkLayout can't place
      --  children at negative positions); grows the whole scene if needed.
      Aquarius.UI.Layout.Normalize (Rects, Bubble_Gap);

      declare
         Max_X, Max_Y : Gdouble := 0.0;
      begin
         for I in 1 .. N loop
            Bubbles (I).X := Gdouble (Rects (I).X);
            Bubbles (I).Y := Gdouble (Rects (I).Y);
            Max_X := Gdouble'Max (Max_X, Bubbles (I).X + Bubbles (I).W);
            Max_Y := Gdouble'Max (Max_Y, Bubbles (I).Y + Bubbles (I).H);
            if Bubbles (I).View /= null
              and then Bubbles (I).View.all in Gtk_View_Interface'Class
            then
               Bubble_Area.Move
                 (Gtk_View_Interface'Class (Bubbles (I).View.all).Widget,
                  Gint (Bubbles (I).X + Content_Inset),
                  Gint (Bubbles (I).Y + Title_Height));
            end if;
         end loop;

         --  Grow the scrollable area to fit (never shrink below the default).
         Bubble_Area.Set_Size
           (Guint (Gint'Max (Canvas_W, Gint (Max_X + Bubble_Gap))),
            Guint (Gint'Max (Canvas_H, Gint (Max_Y + Bubble_Gap))));
      end;

      Bubble_Area.Queue_Draw;
      Overview.Queue_Draw;
   end Resolve_Overlaps;

   ----------------
   -- Open_Model --
   ----------------

   procedure Open_Model (Model : Models.Model_Reference; Title : String) is
      package Layout renames Aquarius.UI.Layout;
      use type Views.View_Reference;

      B      : Bubble;
      Hval   : constant Gdouble := Get_Value (Bubble_Scroll.Get_Hadjustment);
      Vval   : constant Gdouble := Get_Value (Bubble_Scroll.Get_Vadjustment);
      Page_W : constant Gdouble :=
        Get_Page_Size (Bubble_Scroll.Get_Hadjustment);
      Page_H : constant Gdouble :=
        Get_Page_Size (Bubble_Scroll.Get_Vadjustment);
      Base_X : constant Gdouble := Hval + 40.0;
      Base_Y : constant Gdouble := Vval + 40.0;
      Step_X : constant Gdouble := Default_Bubble_W + Bubble_Gap;
      Step_Y : constant Gdouble := Default_Bubble_H + Bubble_Gap;

      function Free_At (X, Y : Gdouble) return Boolean is
         R : constant Layout.Rectangle :=
           (Long_Float (X), Long_Float (Y),
            Long_Float (Default_Bubble_W), Long_Float (Default_Bubble_H));
      begin
         for Existing of Bubbles loop
            if Layout.Overlaps
                 (R,
                  (Long_Float (Existing.X), Long_Float (Existing.Y),
                   Long_Float (Existing.W), Long_Float (Existing.H)),
                  Long_Float (Bubble_Gap))
            then
               return False;
            end if;
         end loop;
         return True;
      end Free_At;

   begin
      B.W := Default_Bubble_W;
      B.H := Default_Bubble_H;
      B.Title := To_Unbounded_String (Title);
      B.Border := Border_Palette (New_Bubble_Count mod Border_Palette'Length);
      B.Model := Model;
      B.View := Views.Registry.Resolve (Model);
      New_Bubble_Count := New_Bubble_Count + 1;

      --  Place in the first free grid slot that fits within the visible
      --  viewport, preferring downward before moving right, so a new bubble
      --  appears on-screen and does not overlap (and thus shove) existing
      --  ones. Falls back to the base position (which then shoves) if the
      --  visible area is full.
      B.X := Base_X;
      B.Y := Base_Y;
      Search :
      for Col in 0 .. 7 loop
         for Row in 0 .. 7 loop
            declare
               X : constant Gdouble := Base_X + Gdouble (Col) * Step_X;
               Y : constant Gdouble := Base_Y + Gdouble (Row) * Step_Y;
            begin
               if X + B.W <= Hval + Page_W
                 and then Y + B.H <= Vval + Page_H
                 and then Free_At (X, Y)
               then
                  B.X := X;
                  B.Y := Y;
                  exit Search;
               end if;
            end;
         end loop;
      end loop Search;

      if B.View /= null
        and then B.View.all in Gtk_View_Interface'Class
      then
         declare
            Content_Widget : constant Gtk_Widget :=
              Gtk_View_Interface'Class (B.View.all).Widget;
            Cx : constant Gdouble := B.X + Content_Inset;
            Cy : constant Gdouble := B.Y + Title_Height;
            Cw : constant Gdouble := B.W - 2.0 * Content_Inset;
            Ch : constant Gdouble := B.H - Title_Height - Content_Inset;
         begin
            Content_Widget.Set_Size_Request (Gint (Cw), Gint (Ch));
            Bubble_Area.Put (Content_Widget, Gint (Cx), Gint (Cy));
            --  The window is already shown for runtime requests, so show the
            --  freshly-added widget explicitly.
            Content_Widget.Show_All;
         end;
      end if;

      Bubbles.Append (B);
      --  Shove any bubbles the new one overlaps out of the way.
      Resolve_Overlaps (Positive (Bubbles.Last_Index));
   end Open_Model;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File (Path : String) is
      Name    : constant String := Ada.Directories.Simple_Name (Path);
      Content : Unbounded_String := Null_Unbounded_String;
      File    : Ada.Text_IO.File_Type;
   begin
      --  If a grammar matches this file, parse it into a program tree and show
      --  it in the (syntax-styled) source view. Any failure (no grammar, parse
      --  error) falls through to the plain-text view below.
      begin
         declare
            Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                        Aquarius.Grammars.Manager.Get_Grammar_For_File (Name);
            Source  : constant Aquarius.Sources.Source_Reference :=
                        Aquarius.Sources.Files.File_Source (Path);
            Stream  : constant Aquarius.Streams.Reader_Reference :=
                        Aquarius.Streams.Files.File_Reader (Path);
            Program : constant Aquarius.Programs.Program_Tree :=
                        Aquarius.Reader.Read (Grammar, Source, Stream);
         begin
            if Program /= null then
               Open_Model
                 (Models.Model_Reference
                    (Tree_Models.Create (Program, Grammar)),
                  Name);
               return;
            end if;
         end;
      exception
         when others =>
            null;  --  fall through to plain text
      end;

      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
         while not Ada.Text_IO.End_Of_File (File) loop
            Append (Content, Ada.Text_IO.Get_Line (File));
            if not Ada.Text_IO.End_Of_File (File) then
               Append (Content, ASCII.LF);
            end if;
         end loop;
         Ada.Text_IO.Close (File);
      exception
         when others =>
            if Ada.Text_IO.Is_Open (File) then
               Ada.Text_IO.Close (File);
            end if;
      end;

      Open_Model
        (Models.Model_Reference
           (Text_Models.Create (To_String (Content))),
         Name);
   end Open_File;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll (Self : access Gtk_Adjustment_Record'Class);

   procedure On_Scroll (Self : access Gtk_Adjustment_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Overview /= null then
         Overview.Queue_Draw;
      end if;
      if Bubble_Area /= null then
         Bubble_Area.Queue_Draw;   --  chrome scrolls with content
      end if;
   end On_Scroll;

   ---------------
   -- Scroll_To --
   ---------------

   procedure Scroll_To (Ox, Oy, Off_X, Off_Y : Gdouble);
   --  Scroll so the canvas point under the overview point (Ox, Oy) sits at
   --  Off_X/Off_Y from the viewport's top-left (kept within the content).
   --  Off = page/2 centres; Off = grab offset preserves the grab position.

   procedure Scroll_To (Ox, Oy, Off_X, Off_Y : Gdouble) is
      Cx   : constant Gdouble := (Ox - Overview_Pad) / Overview_Scale;
      Cy   : constant Gdouble := (Oy - Overview_Pad) / Overview_Scale;
      Hadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Hadjustment;
      Vadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Vadjustment;

      function Clamp (Adj : Gtk_Adjustment; Target : Gdouble) return Gdouble is
         Hi : constant Gdouble :=
           Gdouble'Max
             (Get_Lower (Adj), Get_Upper (Adj) - Get_Page_Size (Adj));
      begin
         return Gdouble'Min (Gdouble'Max (Target, Get_Lower (Adj)), Hi);
      end Clamp;

   begin
      Hadj.Set_Value (Clamp (Hadj, Cx - Off_X));
      Vadj.Set_Value (Clamp (Vadj, Cy - Off_Y));
   end Scroll_To;

   -------------------------
   -- On_Overview_Click --
   -------------------------

   function On_Overview_Click
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;

   function On_Overview_Click
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Self);
      Cx   : constant Gdouble := (Event.X - Overview_Pad) / Overview_Scale;
      Cy   : constant Gdouble := (Event.Y - Overview_Pad) / Overview_Scale;
      Hadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Hadjustment;
      Vadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Vadjustment;
      Hval : constant Gdouble := Get_Value (Hadj);
      Vval : constant Gdouble := Get_Value (Vadj);
      Pw   : constant Gdouble := Get_Page_Size (Hadj);
      Ph   : constant Gdouble := Get_Page_Size (Vadj);
   begin
      --  Defer: don't move on press. A drag moves live; a plain click moves
      --  on release (see On_Overview_Motion / On_Overview_Release).
      Overview_Pressed := True;
      Overview_Dragged := False;
      --  If the press is inside the viewport rectangle, keep the grabbed
      --  point fixed relative to it while dragging; otherwise grab its centre.
      if Cx >= Hval and then Cx <= Hval + Pw
        and then Cy >= Vval and then Cy <= Vval + Ph
      then
         Grab_Off_X := Cx - Hval;
         Grab_Off_Y := Cy - Vval;
      else
         Grab_Off_X := Pw / 2.0;
         Grab_Off_Y := Ph / 2.0;
      end if;
      return True;
   end On_Overview_Click;

   --------------------------
   -- On_Overview_Motion --
   --------------------------

   function On_Overview_Motion
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;

   function On_Overview_Motion
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      --  Only delivered while button 1 is held (Button1_Motion_Mask): a drag.
      --  Keep the grabbed point fixed relative to the viewport rectangle.
      if Overview_Pressed then
         Overview_Dragged := True;
         Scroll_To (Event.X, Event.Y, Grab_Off_X, Grab_Off_Y);
      end if;
      return True;
   end On_Overview_Motion;

   ---------------------------
   -- On_Overview_Release --
   ---------------------------

   function On_Overview_Release
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;

   function On_Overview_Release
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Self);
      Hadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Hadjustment;
      Vadj : constant Gtk_Adjustment := Bubble_Scroll.Get_Vadjustment;
   begin
      --  A plain click (no drag) centres the viewport on the clicked point.
      if Overview_Pressed and then not Overview_Dragged then
         Scroll_To
           (Event.X, Event.Y,
            Get_Page_Size (Hadj) / 2.0, Get_Page_Size (Vadj) / 2.0);
      end if;
      Overview_Pressed := False;
      return True;
   end On_Overview_Release;

   ---------------
   -- To_Canvas --
   ---------------

   procedure To_Canvas (Root_X, Root_Y : Gdouble; Cx, Cy : out Gdouble);
   --  Map a root (screen) point to canvas coordinates, accounting for the
   --  layout's on-screen position and the scroll offset.

   procedure To_Canvas (Root_X, Root_Y : Gdouble; Cx, Cy : out Gdouble) is
      Ox, Oy : Gint;
   begin
      Get_Origin (Bubble_Area.Get_Window, Ox, Oy);
      Cx := Root_X - Gdouble (Ox)
        + Get_Value (Bubble_Scroll.Get_Hadjustment);
      Cy := Root_Y - Gdouble (Oy)
        + Get_Value (Bubble_Scroll.Get_Vadjustment);
   end To_Canvas;

   ---------------------
   -- Move_Bubble_View --
   ---------------------

   procedure Move_Bubble_View (Index : Positive);
   --  Reposition bubble Index's content widget to match its geometry.

   procedure Move_Bubble_View (Index : Positive) is
      use type Views.View_Reference;
   begin
      if Bubbles (Index).View /= null
        and then Bubbles (Index).View.all in Gtk_View_Interface'Class
      then
         Bubble_Area.Move
           (Gtk_View_Interface'Class (Bubbles (Index).View.all).Widget,
            Gint (Bubbles (Index).X + Content_Inset),
            Gint (Bubbles (Index).Y + Title_Height));
      end if;
   end Move_Bubble_View;

   ----------------------
   -- Size_Bubble_View --
   ----------------------

   procedure Size_Bubble_View (Index : Positive);
   --  Resize AND reposition bubble Index's content widget to match its
   --  geometry (used while resizing; Move_Bubble_View only repositions).

   procedure Size_Bubble_View (Index : Positive) is
      use type Views.View_Reference;
      B : Bubble renames Bubbles (Index);
   begin
      if B.View /= null
        and then B.View.all in Gtk_View_Interface'Class
      then
         declare
            W  : constant Gtk_Widget :=
                   Gtk_View_Interface'Class (B.View.all).Widget;
            Cw : constant Gdouble := B.W - 2.0 * Content_Inset;
            Ch : constant Gdouble := B.H - Title_Height - Content_Inset;
         begin
            W.Set_Size_Request
              (Gint (Gdouble'Max (1.0, Cw)), Gint (Gdouble'Max (1.0, Ch)));
            Bubble_Area.Move
              (W,
               Gint (B.X + Content_Inset),
               Gint (B.Y + Title_Height));
         end;
      end if;
   end Size_Bubble_View;

   ---------------
   -- Edge_Hits --
   ---------------

   procedure Edge_Hits
     (B          : Bubble;
      Cx, Cy     : Gdouble;
      L, R, Bttm : out Boolean);
   --  Which resizable borders of B the point (Cx, Cy) is on. The left/right
   --  bands are only live below the title bar; the bottom band spans the full
   --  width (its ends are the corners).

   procedure Edge_Hits
     (B          : Bubble;
      Cx, Cy     : Gdouble;
      L, R, Bttm : out Boolean)
   is
      In_X        : constant Boolean := Cx >= B.X and then Cx <= B.X + B.W;
      In_Y        : constant Boolean := Cy >= B.Y and then Cy <= B.Y + B.H;
      Below_Title : constant Boolean := Cy >= B.Y + Title_Height;
   begin
      R := In_X and then In_Y and then Below_Title
        and then Cx >= B.X + B.W - Resize_Margin;
      L := In_X and then In_Y and then Below_Title
        and then Cx <= B.X + Resize_Margin;
      Bttm := In_X and then In_Y and then Below_Title
        and then Cy >= B.Y + B.H - Resize_Margin;
   end Edge_Hits;

   ------------------
   -- Close_Bubble --
   ------------------

   procedure Close_Bubble (Index : Positive);

   procedure Close_Bubble (Index : Positive) is
      use type Views.View_Reference;
      V : constant Views.View_Reference := Bubbles (Index).View;
   begin
      if V /= null and then V.all in Gtk_View_Interface'Class then
         --  Destroying the content widget removes it from the layout.
         Gtk_View_Interface'Class (V.all).Widget.Destroy;
      end if;
      Bubbles.Delete (Index);
      Bubble_Area.Queue_Draw;
      Overview.Queue_Draw;
   end Close_Bubble;

   -----------------------
   -- On_Canvas_Press --
   -----------------------

   function On_Canvas_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;

   function On_Canvas_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Self);
      Cx, Cy : Gdouble;
   begin
      To_Canvas (Event.X_Root, Event.Y_Root, Cx, Cy);
      --  Topmost bubble first: its close button closes it, else its title
      --  bar starts a drag.
      for I in reverse Bubbles.First_Index .. Bubbles.Last_Index loop
         declare
            B           : constant Bubble := Bubbles (I);
            Bx, By, Bs  : Gdouble;
            EL, ER, EB  : Boolean;
         begin
            Close_Box (B, Bx, By, Bs);
            Edge_Hits (B, Cx, Cy, EL, ER, EB);
            if Cx >= Bx and then Cx <= Bx + Bs
              and then Cy >= By and then Cy <= By + Bs
            then
               Close_Bubble (I);
               return True;
            elsif EL or else ER or else EB then
               Resizing_Bubble := I;
               Resize_L := EL;
               Resize_R := ER;
               Resize_B := EB;
               Anchor_L := B.X;
               Anchor_T := B.Y;
               Anchor_R := B.X + B.W;
               Bubble_Area.Grab_Add;
               return True;
            elsif Cx >= B.X and then Cx <= B.X + B.W
              and then Cy >= B.Y and then Cy <= B.Y + Title_Height
            then
               Dragging_Bubble := I;
               Bubble_Grab_X := Cx - B.X;
               Bubble_Grab_Y := Cy - B.Y;
               Bubble_Area.Grab_Add;
               return True;
            end if;
         end;
      end loop;
      return False;
   end On_Canvas_Press;

   ------------------------
   -- On_Canvas_Motion --
   ------------------------

   function On_Canvas_Motion
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;

   function On_Canvas_Motion
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      pragma Unreferenced (Self);
      Cx, Cy : Gdouble;
   begin
      To_Canvas (Event.X_Root, Event.Y_Root, Cx, Cy);

      if Resizing_Bubble /= 0 then
         declare
            New_L : Gdouble := Anchor_L;
            New_R : Gdouble := Anchor_R;
         begin
            if Resize_R then
               New_R := Gdouble'Max (Cx, Anchor_L + Min_Bubble_W);
            end if;
            if Resize_L then
               New_L := Gdouble'Min
                 (Gdouble'Max (0.0, Cx), Anchor_R - Min_Bubble_W);
            end if;
            Bubbles (Resizing_Bubble).X := New_L;
            Bubbles (Resizing_Bubble).W := New_R - New_L;
            if Resize_B then
               Bubbles (Resizing_Bubble).H :=
                 Gdouble'Max (Cy - Anchor_T, Min_Bubble_H);
            end if;
         end;
         Size_Bubble_View (Resizing_Bubble);
         Bubble_Area.Queue_Draw;
         Overview.Queue_Draw;
         return True;
      end if;

      if Dragging_Bubble /= 0 then
         Bubbles (Dragging_Bubble).X := Gdouble'Max (0.0, Cx - Bubble_Grab_X);
         Bubbles (Dragging_Bubble).Y := Gdouble'Max (0.0, Cy - Bubble_Grab_Y);
         Move_Bubble_View (Dragging_Bubble);
         Bubble_Area.Queue_Draw;
         Overview.Queue_Draw;
         return True;
      end if;

      --  Not dragging: cursor reflects the affordance under the pointer
      --  (close X, title move, or a resize border).
      declare
         Zone : Hover_Zone := Zone_None;
      begin
         for I in reverse Bubbles.First_Index .. Bubbles.Last_Index loop
            declare
               B          : constant Bubble := Bubbles (I);
               Bx, By, Bs : Gdouble;
               EL, ER, EB : Boolean;
            begin
               Close_Box (B, Bx, By, Bs);
               Edge_Hits (B, Cx, Cy, EL, ER, EB);
               if Cx >= Bx and then Cx <= Bx + Bs
                 and then Cy >= By and then Cy <= By + Bs
               then
                  Zone := Zone_Close;
                  exit;
               elsif ER and then EB then
                  Zone := Zone_Resize_BR;
                  exit;
               elsif EL and then EB then
                  Zone := Zone_Resize_BL;
                  exit;
               elsif EB then
                  Zone := Zone_Resize_V;
                  exit;
               elsif EL or else ER then
                  Zone := Zone_Resize_H;
                  exit;
               elsif Cx >= B.X and then Cx <= B.X + B.W
                 and then Cy >= B.Y and then Cy <= B.Y + Title_Height
               then
                  Zone := Zone_Title;
                  exit;
               end if;
            end;
         end loop;

         if Zone /= Current_Zone then
            Current_Zone := Zone;
            Set_Cursor
              (Bubble_Area.Get_Window,
               (case Zone is
                   when Zone_Close     => Close_Cursor,
                   when Zone_Title     => Move_Cursor,
                   when Zone_Resize_H  => Resize_H_Cursor,
                   when Zone_Resize_V  => Resize_V_Cursor,
                   when Zone_Resize_BR => Resize_BR_Cursor,
                   when Zone_Resize_BL => Resize_BL_Cursor,
                   when Zone_None      => Default_Cursor));
         end if;
      end;
      return False;
   end On_Canvas_Motion;

   -------------------------
   -- On_Canvas_Release --
   -------------------------

   function On_Canvas_Release
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;

   function On_Canvas_Release
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Self, Event);
      Dragged : constant Natural := Dragging_Bubble;
      Resized : constant Natural := Resizing_Bubble;
   begin
      if Resized /= 0 then
         Resizing_Bubble := 0;
         Bubble_Area.Grab_Remove;
         --  Shove other bubbles out of the way of the resized one.
         Resolve_Overlaps (Resized);
         return True;
      end if;

      if Dragged = 0 then
         return False;
      end if;
      Dragging_Bubble := 0;
      Bubble_Area.Grab_Remove;
      --  Shove other bubbles out of the way of the one just dropped.
      Resolve_Overlaps (Dragged);
      return True;
   end On_Canvas_Release;

   ------------------------
   -- Choose_And_Open_File --
   ------------------------

   procedure Choose_And_Open_File;

   procedure Choose_And_Open_File is
      Dialog : Gtk_File_Chooser_Dialog;
      Dummy  : Gtk_Widget;
      pragma Unreferenced (Dummy);
   begin
      Gtk_New (Dialog, "Open File", Main_Window, Action_Open);
      Dummy := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Dummy := Dialog.Add_Button ("Open", Gtk_Response_Accept);
      if Dialog.Run = Gtk_Response_Accept then
         Open_File (Dialog.Get_Filename);
      end if;
      Dialog.Destroy;
   end Choose_And_Open_File;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Key) return Boolean;

   function On_Key_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Key) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      if (Event.State and Control_Mask) /= 0 then
         if Event.Keyval = GDK_LC_n then
            --  Ctrl+N: new empty text note.
            Open_Model
              (Models.Model_Reference (Text_Models.Create ("")), "Note");
            return True;
         elsif Event.Keyval = GDK_LC_o then
            --  Ctrl+O: open a file as a plain-text bubble.
            Choose_And_Open_File;
            return True;
         end if;
      end if;
      return False;
   end On_Key_Press;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Gtk.Main.Main_Quit;
   end On_Destroy;

   ------------
   -- Launch --
   ------------

   procedure Launch (Icon_Dir : String := "") is
      Box : Gtk_Box;

      procedure Set_Window_Icons (Dir : String);

      ----------------------
      -- Set_Window_Icons --
      ----------------------

      procedure Set_Window_Icons (Dir : String) is
         use type Object_Simple_List.Glist;
         Sizes : constant array (Positive range <>) of Positive :=
           [16, 24, 32, 48, 256];
         Icons  : Object_Simple_List.Glist := Object_Simple_List.Null_List;
         Pixbuf : Gdk_Pixbuf;
         Error  : GError;
      begin
         for S of Sizes loop
            declare
               Path : constant String :=
                 Dir & "/aquarius-"
                 & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)
                 & ".png";
            begin
               Gdk_New_From_File (Pixbuf, Path, Error);
               if Pixbuf /= null then
                  Object_Simple_List.Append (Icons, GObject (Pixbuf));
               end if;
            end;
         end loop;

         if Icons /= Object_Simple_List.Null_List then
            Main_Window.Set_Icon_List (Icons);
         end if;
      end Set_Window_Icons;

   begin
      Gtk.Main.Init;
      Aquarius.UI.Gtk_Views.Register.Register_All;

      Gdk_New (Move_Cursor, Fleur);
      Gdk_New (Close_Cursor, Hand2);
      Gdk_New (Default_Cursor, Left_Ptr);
      Gdk_New (Resize_H_Cursor, Sb_H_Double_Arrow);
      Gdk_New (Resize_V_Cursor, Sb_V_Double_Arrow);
      Gdk_New (Resize_BR_Cursor, Bottom_Right_Corner);
      Gdk_New (Resize_BL_Cursor, Bottom_Left_Corner);

      Gtk_New (Main_Window);
      Main_Window.Set_Title ("Aquarius");
      Main_Window.Set_Default_Size (1200, 800);
      Main_Window.On_Destroy (On_Destroy'Access);
      Main_Window.On_Key_Press_Event (On_Key_Press'Access);

      if Icon_Dir /= "" then
         Set_Window_Icons (Icon_Dir);
      end if;

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Main_Window.Add (Box);

      --  Overview strip along the top.
      Gtk_New (Overview);
      Overview.Set_Size_Request (-1, 100);
      Overview.On_Draw (Draw_Overview'Access);
      Overview.Add_Events
        (Button_Press_Mask + Button_Release_Mask + Button1_Motion_Mask);
      Overview.On_Button_Press_Event (On_Overview_Click'Access);
      Overview.On_Button_Release_Event (On_Overview_Release'Access);
      Overview.On_Motion_Notify_Event (On_Overview_Motion'Access);
      Box.Pack_Start (Overview, Expand => False, Fill => True, Padding => 0);

      --  Bubble canvas: a large scrollable layout filling the remaining space.
      Gtk_New (Bubble_Scroll);
      Bubble_Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Gtk_New (Bubble_Area);
      Bubble_Area.Set_Size (Guint (Canvas_W), Guint (Canvas_H));
      Bubble_Area.On_Draw (Draw_Canvas'Access);
      Bubble_Area.Add_Events
        (Button_Press_Mask + Button_Release_Mask
         + Button1_Motion_Mask + Pointer_Motion_Mask);
      Bubble_Area.On_Button_Press_Event (On_Canvas_Press'Access);
      Bubble_Area.On_Button_Release_Event (On_Canvas_Release'Access);
      Bubble_Area.On_Motion_Notify_Event (On_Canvas_Motion'Access);
      Bubble_Scroll.Add (Bubble_Area);
      Box.Pack_Start
        (Bubble_Scroll, Expand => True, Fill => True, Padding => 0);

      --  Redraw overview + chrome when the canvas scrolls.
      Bubble_Scroll.Get_Hadjustment.On_Value_Changed (On_Scroll'Access);
      Bubble_Scroll.Get_Vadjustment.On_Value_Changed (On_Scroll'Access);

      --  Seed one welcome bubble. Ctrl+N adds a note, Ctrl+O opens a file.
      Open_Model
        (Models.Model_Reference
           (Text_Models.Create
              ("Welcome to Aquarius." & ASCII.LF & ASCII.LF
               & "Ctrl+N: new note" & ASCII.LF
               & "Ctrl+O: open a file")),
         "Welcome");

      Main_Window.Show_All;
      Gtk.Main.Main;
   end Launch;

end Aquarius.UI.Gtk_View;
