with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;

with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Cairo;                     use Cairo;
with Cairo.Pattern;             use Cairo.Pattern;

with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Drawing_Area;          use Gtk.Drawing_Area;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Layout;                use Gtk.Layout;
with Gtk.Main;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Aquarius.Models;
with Aquarius.Models.Text;
with Aquarius.UI.Views;
with Aquarius.UI.Views.Registry;
with Aquarius.UI.Gtk_Views;
with Aquarius.UI.Gtk_Views.Register;

package body Aquarius.UI.Gtk_View is

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

   type Colour is record
      R, G, B : Gdouble;
   end record;

   --  Palette.
   Overview_Grey : constant Colour := (0.80, 0.80, 0.83);
   Blue_Top      : constant Colour := (0.86, 0.93, 0.98);
   Blue_Bottom   : constant Colour := (0.60, 0.77, 0.90);
   Bubble_Fill   : constant Colour := (0.98, 0.98, 1.00);
   Text_Colour   : constant Colour := (0.16, 0.16, 0.22);
   Black         : constant Colour := (0.0, 0.0, 0.0);

   type Bubble is record
      X, Y, W, H : Gdouble;
      Title      : Unbounded_String;
      Border     : Colour;
      Model      : Aquarius.Models.Model_Reference := null;
      View       : Aquarius.UI.Views.View_Reference := null;
   end record;

   type Bubble_Array is array (Positive range <>) of Bubble;

   --  Placeholder bubbles. Real bubbles and their properties will be managed
   --  from outside this crate; these exist so the canvas is not empty. Their
   --  models and views are filled in by Launch.
   Bubbles : Bubble_Array (1 .. 2) :=
     [(X => 60.0, Y => 40.0, W => 340.0, H => 190.0,
       Title  => To_Unbounded_String ("Welcome"),
       Border => (0.486, 0.227, 0.929),   --  purple
       others => <>),
      (X => 470.0, Y => 300.0, W => 300.0, H => 170.0,
       Title  => To_Unbounded_String ("Notes"),
       Border => (0.10, 0.62, 0.60),       --  teal
       others => <>)];

   procedure Set_Colour (Cr : Cairo_Context; C : Colour);
   --  Set Cr's source to the opaque colour C.

   --  Widgets kept at package level so the overview can query the canvas
   --  scroll position and both can be redrawn when the user scrolls.
   Overview      : Gtk_Drawing_Area;
   Bubble_Scroll : Gtk_Scrolled_Window;
   Bubble_Area   : Gtk_Layout;

   procedure Draw_Bubble (Cr : Cairo_Context; B : Bubble);
   --  Draw one bubble's chrome (body fill, coloured border, title). The bubble
   --  content is a real child widget placed on the canvas by Launch.

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Stop the Gtk main loop when the main window is destroyed.

   function Draw_Canvas
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean;
   --  Paint the bubble canvas: blue gradient background and bubble chrome.

   function Draw_Overview
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean;
   --  Paint the overview strip: grey background and a scaled-down,
   --  black-bordered rectangle per bubble, filled with the bubble's colour.

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

   procedure Draw_Bubble (Cr : Cairo_Context; B : Bubble) is
   begin
      Rectangle (Cr, B.X, B.Y, B.W, B.H);
      Set_Colour (Cr, Bubble_Fill);
      Fill_Preserve (Cr);
      Set_Source_Rgba (Cr, B.Border.R, B.Border.G, B.Border.B, Border_Alpha);
      Set_Line_Width (Cr, Border_Width);
      Stroke (Cr);

      Set_Colour (Cr, Text_Colour);
      Select_Font_Face
        (Cr, "sans-serif", Cairo_Font_Slant_Normal, Cairo_Font_Weight_Bold);
      Set_Font_Size (Cr, 20.0);
      Move_To (Cr, B.X + 20.0, B.Y + 30.0);
      Show_Text (Cr, To_String (B.Title));
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
      --  Cover the whole canvas, and any excess when the window is larger.
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
      Window      : Gtk_Window;
      Box         : Gtk_Box;

      procedure Set_Window_Icons (Dir : String);
      --  Load the "aquarius-<size>.png" icons from Dir into an icon list.

      procedure Populate_Bubbles;
      --  Give each bubble a model, resolve a view for it and place the view's
      --  widget on the canvas.

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
            Window.Set_Icon_List (Icons);
         end if;
      end Set_Window_Icons;

      ----------------------
      -- Populate_Bubbles --
      ----------------------

      procedure Populate_Bubbles is
         use type Aquarius.UI.Views.View_Reference;

         Contents : constant array (1 .. 2) of Unbounded_String :=
           [To_Unbounded_String ("Aquarius" & ASCII.LF
              & "Code-bubbles UI."),
            To_Unbounded_String ("Plain-text bubble." & ASCII.LF
              & "Backed by a text model." & ASCII.LF
              & "Displayed by a text view.")];
      begin
         for I in Bubbles'Range loop
            Bubbles (I).Model :=
              Aquarius.Models.Model_Reference
                (Aquarius.Models.Text.Create (To_String (Contents (I))));
            Bubbles (I).View :=
              Aquarius.UI.Views.Registry.Resolve (Bubbles (I).Model);

            if Bubbles (I).View /= null
              and then Bubbles (I).View.all
                         in Aquarius.UI.Gtk_Views.Gtk_View_Interface'Class
            then
               declare
                  Content_Widget : constant Gtk_Widget :=
                    Aquarius.UI.Gtk_Views.Gtk_View_Interface'Class
                      (Bubbles (I).View.all).Widget;
                  Cx : constant Gdouble := Bubbles (I).X + Content_Inset;
                  Cy : constant Gdouble := Bubbles (I).Y + Title_Height;
                  Cw : constant Gdouble :=
                    Bubbles (I).W - 2.0 * Content_Inset;
                  Ch : constant Gdouble :=
                    Bubbles (I).H - Title_Height - Content_Inset;
               begin
                  Content_Widget.Set_Size_Request (Gint (Cw), Gint (Ch));
                  Bubble_Area.Put (Content_Widget, Gint (Cx), Gint (Cy));
               end;
            end if;
         end loop;
      end Populate_Bubbles;

   begin
      Gtk.Main.Init;
      Aquarius.UI.Gtk_Views.Register.Register_All;

      Gtk_New (Window);
      Window.Set_Title ("Aquarius");
      Window.Set_Default_Size (1200, 800);
      Window.On_Destroy (On_Destroy'Access);

      if Icon_Dir /= "" then
         Set_Window_Icons (Icon_Dir);
      end if;

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Window.Add (Box);

      --  Overview strip along the top.
      Gtk_New (Overview);
      Overview.Set_Size_Request (-1, 100);
      Overview.On_Draw (Draw_Overview'Access);
      Box.Pack_Start (Overview, Expand => False, Fill => True, Padding => 0);

      --  Bubble canvas: a large scrollable layout filling the remaining space,
      --  on which bubble chrome is drawn and content widgets are placed.
      Gtk_New (Bubble_Scroll);
      Bubble_Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Gtk_New (Bubble_Area);
      Bubble_Area.Set_Size (Guint (Canvas_W), Guint (Canvas_H));
      Bubble_Area.On_Draw (Draw_Canvas'Access);
      Bubble_Scroll.Add (Bubble_Area);
      Box.Pack_Start
        (Bubble_Scroll, Expand => True, Fill => True, Padding => 0);

      Populate_Bubbles;

      --  Redraw the overview's viewport indicator when the canvas scrolls.
      Bubble_Scroll.Get_Hadjustment.On_Value_Changed (On_Scroll'Access);
      Bubble_Scroll.Get_Vadjustment.On_Value_Changed (On_Scroll'Access);

      Window.Show_All;
      Gtk.Main.Main;
   end Launch;

end Aquarius.UI.Gtk_View;
