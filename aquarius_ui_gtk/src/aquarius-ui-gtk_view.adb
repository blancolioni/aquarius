with Ada.Strings.Fixed;

with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;

with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Drawing_Area;          use Gtk.Drawing_Area;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Layout;                use Gtk.Layout;
with Gtk.Main;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

package body Aquarius.UI.Gtk_View is

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Stop the Gtk main loop when the main window is destroyed.

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Gtk.Main.Main_Quit;
   end On_Destroy;

   procedure Add_Bubble
     (Canvas  : not null access Gtk_Layout_Record'Class;
      X, Y    : Gint;
      Width   : Gint;
      Height  : Gint;
      Title   : String;
      Content : String);
   --  Place a single bubble on the canvas at (X, Y). A bubble is a titled,
   --  framed box; its real content and behaviour are supplied from outside
   --  this crate. This is a minimal placeholder so the canvas is not empty.

   ----------------
   -- Add_Bubble --
   ----------------

   procedure Add_Bubble
     (Canvas  : not null access Gtk_Layout_Record'Class;
      X, Y    : Gint;
      Width   : Gint;
      Height  : Gint;
      Title   : String;
      Content : String)
   is
      Frame : Gtk_Frame;
      Body_Label : Gtk_Label;
   begin
      Gtk_New (Frame, Title);
      Frame.Set_Shadow_Type (Shadow_Out);
      Frame.Set_Size_Request (Width, Height);

      Gtk_New (Body_Label, Content);
      Body_Label.Set_Halign (Align_Start);
      Body_Label.Set_Valign (Align_Start);
      Body_Label.Set_Margin_Start (6);
      Body_Label.Set_Margin_Top (6);
      Frame.Add (Body_Label);

      Canvas.Put (Frame, X, Y);
   end Add_Bubble;

   ------------
   -- Launch --
   ------------

   procedure Launch (Icon_Dir : String := "") is
      Window        : Gtk_Window;
      Box           : Gtk_Box;
      Miniview      : Gtk_Drawing_Area;
      Bubble_Area   : Gtk_Layout;
      Bubble_Scroll : Gtk_Scrolled_Window;

      procedure Set_Window_Icons (Dir : String);
      --  Load the "aquarius-<size>.png" icons from Dir into an icon list.

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

   begin
      Gtk.Main.Init;

      Gtk_New (Window);
      Window.Set_Title ("Aquarius");
      Window.Set_Default_Size (1200, 800);
      Window.On_Destroy (On_Destroy'Access);

      if Icon_Dir /= "" then
         Set_Window_Icons (Icon_Dir);
      end if;

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Window.Add (Box);

      --  Miniview strip along the top.
      Gtk_New (Miniview);
      Miniview.Set_Size_Request (-1, 100);
      Box.Pack_Start (Miniview, Expand => False, Fill => True, Padding => 0);

      --  Bubble canvas: a large scrollable layout filling the remaining
      --  space, on which bubbles are freely placed.
      Gtk_New (Bubble_Scroll);
      Bubble_Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Gtk_New (Bubble_Area);
      Bubble_Area.Set_Size (2000, 2000);
      Bubble_Scroll.Add (Bubble_Area);
      Box.Pack_Start
        (Bubble_Scroll, Expand => True, Fill => True, Padding => 0);

      --  An initial bubble so the canvas is not empty. Real bubbles and
      --  their properties are managed from outside this crate.
      Add_Bubble
        (Canvas  => Bubble_Area,
         X       => 60,
         Y       => 40,
         Width   => 320,
         Height  => 180,
         Title   => "Welcome",
         Content => "Aquarius");

      Window.Show_All;
      Gtk.Main.Main;
   end Launch;

end Aquarius.UI.Gtk_View;
